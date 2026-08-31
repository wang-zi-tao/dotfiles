---
name: hermes-cli-from-other-agents
description: 当其他 agent（如 OpenCode, Cursor, Claude Code）需要调用 Hermes CLI 来管理系统配置、Kanban 看板或定时任务时的参考指南。涵盖 profiles 职能、调用模式和常见陷阱。
version: 1.0.0
platforms: [windows]
environments: [agent-collaboration]
metadata:
  hermes:
    tags: [hermes-cli, profile, kanban, cron, multi-agent]
    related_skills: [dotfiles, kanban-worker, kanban-orchestrator, dotfiles-install-targets]
---
# Hermes CLI from Other Agents

## 概述

本机安装有多个 **Hermes Agent profiles**。其他 AI agent（OpenCode、Cursor、Claude Code 等）可以通过 `hermes -p <profile> -z "指令"` 将特定任务委托给对应的 Hermes profile。

所有 Hermes 配置存储在 `%LOCALAPPDATA%\hermes\` 下（即 `C:\Users\wps\AppData\Local\hermes\`）。

---

## Profiles 与职责

| Profile | 模型 | 角色 | 职责 |
|---|---|---|---|
| `default` | deepseek-v4-flash | 通用助手 | 默认 profile, 通用任务 |
| `system_manager` | deepseek-v4-flash | **系统管理员** | 管理 dotfiles-windows 仓库、软件安装、系统服务、配置文件、系统优化 |
| `orchestrator` | deepseek/deepseek-v4-pro (wps_claude) | **工作流管理器** | Kanban orchestration：任务分解、路由、多 agent 协作 |

### system_manager — 系统管理员

适合处理以下任务：
- 安装/卸载软件（choco, scoop, pip, npm, cargo 等）
- 修改系统服务（sshd, keymap 等）
- 修改 dotfiles-windows 仓库中的配置文件
- 运行 `install.py` 应用配置
- 系统优化与调试

**调用方式：**
```bash
hermes -p system_manager -z "请帮我安装 ripgrep"
hermes -p system_manager -z "修改 ~/.gitconfig，添加 proxy 配置"
hermes -p system_manager -z "运行 install.py all"
```

### orchestrator — 工作流管理器

适合处理以下任务：
- 将大型任务分解为多个 Kanban 子任务
- 创建和管理 Kanban 看板
- 多 profile 协作工作流编排
- 创建定时器/定时任务
- 技能管理（skills）

**调用方式：**
```bash
hermes -p orchestrator -z "将'重构用户模块'拆分为 Kanban 任务"
hermes -p orchestrator -z "创建一个每天备份的 cron 任务"
```

---

## Kanban 命令（核心操作）

**重要原则：** 在 Kanban worker 会话内使用 `kanban_*` 工具函数；在非 worker 上下文中（其他 agent 调用）使用 `hermes kanban` CLI。

### 创建任务
```bash
hermes kanban create "任务标题" \
  --assignee system_manager \
  --body "详细描述"
```

### 创建带依赖的任务
```bash
hermes kanban create "子任务" \
  --assignee system_manager \
  --parent T_xxx
```

### 查看任务
```bash
hermes kanban show T_xxx          # 详细信息 + 评论 + 事件日志（默认已包含）
```

### 列示任务
```bash
hermes kanban list                        # 所有未完成任务
hermes kanban list --status ready         # 按状态筛选
hermes kanban list --assignee system_manager  # 按 profile 筛选
```

### 任务生命周期
```bash
hermes kanban claim T_xxx          # 认领任务（输出工作空间路径）
hermes kanban comment T_xxx "备注" # 添加评论
hermes kanban block T_xxx "原因"   # 阻塞
hermes kanban unblock T_xxx        # 解除阻塞
hermes kanban complete T_xxx       # 完成
hermes kanban archive T_xxx        # 归档
```

### 工作流管理
```bash
hermes kanban link T_xxx T_yyy    # 添加依赖（parent child 为位置参数）
hermes kanban promote T_xxx       # 手动推进到 ready（恢复路径）
hermes kanban reassign T_xxx system_manager --reclaim  # 转派
hermes kanban tail T_xxx          # 跟踪事件流
```

### 看板管理
```bash
hermes kanban boards              # 列出看板
hermes kanban stats                # 看板统计
# 创建 Swarm 任务组（--worker 可重复；--verifier/--synthesizer 必填 profile）
hermes kanban swarm --worker system_manager:任务1 --verifier orchestrator --synthesizer orchestrator "目标"
```

---

## 定时器 / Cron 命令

### 创建定时任务
```bash
# schedule 与 prompt 均为位置参数（--name/--deliver 等为选项）
hermes cron create \
  "0 9 * * *" \
  "每天 9 点向 Telegram 发送日报" \
  --name "日报" \
  --deliver telegram
```

### 管理定时任务
```bash
hermes cron list                  # 列出所有任务
hermes cron pause <job-id>        # 暂停
hermes cron resume <job-id>       # 恢复
hermes cron run <job-id>          # 手动触发一次
hermes cron remove <job-id>       # 删除
hermes cron status                # 检查调度器运行状态
```

---

## 通用 Hermes CLI 模式

### 从其他 agent 调用 Hermes（核心模式）

```bash
# 一般形式（顶层非交互参数为 -z/--oneshot；-q 属于 chat 子命令）
hermes -p <profile> -z "你要执行的指令"

# 如果需要使用特定 skill
hermes -p system_manager -z "..." --skills dotfiles

# 如果需要继续已有会话
hermes -p orchestrator --resume <session_id>  # 按会话 ID 或标题
hermes -p orchestrator --continue "名称"       # 按名称继续

# 查看 profile 信息
hermes profile list                        # 列出所有 profile
hermes profile show <name>                 # 查看特定 profile
```

### 带输出的非交互调用

`-z`（或 `--oneshot`）执行一次非交互对话，仅输出最终回复到 stdout（注意：`-q`/`--query` 是 `hermes chat` 子命令的参数，顶层等效参数为 `-z`）：

```bash
# OpenCode 中使用（bash）
result=$(hermes -p system_manager -z "查询已安装的 choco 包列表" 2>&1)
echo "$result"

# Nushell 中使用
let result = (hermes -p system_manager -z "查询已安装的 choco 包列表" | complete)
$result.stdout
```

### 安全注意事项

- 敏感操作（修改系统配置、安装软件）会触发 `system_manager` 的 ask-for-approval 规则
- 跨 profile 写操作（修改另一个 profile 的 skills/plugins）会被 cross-profile write guard 阻止
- 环境变量 `HERMES_ACCEPT_HOOKS=1` 可自动批准 hooks（用于自动化脚本）

---

## 常见场景示例

### OpenCode 调用 Hermes 安装软件
```bash
# OpenCode 中执行
hermes -p system_manager -z "使用 choco 安装 everything 和 powertoys"
```

### OpenCode 需要修改系统配置
```bash
# OpenCode 中通知 Hermes 管理 dotfiles
hermes -p system_manager -z "在 dotfiles-windows 仓库中添加 alacritty 的字体配置，然后运行 install.py 应用"
```

### 创建 Kanban 看板任务
```bash
# 从 Cursor 中创建
hermes -p orchestrator -z "
  创建 3 个 Kanban 任务：
  1. '研究 Postgres 迁移成本' → 分配给 system_manager
  2. '分析性能对比数据' → 分配给 system_manager
  3. '撰写迁移建议报告' → 等待 1 和 2 完成后执行 → 分配给 orchestrator
"
```

### 设置定时备份
```bash
hermes -p system_manager -z "
  创建一个每天凌晨 2 点的定时任务：
  - 名称: db-backup
  - 执行: 备份重要数据库
  - 使用 cron create
"
```

### 从 OpenCode 调用 orchestrator 管理看板
```bash
# OpenCode 中查看看板状态
hermes -p orchestrator -z "查看 Kanban 看板当前所有待处理任务"

# 推进阻塞任务
hermes -p orchestrator -z "推进 T_abc 到 ready 状态"
```

---

## 注意事项 / Pitfalls

1. **`hermes` 命令需要 PATH 中存在。** 如果使用 git-bash/MSYS，确保 `%LOCALAPPDATA%\hermes\Scripts` 在 PATH 中，或使用绝对路径。

2. **长时间运行的任务使用 `background` + `notify_on_complete`。** 调用 `hermes -p system_manager -z "install.py all"` 可能耗时很长，应在后台运行并等待完成。

3. **Kanban `delegate_task` 陷阱：** 在非 Kanban worker 会话中，`delegate_task(toolsets=["kanban"])` **不会**注入 `kanban_*` 工具，子 agent 会伪造结果。应改用 `hermes kanban` CLI。

4. **跨 profile 操作限制：** 默认不能修改其他 profile 的 skills/plugins/cron/memories。通过 `cross_profile=True` 覆盖前需要用户明确许可。

5. **`orchestrator` profile 使用 `wps_claude` provider，** 需要 `WPS_CLAUDE_API_KEY` 环境变量。如果该变量未设置，调用 `orchestrator` 会失败。

6. **PowerShell 中的路径分隔符问题：** 在 PowerShell 中 `/` 是除法运算符，传递路径时需要用引号包裹或使用 `\`。

7. **在 bash(git-bash) 中运行 `hermes` 时：** 使用单引号包裹 query 参数以避免 MSYS 路径转义：
   ```bash
   hermes -p system_manager -z '安装 git'
   ```
