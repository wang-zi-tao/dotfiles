---
name: dotfiles
description: 关于此操作系统的一切服务, 软件和配置文件的集中式管理, 配置脚本. 包括AI agent的一切配置, skills, 插件. 修改AGENTS.md或者skills/dsh插件/或者neovim配置请读取此skill， 并同步到安装位置。
---
# 要求
## windows
- **MUST** windows下一切软件默认使用choco安装, 其次用scoop, 最后下载绿色版
- **MUST** 对windows系统服务的创建和配置都要转为powershell脚本, 写在dotfiles-windows中
- **MUST** 一切配置文件都放在dotfiles-windows中, 并通过install.yaml中的powershell脚本复制配置文件
- **MUST** 修改后用install.py应用配置
```nu
# 应用skills/全局AGETNS.md/dsh插件相关修改
^python $env.DOTFILE_WINDOWS/install.py install dsh-env 
# 应用neovim插件相关修改
^python $env.DOTFILE_WINDOWS/install.py install nvim-env 
```

# 配置仓库

## dotfiles 仓库
主dotfiles仓库, 主要用于nixos linux系统的配置, 以及多系统之间共享的文件
- 位置: `DOTFILE`环境变量

### neovim配置
- 位置: `$env.DOTFILE/packages/wangzi-neovim/`

### skills
- 位置: `$env.DOTFILE/skills/`

### deepseek-harness 插件
- 位置: `$env.DOTFILE/packages/`

### 全局AGENTS.md
- 位置: `$env.DOTFILE/packages/dsh-home/AGENTS.md`

### deepseek-harness配置
- windows位置: `$env.DOTFILE_WINDOWS/dsh`
- linux位置: `$env.DOTFILE/packages/dsh-home`

## dotfiles-windows 仓库
windows下的dotfiles仓库
- 位置: `$env.DOTFILE_WINDOWS`环境变量
- 配置使用方法请参考仓库下的AGENTS.md文件

### 配置脚本
- 位置: `$env.DOTFILE_WINDOWS/install.yaml`
