---
name: dotfiles
description: 关于此操作系统的一切服务, 软件和配置文件的集中式管理, 配置脚本.
---
# 要求
## windows
- **IMPORTANT** 请让`system_manager`来修改dotfiles-windows仓库, 以及应用系统配置
```bash
hermes -p system_manager -q "your request"
```
- **MUST** windows下一切软件默认使用choco安装, 其次用scoop, 最后下载绿色版
- **MUST** 对windows系统服务的创建和配置都要转为powershell脚本, 写在dotfiles-windows中
- **MUST** 一切配置文件都放在dotfiles-windows中, 并通过install.yaml中的powershell脚本复制配置文件
- **MUST** 修改后用install.py应用配置

# 配置仓库

## dotfiles 仓库
主dotfiles仓库, 主要用于nixos linux系统的配置
- 位置: `DOTFILE`环境变量

### neovim配置
- 位置: `$DOTFILE/packages/wangzi-neovim/`

## dotfiles-windows 仓库
windows下的dotfiles仓库
- 位置: `$env:DOTFILE_WINDOWS`环境变量
- 配置使用方法请参考仓库下的AGENTS.md文件

### 配置脚本
- 位置: `$DOTFILE_WINDOWS/install.yaml`
