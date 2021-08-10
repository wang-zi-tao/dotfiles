export PATH=${HOME}/.cargo/bin:$PATH

#alias glog = "git --no-pager log --all --color=always --graph --abbrev-commit --decorate --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' | sed -r -e 's/\\|(\\x1b\\[[0-9;]*m)+\\\\(\\x1b\\[[0-9;]*m)+ /├\\1─╮\\2/' -e 's/(\\x1b\\[[0-9;]+m)\\|\\x1b\\[m\\1\\/\\x1b\\[m /\\1├─╯\\x1b\\[m/' -e 's/\\|(\\x1b\\[[0-9;]*m)+\\\\(\\x1b\\[[0-9;]*m)+/├\\1╮\\2/' -e 's/(\\x1b\\[[0-9;]+m)\\|\\x1b\\[m\\1\\/\\x1b\\[m/\\1├╯\\x1b\\[m/' -e 's/╮(\\x1b\\[[0-9;]*m)+\\\\/╮\\1╰╮/' -e 's/╯(\\x1b\\[[0-9;]*m)+\\//╯\\1╭╯/' -e 's/(\\||\\\\)\\x1b\\[m   (\\x1b\\[[0-9;]*m)/╰╮\\2/' -e 's/(\\x1b\\[[0-9;]*m)\\\\/\\1╮/g' -e 's/(\\x1b\\[[0-9;]*m)\\//\\1╯/g' -e 's/^\\*|(\\x1b\\[m )\\*/\\1⎬/g' -e 's/(\\x1b\\[[0-9;]*m)\\|/\\1│/g' | less -r -X +/[^/]HEAD"
#alias ls='ls --color=auto'
#command -v lsd &> /dev/null && alias ls='lsd --group-dirs first'
#command -v colorls &> /dev/null && alias ls='colorls --sd --gs'
#command -v htop &> /dev/null && alias top='htop'
#command -v gotop &> /dev/null && alias top='gotop -p'

export LESS_TERMCAP_mb=$'\e[1;32m'
export LESS_TERMCAP_md=$'\e[1;32m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[01;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;4;31m'

setopt HIST_FIND_NO_DUPS
function find() {
  if [ $# = 1 ];
  then
    command find . -iname "*$@*"
  else
    command find "$@"
  fi
}

cdUndoKey() {
  popd      > /dev/null
  zle       reset-prompt
  echo
  ls
  echo
}

cdParentKey() {
  pushd .. > /dev/null
  zle      reset-prompt
  echo
  ls
  echo
}

rg() {
    if [ -z "$RANGER_LEVEL" ]
    then
        ranger
    else
        exit
    fi
}

zle -N                 cdParentKey
zle -N                 cdUndoKey
bindkey '^[[1;3A'      cdParentKey
bindkey '^[0d'      cdUndoKey

export http_proxy=http://127.0.0.1:8889
export https_proxy=http://127.0.0.1:8889
export HTTP_PROXY=http://127.0.0.1:8889
export HTTPS_PROXY=http://127.0.0.1:8889
export ALL_PROXY=socks5://127.0.0.1:1089
export NO_PROXY=localhost,127.0.0.1,10.96.0.0/12,192.168.99.0/24,192.168.39.0/24
export CURL_NIX_FLAGS="-x $http_proxy"
#source ~/工作空间/ros/workspace/local_ws/devel/setup.zsh

export RUSTUP_DIST SERVER=http://mirrors.ustc.edu.cn/rust-static
export RUSTUP_UPDATE ROOT=http://mirrors.ustc.edu.cn/rust-static/rustup
export RUST_BACKTRACE=1
export GOPATH=~/工作空间/Go
# export NIXPKGS_ALLOW_UNFREE=1
export NIX_AUTO_RUN=1

export PKG_CONFIG_PATH=/usr/local/lib64/pkgconfig/  

# eval "$(direnv hook zsh)"
