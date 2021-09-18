source ${HOME}/.p10k.zsh
if [[ $TMUX != "" ]] then
    export TERM="tmux-256color"
fi
function find() {
  if [ $# = 1 ];
  then
    command find . -iname "*$@*"
  else
    command find "$@"
  fi
}
function sudo() {
  if [ $1 = "su" ];
  then
    command sudo tmux
  else
    command sudo $@
  fi
}
function wo() {
  cd `autojump $1`
  command tmux split -p 10
  command vim
}
systemctl()
{
  command sudo systemctl "$@"
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
killall(){
  killall `which $1`
}

zle -N                 cdParentKey
zle -N                 cdUndoKey
bindkey '^[[1;3A'      cdParentKey
bindkey '^[0d'      cdUndoKey
