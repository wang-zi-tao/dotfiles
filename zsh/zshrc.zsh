source ${HOME}/.p10k.zsh
# if [[ $TMUX != "" ]] then
    # export TERM="tmux-256color"
# fi
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
  # cd `autojump $1`
  z $1
  command tmux rename-window ${PWD##*/}
  # command tmux split -p 10
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

r() {
    if [ -z "$RANGER_LEVEL" ]
    then
        ranger
    else
        exit
    fi
}
gen-nix-shell(){
  echo "{ pkgs ? import <nixpkgs> {} }:
    pkgs.mkShell {
      nativeBuildInputs = with pkgs; [
        $@
      ];
  }" > shell.nix
  echo use_nix >> .envrc
  direnv allow .
  if test -f ".gitignore"; then
    echo -e ".envrc\nshell.nix" >> .gitignore
  fi
}
wifi(){
  if [ $# = 1 ];
  then
    command nmcli device wifi $@
  else
    command nmcli device wifi list
  fi
}
reload(){
  source ${HOME}/.zshrc
}
# killall(){
  # killall `which $1`
# }

zle -N                 cdParentKey
zle -N                 cdUndoKey
bindkey '^[[1;3A'      cdParentKey
bindkey '^[0d'      cdUndoKey
bindkey '^[\' tmux split -h
bindkey '^[-' tmux split -v

eval "$(zoxide init zsh)"
# eval "$(mcfly init zsh)"

export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"
