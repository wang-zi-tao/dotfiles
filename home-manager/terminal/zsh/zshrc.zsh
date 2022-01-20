# if [[ $TMUX != "" ]] then
    # export TERM="tmux-256color"
# fi
function sudo() {
  if [ $1 = "su" ];
  then
    command sudo zsh
  else
    args="$@"
    command sudo -E zsh -c $args
  fi
}
function wo() {
  # cd `autojump $1`
  z $1
  command tmux rename-window ${PWD##*/}
  # command tmux split -p 10
  command vim
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
  IGNORE=".envrc\nshell.nix"
  echo -e $IGNORE >> .gitignore
  echo -e $IGNORE >> .dockerignore
}
wifi(){
  if [ $# = 1 ];
  then
    command nmcli device wifi connect $@
  else
    command nmcli device wifi list --rescan yes
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

export PATH="${KREW_ROOT:-$HOME/.krew}/bin:$PATH"


alias -s gz='tar -xzvf'
alias -s tgz='tar -xzvf'
alias -s zip='unzip'
alias -s bz2='tar -xjvf'
alias -s php=nvim
alias -s py=nvim
alias -s html=nvim
alias -s java=nvim
alias -s html=nvim
alias -s css=nvim
alias -s js=nvim
alias -s ts=nvim
alias -s rs=nvim
alias -s c=nvim
alias -s h=nvim
alias -s cpp=nvim
alias -s hpp=nvim
