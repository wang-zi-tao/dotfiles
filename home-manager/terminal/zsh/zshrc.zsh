typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet

# if [[ $TMUX != "" ]] then
    # export TERM="tmux-256color"
# fi
function tmux() {
    if [[ -n "$NVIM" || -n "$SSH_CONNECTION" ]];then
        if (( "$#" == 0 )); then
            command tmux new-session \; set-option prefix2 C-a
        else
            command tmux "$@" \; set-option prefix2 C-a
        fi
    else
        command tmux "$@"
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

if [[ -e $SSH_CONNECTION && -n $TMUX ]];then
  command tmux set prefix C-v
fi

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

alias -s xlsx=et
alias -s doc=wps
alias -s docx=wps
upfind() {
  folder="$(pwd)"
  while [ "$folder" != "/" ]; do
    for arg in "$@"; do
      if [ -e "${folder}/$arg" ]; then
        echo $folder/$arg
        folder="/"
      fi
    done
    folder="$(dirname "$folder")"
  done
}

ninja(){
  local build_path="$(dirname "$(upfind "build.ninja")")"
  command ninja -C "${build_path:-.}" "$@"
}
make(){
  local build_path="$(dirname "$(upfind "Makefile")")"
  command make -C "${build_path:-.}" "$@"
}
retry() {
  local n=0
  local trys=${TRYS:32}
  local sleep_time=${SLEEP:10}
  until ($1 "${@:2}") ; do
      n=$(( n + 1 ))
      [ $n -gt $trys ] && return 1
      sleep $sleep_time
  done
}
own() {
  if [[ -n "${commands[sudo]}" ]]; then
    sudo chown -R "$USER:$(id -gn)" "$@"
  else
    chown -R "$USER:$(id -gn)" "$@"
  fi
}
mkcd() { mkdir -p "$1" && cd "$1"; }
new-nix-shell() {
  if [[ ! -e shell.nix ]]; then
    cat > shell.nix <<'EOF'
{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = with pkgs; [
      
    ];
}
EOF
    ${EDITOR:-vim} shell.nix
  fi
  if [ -e ./.git ]; then 
    git add shell.nix
  fi
  if [ ! -e ./.envrc ]; then
    echo "use_nix" > .envrc
    direnv allow
  fi
}
new-nix-shell-flake() {
  if [ ! -e ./.direnv ]; then
    mkdir .direnv
  fi
  if [[ ! -e flake.nix ]]; then
    cat > flake.nix <<'EOF'
{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  # inputs.fenix.url = "github:nix-community/fenix";
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [ 

            # (inputs.fenix.packages.${system}.fromToolchainFile {
            #   file = ./rust-toolchain.toml;
            #   sha256 = lib.fakeHash;
            # })
          ];
        };
      });
}
EOF
    ${EDITOR:-vim} flake.nix
  fi
  if [ -e ./.git ]; then 
    git add flake.nix
  fi
  if [ ! -e ./.envrc ]; then
    echo "use flake" > .envrc
    direnv allow
  fi
}
function nrun() {
  command nix run "nixpkgs#$1"
}
function nshell() {
  command nix shell "nixpkgs#$1"
}
if [[ -n "$TMUX" && ( -n "$SSH_CONNECTION" ) ]]; then
  command tmux set-option prefix2 C-a
fi
