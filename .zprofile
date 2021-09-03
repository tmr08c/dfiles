# Set Language
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export GOPATH="$HOME/code/golang"

export EMACSDIR="~/.emacs-doom.d"
export DOOMDIR="~/.doom.d"
export DOOMLOCALDIR="~/.emacs-doom.d/.local"

arch_name="$(uname -m)"

IS_M1=false
if [ "${arch_name}" = "arm64" ]; then
  IS_M1=true
fi

if [ $IS_M1 ]; then
  eval $(/opt/homebrew/bin/brew shellenv)
fi

export IS_M1

path=(
  $HOME/.local/bin
  $HOME/.platformio/penv/bin
  $HOME/.cargo/bin
  $GOPATH/bin
  $HOME/.cask/bin
  $path
)

. $HOME/.asdf/asdf.sh

[[ -f ~/.zshenv.local ]] && source ~/.zshenv.local

export PATH
