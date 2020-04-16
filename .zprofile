# Set Language
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export GOPATH="$HOME/code/golang"

path=(
  $HOME/.local/bin
  $HOME/.platformio/penv/bin
  $HOME/.cargo/bin
  $GOPATH/bin
  $path
)

. $HOME/.asdf/asdf.sh

[[ -f ~/.zshenv.local ]] && source ~/.zshenv.local

export PATH
