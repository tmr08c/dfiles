# Set Language
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

export GOPATH="$HOME/code/golang"

path=(
  $HOME/.local/bin
  $HOME/.asdf/bin
  $HOME/.asdf/shims
  $HOME/.platformio/penv/bin
  $HOME/.cargo/bin
  $GOPATH/bin
  $path
)

[[ -f ~/.zprofile.local ]] && source ~/.zprofile.local

. $HOME/.asdf/asdf.sh
