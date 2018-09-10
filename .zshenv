# Set Language
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# asdf version manager
if [[ -d ~/.asdf ]]; then
  source ~/.asdf/asdf.sh
fi

# Rust
if [[ -d ~/.cargo ]]; then
  path=(
    $HOME/.cargo/bin
    $path
  )
fi

# PlatformIO
if [[ -d ~/.platformio ]]; then
  path=(
    $path
    $HOME/.platformio/penv/bin
  )
fi

if [ -f ~/.zshenv.local ]; then
	source ~/.zshenv.local
fi
