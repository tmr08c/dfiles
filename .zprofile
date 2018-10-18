OS=$(uname -s)

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

if (( $+commands[emacs] )); then
  export EDITOR='emacs'
elif (( $+commands[vim] )); then
  export EDITOR='vim'
fi

if (( $+commands[psql] )); then
  export PGHOST='127.0.0.1'

  export PGPORT='5432'
  export PGUSER='postgres'
fi

if (( $+commands[gpgconf] )); then
  unset SSH_AGENT_PID
  if [ "${gnupg_SSH_AUTH_SOCK_by:-0}" -ne $$ ]; then
    export SSH_AUTH_SOCK="$(gpgconf --list-dirs agent-ssh-socket)"
  fi

  export GPG_TTY=$(tty)
  export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
  gpgconf --launch gpg-agent
  gpg-connect-agent updatestartuptty /bye >/dev/null
fi

# if [ $TILIX_ID ] || [ $VTE_VERSION ]; then
  # source /etc/profile.d/vte.sh
# fi
