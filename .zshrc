OS=$(uname -s)

# Set Language
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8


fpath=( $HOME/.zsh "${fpath[@]}" )

# Add local packages to PATH
path=(
  $HOME/.local/bin
  $path
)

# Rust
# if [[ -d ~/.cargo ]]; then
#   path=(
#     $HOME/.cargo/bin
#     $path
#   )
# fi

# PlatformIO
# if [[ -d ~/.platformio ]]; then
#   path=(
#     $path
#     $HOME/.platformio/penv/bin
#   )
# fi

if (( $+commands[emacs] )); then

  function magit() {
    emacsclient -n -e "(magit-status)"
  }

  function em()
  {
    # -c creates a new frame
    # -a= fires a new emacs server if none is running
    # emacsclient -c -a= "" $* &> /dev/null

    # get list of emacs frames.
    frameslist=`emacsclient --eval '(frame-list)' 2>/dev/null | egrep -o '(frame)+'`

    if [ "$(echo "$frameslist" | sed -n '$=')" -ge 2 ] ;then
        # prevent creating another X frame if there is at least one present.
        emacsclient --no-wait "$@"
    else
        # Create one if there is no X window yet.
        emacsclient --no-wait --create-frame "$@"
    fi
  }
  export ALTERNATE_EDITOR="emacs"
  export GIT_EDITOR="emacsclient -c"
  export EDITOR="emacsclient -c"
  export VISUAL="emacsclient -c"
elif (( $+commands[vim] )); then
  export EDITOR='vim'
fi

if (( $+commands[psql] )); then
  export PGHOST='127.0.0.1'

  export PGPORT='5432'
  export PGUSER='postgres'
fi

# Disable "flow control"
setopt noflowcontrol

# Allow dynamic prompts
setopt prompt_subst

# Configure History
setopt append_history
setopt hist_expire_dups_first
setopt hist_fcntl_lock
setopt hist_ignore_all_dups
setopt hist_lex_words
setopt hist_reduce_blanks
setopt hist_save_no_dups
#setopt share_history
setopt HIST_IGNORE_SPACE
export BLOCK_SIZE=human-readable # https://www.gnu.org/software/coreutils/manual/html_node/Block-size.html
export HISTSIZE=11000
export SAVEHIST=10000
export HISTFILE=~/.zsh_history

# enable colored output from ls, etc
export CLICOLOR=1

# NVM (Node.JS) auto use
export NVM_AUTO_USE=true

export ZSH_AUTOSUGGEST_USE_ASYNC=true

# Delete key
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char

# direnv
if (( $+commands[direnv] )); then
  eval "$(direnv hook zsh)"
else
  echo "Missing direnv. Please install it `brew install direnv` or `apt install direnv`."
fi

# completion
autoload -Uz compinit
compinit

[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
[[ -f ~/.zshrc.private ]] && source ~/.zshrc.private

source ~/.zpluginrc

# Aliases
[[ -f ~/.aliases ]] && source ~/.aliases

. $HOME/.asdf/asdf.sh

. $HOME/.asdf/completions/asdf.bash
