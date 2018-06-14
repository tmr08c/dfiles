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

# Python
export PYENV_ROOT="$HOME/.pyenv"
path=(
  $PYENV_ROOT/bin
  $path
)
if (( $+commands[pyenv] )); then
  eval "$(pyenv init -)"
fi

# Ruby (rbenv)
if [[ -d ~/.rbenv ]]; then
  export PATH="$HOME/.rbenv/bin:$PATH"
  eval "$(rbenv init -)"
else
  echo "Missing rbenv. Please install it https://github.com/rbenv/rbenv#basic-github-checkout"
fi

[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
[[ -f ~/.zshrc.private ]] && source ~/.zshrc.private

source ~/.zpluginrc

# Aliases
[[ -f ~/.aliases ]] && source ~/.aliases

# completion
autoload -Uz compinit
compinit

