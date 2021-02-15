OS=$(uname -s)

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
# export NVM_AUTO_USE=true

# export ZSH_AUTOSUGGEST_USE_ASYNC=true

# Delete key
bindkey    "^[[3~"          delete-char
bindkey    "^[3;5~"         delete-char

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing DHARMA Initiative Plugin Manager (zdharma/zinit)…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

export GITSTATUS_LOG_LEVEL=DEBUG
zinit ice depth=1; zinit light romkatv/powerlevel10k

zinit fpath -f ${ASDF_DIR}/completions

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-as-monitor

# zinit from"gh-r" as"program" mv"direnv* -> direnv" \
#     atclone'./direnv hook zsh > zhook.zsh' atpull'%atclone' \
#     pick"direnv" src="zhook.zsh" for \
#         direnv/direnv

# zinit ice atclone"dircolors -b LS_COLORS > clrs.zsh" \
#     atpull'%atclone' pick"clrs.zsh" nocompile'!' \
#     atload'zstyle ":completion:*" list-colors “${(s.:.)LS_COLORS}”'
# zinit light trapd00r/LS_COLORS

zinit wait lucid light-mode for \
  atinit"zicompinit; zicdreplay" \
      zdharma/fast-syntax-highlighting \
  atload"_zsh_autosuggest_start" \
      zsh-users/zsh-autosuggestions \
  blockf atpull'zinit creinstall -q .' \
      zsh-users/zsh-completions


# zinit light djui/alias-tips



### End of Zinit's installer chunk

# Use asdf for direnv to speed up lookups on commands
#
# See ~/.config/direnv/direnvrc
eval "$(asdf exec direnv hook zsh)"
direnv() { asdf exec direnv "$@"; }

# VTerm Compat
function vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

export ERL_AFLAGS="-kernel shell_history enabled"

[[ -f ~/.zshrc.local ]] && source ~/.zshrc.local
[[ -f ~/.zshrc.private ]] && source ~/.zshrc.private

# Aliases
[[ -f ~/.aliases ]] && source ~/.aliases

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
