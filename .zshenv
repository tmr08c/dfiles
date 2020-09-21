# Ensure that a non-login, non-interactive shell has a defined environment.
# (Only once) if it was not sourced before, because .zshenv is always sourced
if [[ ( "$SHLVL" -eq 1 && ! -o LOGIN ) && -s "${ZDOTDIR:-$HOME}/.zprofile" ]]; then
  source "$HOME/.zprofile"
fi
