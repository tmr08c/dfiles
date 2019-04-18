export GOPATH="$HOME/code/golang"
export PATH="$HOME/.asdf/bin:$HOME/.asdf/shims:$HOME/.platformio/penv/bin:$HOME/.cargo/bin:$GOPATH/bin:$PATH"

if [ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]; then . $HOME/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
