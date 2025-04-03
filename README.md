# jrockway's dotfiles

This is my ~ configuration. It uses Nix and home-manager because of course it does.

On a new machine:

    git clone git@github.com:jrockway/dotfiles ~/.dotfiles
    sh <(curl -L https://nixos.org/nix/install) --daemon
    # or sh <(curl -L https://nixos.org/nix/install) # on mac os
    ln -s $HOME/.dotfiles/config/nix ~/.config/nix
    ln -s $HOME/.dotfiles/config/home-manager ~/.config/home-manager
    nix run home-manager/release-24.11 -- switch
    ln -s $HOME/.dotfiles/ssh/authorized_keys ~/.ssh/authorized_keys

For help configuring home-manager:

    man home-configuration.nix

If you want to steal something from here, feel free, but I mostly use flat config files and not the
fancy home-manager `program.<whatever> = { ... };` method. You should probably do that instead if
you're less lazy than me.
