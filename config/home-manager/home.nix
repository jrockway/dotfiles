{ config, pkgs, unstable, system, ... }:
let darwin = pkgs.lib.strings.hasSuffix "darwin" system;
in {
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "jrockway";
  home.homeDirectory = if darwin then "/Users/jrockway" else "/home/jrockway";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "24.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = [
    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')
    pkgs.bash
    pkgs.bat
    pkgs.bazelisk
    pkgs.buildifier
    pkgs.deno
    pkgs.doctl
    pkgs.emacs
    pkgs.esbuild
    pkgs.git
    pkgs.hugo
    pkgs.istioctl
    pkgs.kind
    pkgs.kubeconform
    pkgs.kubectl
    pkgs.kubernetes-helm
    pkgs.kubeseal
    pkgs.kustomize
    pkgs.mitmproxy
    pkgs.nixd
    pkgs.nixfmt-classic
    pkgs.nodejs
    pkgs.skopeo
    pkgs.tinygo
    pkgs.tmux
    pkgs.tmux-mem-cpu-load
    pkgs.typescript-language-server
    pkgs.yq
    unstable.go
    unstable.gopls
    unstable.jujutsu
  ] ++ [
    (pkgs.writeShellScriptBin "bazel"
      "exec -a $0 ${pkgs.bazelisk}/bin/bazelisk $@")
  ] ++ (if darwin then [ ] else [ unstable.envoy ]);

  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };

  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/jrockway/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
