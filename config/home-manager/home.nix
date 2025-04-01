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
    pkgs.bashInteractive
    pkgs.bat
    pkgs.bazel-watcher
    pkgs.bazelisk
    pkgs.buildifier
    pkgs.curlHTTP3
    pkgs.deno
    pkgs.doctl
    pkgs.emacs
    pkgs.esbuild
    pkgs.git-crypt
    pkgs.gitFull
    pkgs.grafana
    pkgs.htop
    pkgs.hugo
    pkgs.iperf3
    pkgs.istioctl
    pkgs.jq
    pkgs.jsonnet
    pkgs.jsonnet-language-server
    pkgs.kind
    pkgs.krew
    pkgs.kubeconform
    pkgs.kubectl
    pkgs.kubectx
    pkgs.kubernetes-helm
    pkgs.kubeseal
    pkgs.kustomize
    pkgs.mg
    pkgs.mitmproxy
    pkgs.nixd
    pkgs.nixfmt-classic
    pkgs.nodePackages.concurrently
    pkgs.nodePackages.prettier
    pkgs.nodejs
    pkgs.postgresql_17
    pkgs.procps
    pkgs.prometheus
    pkgs.prometheus-node-exporter
    pkgs.ripgrep
    pkgs.skopeo
    pkgs.socat
    pkgs.tmux
    pkgs.tmux-mem-cpu-load
    pkgs.typescript-language-server
    pkgs.wget
    pkgs.yaml-language-server
    pkgs.yq
    unstable.go
    unstable.gopls
    unstable.jujutsu
    unstable.tinygo
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

  home.sessionVariables = { XCURSOR_SIZE = "16"; };

  home.sessionPath = [
    "$HOME/bin"
    "$HOME/go/bin"
    "$HOME/.krew/bin"
    "$HOME/.local/bin"
    "$HOME/.dotfiles/bin"
    "/usr/local/scripts"
    "/sbin"
    "/usr/sbin"
  ];

  # Let Home Manager install and manage itself.
  programs = {
    home-manager.enable = true;
    bash = {
      enable = true;
      enableCompletion = true;
      historyFileSize = 100000;
      historySize = 10000;
      bashrcExtra = builtins.readFile ./bashrc;
      initExtra = builtins.readFile ./bashrc.interactive;
      profileExtra = builtins.readFile ./bash_profile;
      logoutExtra = builtins.readFile ./bash_logout;
      shellAliases = {
        cover = "go test -coverprofile=cover.out -covermode=atomic";
        coverall =
          "go test -coverprofile=cover.out ./... -covermode=atomic -coverpkg=./...";
        coverreport =
          "go tool cover -html cover.out -o cover.html && serveme cover.html";
        kctx = "kubectx";
        kns = "kubens";
        k = "kubectl";
        pctx = "pachctl config set active-context";
        gaz = "bazel run //:gazelle";
        bdf = "bazel run //:buildifier";
        master = "jj bookmark move --to=@ master";
        main = "jj bookmark move --to=@ main";
        h = "history";
        ec = "emacsclient -t";
        hm = "home-manager";
      };
      shellOptions = [ "cmdhist" "checkwinsize" "cdable_vars" "histappend" ];
    };
  };
}
