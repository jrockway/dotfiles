{ config, pkgs, unstable, system, sops-nix, ... }:
let darwin = pkgs.lib.strings.hasSuffix "darwin" system;
in {
  imports = [ sops-nix.homeManagerModules.sops ];

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
    pkgs.coreutils-full
    pkgs.curlHTTP3
    pkgs.deno
    pkgs.doctl
    pkgs.esbuild
    pkgs.fd
    pkgs.git-crypt
    pkgs.gitFull
    pkgs.grafana
    pkgs.htop
    pkgs.hugo
    pkgs.iosevka
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
    pkgs.postgresql_17
    pkgs.procps
    pkgs.prometheus
    pkgs.prometheus-node-exporter
    pkgs.ripgrep
    pkgs.skopeo
    pkgs.socat
    pkgs.sops
    pkgs.tmux
    pkgs.tmux-mem-cpu-load
    pkgs.typescript-language-server
    pkgs.wget
    pkgs.yaml-language-server
    pkgs.yq
    sops-nix
    unstable.go
    unstable.gopls
    unstable.jujutsu
    unstable.ncurses
    unstable.nodejs_20
    unstable.tinygo
  ] ++ [
    (pkgs.writeShellScriptBin "bazel"
      "exec -a $0 ${pkgs.bazelisk}/bin/bazelisk $@")
    (pkgs.writeShellScriptBin "fdfind" "exec -a $0 ${pkgs.fd}/bin/fd $@")
  ] ++ (if darwin then [ ] else [ unstable.envoy ]);

  home.file = {
    ".aspell.en.prepl".source = ./aspell/aspell.en.prepl;
    ".aspell.en.pws".source = ./aspell/aspell.en.pws;
    ".bazelrc".source = ./bazel/bazelrc;
    ".emacs".source = ./emacs/emacs;
    ".gitconfig".source = ./git/gitconfig;
    ".htoprc".source = ./htop/htoprc;
    ".jq".source = ./jq/jq;
    ".tmux".source = ./tmux/tmux.conf;
    ".tmux.conf".source = ./tmux/tmux.conf;
  };

  xdg.configFile = {
    "jj".source = ./jj;
    "kitty".source = ./kitty;
  };

  home.sessionVariables = {
    EDITOR = pkgs.lib.getBin (pkgs.writeShellScript "emacsclient" ''
      exec ${config.programs.emacs.finalPackage}/bin/emacsclient -a ''' -t $@
    '');
    XCURSOR_SIZE = "16";
  } // (if !darwin then {
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  } else
    { });

  home.sessionPath = [
    "$HOME/bin"
    "$HOME/go/bin"
    "$HOME/.krew/bin"
    "$HOME/.local/bin"
    "$HOME/.dotfiles/bin"
    "/usr/local"
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
      bashrcExtra = builtins.readFile ./bash/bashrc;
      initExtra = builtins.readFile ./bash/bashrc.interactive;
      profileExtra = builtins.readFile ./bash/bash_profile;
      logoutExtra = builtins.readFile ./bash/bash_logout;
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
    emacs = {
      enable = true;
      package = pkgs.emacs-nox;
      extraPackages = e: [
        e.bazel
        e.clang-format
        e.consult
        e.consult-dir
        e.copilot
        e.corfu
        e.corfu-terminal
        e.deadgrep
        e.dockerfile-mode
        e.editorconfig
        e.fill-column-indicator
        e.format-all
        e.go-mode
        e.graphql-mode
        e.highlight-indentation
        e.jsonnet-mode
        e.markdown-mode
        e.minizinc-mode
        e.nix-mode
        e.powershell
        e.prettier-js
        e.protobuf-mode
        e.quelpa
        e.quelpa-use-package
        e.rainbow-delimiters
        e.scss-mode
        e.treesit-grammars.with-all-grammars
        e.typescript-mode
        e.use-package
        e.web-mode
        e.window-number
        e.with-editor
        e.yaml-mode
        e.yasnippet
      ];
    };
  };

  services = if !darwin then {
    emacs = {
      enable = true;
      startWithUserSession = true;
    };
  } else
    { };

  sops = {
    age.sshKeyPaths = [ "${config.home.homeDirectory}/.ssh/id_ed25519" ];
    defaultSopsFile = ./sops/secrets.sops.yaml;
    secrets.test = { path = "%r/test.txt"; };
  };

  fonts.fontconfig = {
    enable = true;
    defaultFonts.monospace = [ "Iosevka-Regular" ];
  };
}
