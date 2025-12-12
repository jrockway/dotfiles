{ config, pkgs, unstable, sops-nix, username, ... }:
let
  darwin = pkgs.stdenv.isDarwin;
  emacs = if darwin then pkgs.emacs else pkgs.emacs-nox;
in {
  imports = [ sops-nix.homeManagerModules.sops ];

  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = username;
  home.homeDirectory =
    if darwin then "/Users/" + username else "/home/" + username;

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
    pkgs.aspell
    pkgs.bashInteractive
    pkgs.bat
    pkgs.bazel-watcher
    pkgs.bazelisk
    pkgs.bc
    pkgs.buildifier
    pkgs.cfssl
    pkgs.copilot-language-server
    pkgs.coreutils-full
    pkgs.curl
    pkgs.deno
    pkgs.devcontainer
    pkgs.dig
    pkgs.doctl
    pkgs.esbuild
    pkgs.fd
    pkgs.gh
    pkgs.git-crypt
    pkgs.gitFull
    pkgs.gnumake
    pkgs.go
    pkgs.gopls
    pkgs.grafana
    pkgs.grafana-loki
    pkgs.hdf5
    pkgs.helmfile
    pkgs.htop
    pkgs.hugo
    pkgs.iosevka
    pkgs.iperf3
    pkgs.istioctl
    pkgs.jdk
    pkgs.jq
    pkgs.jsonnet
    pkgs.jsonnet-language-server
    pkgs.jujutsu
    pkgs.kind
    pkgs.krew
    pkgs.kubeconform
    pkgs.kubectl
    pkgs.kubectx
    pkgs.kubernetes-helm
    pkgs.kubeseal
    pkgs.kustomize
    pkgs.less
    pkgs.mg
    pkgs.minikube
    pkgs.mitmproxy
    pkgs.moon
    pkgs.mqttui
    pkgs.ncurses
    pkgs.neofetch
    pkgs.nginxMainline
    pkgs.nix-output-monitor
    pkgs.nixd
    pkgs.nixfmt-classic
    pkgs.nixos-rebuild
    pkgs.nodePackages.prettier
    pkgs.nodejs_22
    pkgs.nvd
    pkgs.openssl
    pkgs.postgresql_17
    pkgs.procps
    pkgs.prometheus
    pkgs.prometheus-node-exporter
    pkgs.proto
    pkgs.qmk
    pkgs.ripgrep
    pkgs.skopeo
    pkgs.socat
    pkgs.sops
    pkgs.ssh-to-age
    pkgs.stress-ng
    pkgs.termcap
    pkgs.texinfoInteractive
    pkgs.tinygo
    pkgs.tmux
    pkgs.tmux-mem-cpu-load
    pkgs.typescript-language-server
    pkgs.units
    pkgs.vscode-langservers-extracted
    pkgs.vue-language-server
    pkgs.wget
    pkgs.yaml-language-server
    pkgs.yq
    sops-nix
  ] ++ [
    (pkgs.writeShellScriptBin "bazel"
      "exec -a $0 ${pkgs.bazelisk}/bin/bazelisk $@")
    (pkgs.writeShellScriptBin "fdfind" "exec -a $0 ${pkgs.fd}/bin/fd $@")
    (pkgs.writeShellApplication {
      name = "gencert";
      runtimeInputs = [ pkgs.cfssl ];
      text = ''
        exec cfssl gencert -config "$HOME/.ca/ca-config.json" -ca "$HOME/.ca/ca.pem" -ca-key "$HOME/.ca/ca-key.pem" "$@"'';
    })
  ] ++ (if darwin then
    [ ]
  else [
    pkgs.envoy-bin
    pkgs.lm_sensors
    pkgs.nixos-install-tools
    pkgs.tinymembench
  ]);

  home.file = {
    ".aspell.en.prepl".source = ./aspell/aspell.en.prepl;
    ".aspell.en.pws".source = ./aspell/aspell.en.pws;
    ".bazelrc".source = ./bazel/bazelrc;
    ".emacs".source = ./emacs/emacs;
    ".htoprc".source = ./htop/htoprc;
    ".jq".source = ./jq/jq;
    ".tmux".source = ./tmux/tmux.conf;
    ".tmux.conf".source = ./tmux/tmux.conf;
    ".ca" = {
      source = ./ca;
      recursive = true;
    };
  };

  xdg.configFile = {
    "jj".source = ./jj;
    "kitty".source = ./kitty;
    "git".source = ./git;
  };

  home.sessionVariables = {
    EDITOR = pkgs.lib.getBin (pkgs.writeShellScript "emacsclient" ''
      exec ${config.programs.emacs.finalPackage}/bin/emacsclient -a ''' -t $@
    '');
    TERMINFO = "$HOME/.nix-profile/lib/terminfo";
    XCURSOR_SIZE = "16";
  } // (if !darwin then {
    LOCALE_ARCHIVE = "${pkgs.glibcLocales}/lib/locale/locale-archive";
  } else {
    SSH_AUTH_SOCK =
      "$HOME/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock";
  });

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
        hms = "nh home switch";
      } // (if darwin then {
        emacs =
          "${config.programs.emacs.finalPackage}/Applications/Emacs.app/Contents/MacOS/Emacs";
      } else
        { });
      shellOptions = [ "cmdhist" "checkwinsize" "cdable_vars" "histappend" ];
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    emacs = {
      enable = true;
      package = emacs;
      extraPackages = e:
        let
          # The package patches copilot to refer to copilot-language-server-fhs instead of
          # copilot-language-server; this patching isn't possible to do on Darwin.  It's only
          # referred to in the postPatch directive, so we'll just skip patching.
          copilot = if darwin then
            e.copilot.overrideAttrs { postPatch = ""; }
          else
            e.copilot;
        in [
          copilot
          e.bazel
          e.clang-format
          e.consult
          e.consult-dir
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
          e.kkp
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
          e.vterm
          e.web-mode
          e.window-number
          e.with-editor
          e.yaml-mode
          e.yasnippet
        ];
    };

    fzf = {
      enable = true;
      enableBashIntegration = true;
      changeDirWidgetCommand = "fd --type d";
      defaultCommand = "fd --type f";
      tmux.enableShellIntegration = true;
    };

    nh = {
      enable = true;
      flake = "${config.home.homeDirectory}/.config/home-manager";
    };
  };

  services = { } // (if !darwin then {
    home-manager.autoExpire = {
      enable = true;
      frequency = "daily";
      timestamp = "-7 days";
      store.cleanup = true;
    };
    emacs = {
      enable = true;
      startWithUserSession = true;
    };
  } else
    { });

  sops = {
    age.sshKeyPaths = [ "${config.home.homeDirectory}/.ssh/id_ed25519" ];
    defaultSopsFile = ./sops/secrets.sops.yaml;
    secrets.test = { path = "%r/test.txt"; };
    secrets."ca-key.pem" = {
      path = "${config.home.homeDirectory}/.ca/ca-key.pem";
    };
  };

  fonts.fontconfig = {
    enable = true;
    defaultFonts.monospace = [ "Iosevka-Regular" ];
  };
}
