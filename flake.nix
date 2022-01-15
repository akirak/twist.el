{
  description = "The Twist frontend for Emacs";

  inputs = {
    utils.url = "github:numtide/flake-utils";
    flake-utils-plus.url = "github:gytis-ivaskevicius/flake-utils-plus/v1.3.1";
    nix-filter.url = "github:numtide/nix-filter";

    twist.url = "github:akirak/emacs-twist";
    melpa = {
      url = "github:akirak/melpa/twist";
      flake = false;
    };
    gnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
    # epkgs = {
    #   url = "github:emacsmirror/epkgs";
    #   flake = false;
    # };

    makem = {
      url = "github:alphapapa/makem.sh";
      flake = false;
    };
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils-plus
    , utils
    , ...
    } @ inputs:
    let
      inherit (builtins) removeAttrs;
      nix-filter = inputs.nix-filter.lib;
      mkApp = utils.lib.mkApp;
    in
    flake-utils-plus.lib.mkFlake {
      inherit self inputs;

      # supportedSystems = [ "x86_64-linux" ];

      channelsConfig = {
        allowBroken = false;
      };

      sharedOverlays = [
        inputs.twist.overlay
      ];

      outputsBuilder = channels:
        let
          inherit (channels.nixpkgs) system;

          emacs = channels.nixpkgs.callPackage ./nix {
            makem = inputs.makem.outPath;
          } {
            src = nix-filter.filter {
              root = ./.;
              include = [
                (nix-filter.matchExt "el")
              ];
            };
            inventories = [
              {
                type = "elpa";
                path = inputs.gnu-elpa.outPath + "/elpa-packages";
              }
              {
                type = "melpa";
                path = inputs.melpa.outPath + "/recipes";
              }
            ];            
            lockDir = ./lock;
            localPackages = [
              "twist"
            ];
          };
        in
        {
          packages = {
            inherit emacs;
          } // nixpkgs.lib.getAttrs [ "lock" "update" ] (emacs.admin "lock");

          defaultPackage = emacs;
        };
    };
}
