{ lib
, emacsTwist
, makem
, bash
, getopt
}:
{
  inventories
, lockDir
, src
, localPackages
, extraPackages ? [ ]
  # Lint options
, useMakem ? true
, checkdoc ? true
, check-declare ? true
, elsa ? true
, indent-lint ? true
# , package-lint ? true
, relint ? true
}:
let
  # package-lint requires package-archives to check installability of packages,
  # which is unsuitable for running inside Nix sandboxes. Thus its support is
  # dropped in the lintPhase for now. However, you can run it on CI.

  lintPackages =
    lib.optional relint "relint" # gnu-elpa
    # ++ lib.optional package-lint "package-lint" # melpa
    ++ lib.optional elsa "elsa"; # melpa

  makemRules =
    lib.optional checkdoc "lint-checkdoc"
    ++ lib.optional check-declare "lint-declare"
    ++ lib.optional elsa "lint-elsa"
    ++ lib.optional indent-lint "lint-indent"
    # ++ lib.optional package-lint "lint-package"
    ++ lib.optional relint "lint-regexps";

  emacsForLint = (emacsTwist {
    inherit inventories;
    inherit lockDir;
    initFiles = [ ];
    extraPackages = lintPackages;
  }).overrideScope' twistOverlay;

  makemPhase = ''
    makem_args=()
    for el in *.el
    do
      makem_args+=(-f "$el")
    done
    
    # /usr/bin/env is unavailable in the sandboxed environment, so run
    # makem via a provided bash
    #
    # Also, makem requires getopt.
    PATH="${getopt}/bin:$PATH" ${bash}/bin/bash \
      ${makem}/makem.sh -E "${emacsForLint}/bin/emacs" \
      --no-compile ''${makem_args[@]} ${lib.escapeShellArgs makemRules}
  '';

  twistOverlay = _self: super:
    {
      elispPackages = super.elispPackages.overrideScope' (eself: esuper:
        builtins.mapAttrs (ename: epkg:
          if builtins.elem ename localPackages
          then epkg.overrideAttrs (_: {
            # Strictly check issues
            errorOnWarn = true;
            doByteCompileSeparately = true;

            lint = lib.optionalString useMakem makemPhase;
          })
          else epkg.overrideAttrs (_: {
            # Ignore errors of dependencies
            dontByteCompile = true;
          })
        ) esuper
      );
    };

  packageNames =
    localPackages
    ++ extraPackages
    ++ lib.optionals useMakem lintPackages;

  twist = (emacsTwist {
    inherit inventories;
    inherit lockDir;
    initFiles = [ ];
    extraPackages = packageNames;
    inputOverrides = lib.genAttrs localPackages (_: _: _: {
      inherit src;
    });
  }).overrideScope' twistOverlay;
in
twist
