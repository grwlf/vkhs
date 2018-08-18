{ nixpkgs ? import <nixpkgs> {},
  ghc ? "ghc822",
  force_build ? false }:

let

  inherit (nixpkgs) pkgs;

  vkhs = { mkDerivation, aeson, aeson-pretty, base, bytestring
      , case-insensitive, clock, containers, data-default-class
      , directory, filepath, http-client, http-client-tls, http-types
      , mtl, network-uri, optparse-applicative, parsec, pipes, pipes-http
      , pretty-show, regexpr, split, stdenv, taglib, tagsoup, text, time
      , utf8-string, vector, cabal-install, zlib, haskdogs, hasktags, scientific
      , hdevtools, lens
      }:
      mkDerivation {
        pname = "VKHS";
        version = "1.9.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        extraLibraries = [taglib zlib];
        libraryHaskellDepends = [
          aeson aeson-pretty base bytestring case-insensitive clock
          containers data-default-class directory filepath http-client
          http-client-tls http-types mtl network-uri optparse-applicative
          parsec pipes pipes-http pretty-show split taglib tagsoup time
          utf8-string vector cabal-install zlib scientific
          hdevtools pkgs.python3 lens
        ];
        executableHaskellDepends = [ regexpr text haskdogs ];
        executableToolDepends = [ haskdogs hasktags ];
        doHaddock = false;
        homepage = "http://github.com/grwlf/vkhs";
        description = "Provides access to Vkontakte social network via public API";
        license = stdenv.lib.licenses.bsd3;

        shellHook=''
          if test -f /etc/myprofile ; then
            . /etc/myprofile
          fi
          export LIBRARY_PATH=${pkgs.zlib}/lib:${pkgs.taglib}/lib
          cabal() {( `which cabal` --ghc-options=-freverse-errors "$@" ; )}
        '';
      };

  drv = p : pkgs.haskell.packages.${ghc}.callPackage p {};

in

  if !force_build && pkgs.lib.inNixShell then (drv vkhs).env else (drv vkhs)

