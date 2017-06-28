{ nixpkgs ? import <nixpkgs> {},
  ghc ? "ghc7103",
  force_build ? false }:

let

  inherit (nixpkgs) pkgs;

  vkhs = { mkDerivation, aeson, aeson-pretty, base, bytestring
      , case-insensitive, clock, containers, data-default-class
      , directory, filepath, http-client, http-client-tls, http-types
      , mtl, network-uri, optparse-applicative, parsec, pipes, pipes-http
      , pretty-show, regexpr, split, stdenv, taglib, tagsoup, text, time
      , utf8-string, vector, cabal-install, zlib, haskdogs, hasktags, scientific
      }:
      mkDerivation {
        pname = "VKHS";
        version = "1.8.2";
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
        ];
        executableHaskellDepends = [ regexpr text haskdogs ];
        executableToolDepends = [ haskdogs hasktags ];
        doHaddock = false;
        homepage = "http://github.com/grwlf/vkhs";
        description = "Provides access to Vkontakte social network via public API";
        license = stdenv.lib.licenses.bsd3;

        shellHook=''
          export LIBRARY_PATH=${pkgs.zlib}/lib:${pkgs.taglib}/lib
        '';
      };

  drv = p : pkgs.haskell.packages.${ghc}.callPackage p {};

in

  if !force_build && pkgs.lib.inNixShell then (drv vkhs).env else (drv vkhs)

