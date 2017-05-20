{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7103" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, aeson-pretty, base, bytestring
      , case-insensitive, clock, containers, data-default-class
      , directory, filepath, http-client, http-client-tls, http-types
      , mtl, network-uri, optparse-applicative, parsec, pipes, pipes-http
      , pretty-show, regexpr, split, stdenv, taglib, tagsoup, text, time
      , utf8-string, vector, cabal-install, zlib, haskdogs, hasktags
      }:
      mkDerivation {
        pname = "VKHS";
        version = "1.7.2";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        extraLibraries = [taglib zlib];
        libraryHaskellDepends = [
          aeson aeson-pretty base bytestring case-insensitive clock
          containers data-default-class directory filepath http-client
          http-client-tls http-types mtl network-uri optparse-applicative
          parsec pipes pipes-http pretty-show split taglib tagsoup time
          utf8-string vector cabal-install zlib
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

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = p : haskellPackages.callPackage p {};

in

  if pkgs.lib.inNixShell then (drv f).env else (drv f)
