VKHS
====

VKHS provides access to [Vkontakte](http://vk.com) social network, popular
mainly in Russia.  Library can be used to login into the network as a
standalone application (OAuth implicit flow as they call it). Having the access
token, it is possible to call VK API methods.

Features
--------

* Provides access to VK API via `VKT` monad and `vkq` command line tool.
* Supports HTTPS protocol.
* Supports `http_proxy` variables.
* Handles interaction with VK Login form.
* `VKT` monad is designed to handle long-running tasks and allows programs to recover
  from errors like network errors or token expirations.
* [Web.VKHS.API.Simple](./src/Web/VKHS/API/Simple.hs) module defines handy API wrappers.
  See example [`runhaskell` scripts](./app/runhaskell).

TODO
----

* ~~Decrypt 'RepeatedForm' errors~~
* ~~Support storing access-tokens in a temp file~~
* ~~Still no support for captchas, one probably should hack `defaultSupervisor`
  and add them.~~
* ~~File uploading still not functioning.~~
* Preserve cookies between sessions
* Make user-friendly multy-platform captcha display.
* Fix login automata behaviour regarding captcha failures.
* Re-test existing captcha-related functionality
* Re-implement VK monad as a Free monad special case.
* Runhaskell: handle some standard command line arguments.
* Network connection timeout is not handled by the coroutine supervisor.
* Enhance the way `vkq` accepts arguments, support multy-line messages.
* Fix grammatical mistakes here and there. Any corrections will be kindly accepted.
* Minor issues here and there. Use `git grep FIXME` to find them.
* Write simple RSS-feeder server, see [specs](./app/rss/README.md)

Installing
==========

The author of this project uses [Nix](http://nixos.org/nix) package manager for
maintaining development environment. The `default.nix` file contains Nix
expression describing the environment and a tree of dependencies.

Non-nix installations are possible, but this way requires user to install the
right Cabal and GHC versions. The GHC version which is known to work is listed
in `default.nix` file of the current repo.

    $ cat ./default.nix | grep "ghc ?"
       ghc ? "ghc844"

means that the author used `GHC-8.4.4` to build the package.

Installing with Nix
-------------------

1. Make sure that [Nix](http://nixos.org/nix) package manager is installed.
   NixOS distribution has it by default, other distros may install it
   as a regular application.
2. Check that `NIX_PATH` variable is set, and its `nixpkg` section points to
   recent `nixpkgs` repository. The `8916ac0` revision of `release-19.03` branch
   is known to work.
3. Do the following:
   ```
   $ git clone https://github.com/grwlf/vkhs
   $ cd vkhs
   $ nix-shell

     ... Wait until dependencies are fetched

   (shell) $ cabal repl vkq
   > :lo Main
   > :main --help
   > ...

   (shell) $ cabal build
   ```

Installing from Hackage
-----------------------

To install VKHS as a library, one typically should use the Cabal
package manager of Haskell, as follows:

    $ cabal update
    $ cabal install VKHS

Note, that Hackage may contain slightly outdated version of VKHS.

Installing from source
----------------------

To install from source, one typically need to install HaskellPlatform of the
right version and do the following:

    $ git clone https://github.com/grwlf/vkhs
    $ cd vkhs
    $ cabal install

Building ctags file
-------------------

`./mktags.sh` script may be used to build ctags `tags` file supported by many
text editors. The script uses `hasktags` via `haskdogs` tools, available on
Hackage.

    $ haskdogs

VKQ command line application
============================

`vkq` is a command line tool which demonstrates API usage. It can be used for
logging in, -downloading music- and reading wall messages. Call `vkq --help` or
`vkq command --help` to read online help.

Logging in to VK
----------------

In order to send API requests, the VK client typically needs an access token.
`vkq` receives it as a result of signing in.  Once received, the token may be
saved to `VKQ_ACCESS_TOKEN` environment variable, into `.vkhs-access-token`
file or passed to future instances directly using `-a` argument.

#### Saving access token in the environment variable

    $ vkq login user@mail.org pass123
    d8a41221616ef5ba19537125dc0349bad9d529fa15314ad765911726fe98b15185ac41a7ca2c62f3bf4b9
    $ export VKQ_ACCESS_TOKEN=d785932b871f096bd73aac6a35d7a7c469dd788d796463a871e5beb5c61bc6c96788ec2

Alternatively, result may be achieved using `--eval` option

    $ eval `vkq login user@mail.org pass123 --eval`

#### Saving access token to file

VKQ will cache the access token into a file. Newer versions of VKHS have
`--access-token-flag` option enabled by default. Set it to empty value to
disable the caching.


Performing API calls
--------------------

`vkq` allows user to call arbitrary API method. The generic interface is as follows:

    $ vkq api --help
    Usage: vkq api [--verbose] [--req-per-sec N] [--interactive] [--appid APPID]
                   [--user USER] [--pass PASS] [-a ACCESS_TOKEN]
                   [--access-token-file FILE] METHOD [PARAMS] [--pretty]
      Call VK API method

    Available options:
      --verbose                Be verbose
      --req-per-sec N          Max number of requests per second
      --interactive            Allow interactive queries
      --appid APPID            Application ID, defaults to VKHS
      --user USER              User name or email
      --pass PASS              User password
      -a ACCESS_TOKEN          Access token. Honores VKQ_ACCESS_TOKEN environment
                               variable
      --access-token-file FILE Filename to store actual access token, should be used
                               to pass its value between sessions
      METHOD                   Method name
      PARAMS                   Method arguments, KEY=VALUE[,KEY2=VALUE2[,,,]]
      --pretty                 Pretty print resulting JSON
      -h,--help                Show this help text


The session may look like the following:

    $ vkq api 'users.get'
    {"response":[{"first_name":"Сергей","uid":222222,"last_name":"Миронов"}]}

    $ vkq api 'messages.send' 'user_id=333333' 'message="Hi there!!!"'
    {"response":57505}

    $ vkq api 'groups.search' 'q=Haskell'
    {
        "response": [
            30,
            {
                "screen_name": "ml_mat_asm",
                "photo": "https://pp.userapi.com/c638217/v638217626/54113/v5Ib71-dDzo.jpg",
                "is_closed": 0,
                "photo_medium": "https://pp.userapi.com/c638217/v638217626/54112/Nu_si987vOc.jpg",
                "name": "Matlab | Assembler | MathCAD | Haskell | Prolog",
                "photo_big": "https://pp.userapi.com/c638217/v638217626/54111/HGnUbgUorVU.jpg",
                "gid": 78651325,
                "is_admin": 0,
                "is_member": 0,
                "type": "page"
            },
            ...
    }

VKHS library/runhaskell mode
============================

Starting from 1.7.2 the library supports runhaskell-mode.


    #!/usr/bin/env runhaskell
    {-# LANGUAGE RecordWildCards #-}

    import Web.VKHS
    import Web.VKHS.Imports

    main :: IO ()
    main = runVK_ defaultOptions $ do
      Sized cnt cs <- getCountries
      forM_ cs $ \Country{..} -> do
        liftIO $ tputStrLn co_title

When executed, the program asks for login/password and outputs list of countries
known to VK.  `getCountries` and several other methods are defined in
`Web.VKHS.API.Simple`.

The distribuption contains `./app/runhaskell` folder with a couple of examples.

Debugging
=========

Verbosity may be increased using `--verbose` flag or `o_verbose` field of
`GenericOptions`. Login automata saves `latest.html` file during operation.

References
==========
* Implicit-flow authentication, see
  [documentation in Russian](http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений)
  for details
* [VK API documentation](https://vk.com/dev/methods)

License
=======

BSD3 license

Copyright (c) 2018, Sergey Mironov <grrwlf@gmail.com>

