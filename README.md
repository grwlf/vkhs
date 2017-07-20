VKHS
====

VKHS provides access to [Vkontakte](http://vk.com) social network, popular
mainly in Russia.  Library can be used to login into the network as a
standalone application (OAuth implicit flow as they call it). Having the access
token, it is possible to call VK API methods.

Features
--------

* Provides access to VK API. Interface options include: VK monad and `vkq` command
  line tool.
* Uses HTTPS protocol.
* Solves login form interaction, may be used to operate new/disposable VK accounts.
* VK monad is designed as an interruptable coroutine. The supervisor supports
  ondemand re-login, and may be used for long-running tasks.
* Project includes a set of `Web.VKHS.API.Simple` wrappers designed to be
  copied into `runhaskell` scripts and tweaked according to ones need.
* No more dependencies on curlhs/taglib.

ToDo
----

* ~~Decrypt 'RepeatedForm' errors~~
* ~~Support storing access-tokens in a temp file~~
* Still no support for captchas, one probably should hack `defaultSupervisor`
  and add them.
* Re-implement VK monad as a Free monad special case
* Runhaskell: handle some standard command line arguments
* Minor issues here and there. Use `git grep FIXME` to find them
* ~~File uploading still not functioning.~~
* Network connection timeout is not handled by the coroutine supervisor.
* Enhance the way `vkq` accepts arguments, support multy-line messages.
* Grammatical mistakes. Any corrections will be kindly accepted.

Installing
==========

Installing from Hackage
-----------------------

In order to install VKHS, one typically should do the following

    $ cabal update
    $ cabal install VKHS


Installing from source
----------------------

    $ git clone https://github.com/grwlf/vkhs
    $ cd vkhs
    $ cabal install

Developing using Nix
--------------------

The author of this project uses [Nix](http://nixos.org) as a main development
platform. The `default.nix` file contain Nix expression describing the environment

#### Entering Nix shell environment

    $ git clone https://github.com/grwlf/vkhs
    $ cd vkhs
    $ nix-shell

#### Usual development

    $ ghci -isrc:app/vkq:app/common
    $ cabal install
    $ ^D

#### Returning to the system shell

    $ ^D
    $ nix-build
    $ ls ./result


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
                   [--access-token-file FILE] METHOD PARAMS [--pretty]
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

    $ vkq api "messages.send" "user_id=111111,message=\"test\""  --pretty
    bd7da7e9cfb4cc12c0a49093173ca8785c7d6c918f00edb7315bb8526f5f372f1174b643e50e1a47d35da

    $ vkq api "users.get" ""
    {"response":[{"first_name":"Сергей","uid":222222,"last_name":"Миронов"}]}

    $ vkq api "messages.send" "user_id=333333,message=Hi theree!"
    {"response":57505}

    $ vkq api "groups.search" "q=Haskell"
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

Copyright (c) 2014, Sergey Mironov <grrwlf@gmail.com>

