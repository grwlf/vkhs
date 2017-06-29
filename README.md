VKHS
====

VKHS provides access to [Vkontakte](http://vk.com) social network, popular
mainly in Russia.  Library can be used to login into the network as a
standalone application (OAuth implicit flow as they call it). Having the access
token, it is possible to call various API methods to -query audio files-
(disabled by VK) or retrieve wall messages.

Features
========

* Provides access to VK API. Interface options include: VK monad and `vkq` command
  line tool.
* Uses HTTPS protocol.
* Solves login form interaction, may be used to operate new/disposable VK accounts.
* VK monad is designed as an interruptable coroutine. The supervisor supports
  ondemand re-login, and may be used for long-running tasks.
* Project includes a set of `Web.VKHS.API.Simple` wrappers designed to be
  copied into `runhaskell` scripts and tweaked according to ones need.
* No more dependencies on curlhs/taglib.

Issues
======

* Still no support for captchas, one probably should hack `defaultSupervisor`
  and add them.
* Network connection timeout is not handled by the coroutine supervisor.
* Minor issues here and there. Use `git grep FIXME` to find them
* File uploading still not functioning.
* Lots grammatical mistakes. Any corrections will be kindly accepted.

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
`vkq --help [command]` to read online help.


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

Alternatively, using `--eval` option

    $ eval `vkq login user@mail.org pass123 --eval`

#### Saving access token to file

    $ vkq login --access-token-file=.access-token

VKQ will ask for email/password and cache the access token into a file.  Newer
versions of VKHS have `--access-token-flag` option enabled by default. Set it
to empty value to disable the caching feature.


Performing custom API calls
---------------------------

vkq allows user to call arbitrary API method. The format is as follows:

    Usage: vkq call [--verbose] [--req-per-sec N] [--interactive] [--appid APPID]
                    [--user USER] [--pass PASS] [-a ACCESS_TOKEN] METHOD PARAMS


For example, lets call ausio.search method to get some Beatles records:

    $ vkq call group.search q=Beatles --pretty

      { "response": [
        614751,
        {
            "lyrics_id": "6604412",
            "url": "http://cs1-36v4.vk-cdn.net/p16/59674dd8717db2.mp3?extra=k0s2ja3l6pq6aIDOEW5y5XUCs2--JLX9wZpzOT3iuSnZPR-DNhJSF075NUhICB_szMOKKlVJFFlqLlg691q6cKhwiGZgTRU1oAimXzXY396cfNAHnotc8--7w-0xnvoPK6qVoI8",
            "aid": 85031440,
            "title": "Twist and Shout  ",
            "genre": 1,
            "owner_id": 9559206,
            "duration": 156,
            "artist": "The Beatles"
        },
    ...


VKHS library/runhaskell mode
============================

Starting from 1.7.2 the library supports RunHaskell-mode. Consider the
following example:


    #!/usr/bin/env runhaskell
    {-# LANGUAGE RecordWildCards #-}

    import Prelude ()
    import Web.VKHS
    import Web.VKHS.Imports

    main :: IO ()
    main = runVK_ defaultOptions $ do
      Sized cnt cs <- getCountries
      forM_ cs $ \Country{..} -> do
        liftIO $ putStrLn co_title

When executed, the program asks for login/password and outputs list of countries
known to VK.  `getCountries` and several other methods are defined in
`Web.VKHS.API.Simple`. `vkq` application may be used as a more elaborated
example.

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

