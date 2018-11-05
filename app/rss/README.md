VKHS-RSS
========

This file contains the spec of Vkontakte-to-RSS feeder application. Components:

* VKHS. This repository.
* [RSS](https://github.com/haskell-hvr/rss)
* [Nix](https://nixos.org) to provide inetd-like environment

[RSS Example](https://github.com/haskell-hvr/rss/blob/master/examples/whiskers.hs)

Minimal design
--------------

* The App should be a non-interactive console application, input is taken via
  command-line arguments, output goes to standard output.
* Appliction should accept the following command line arguments:
  - Vkontakte login credentials: username + password, other VKHS options via
    `--vkhs-options` option.
  - Vkontakte group ID to query posts from, See `GroupId`.
* After lunch, application should convert Group posts to RSS entries of the feed
  and print resulting XML to the stdout.
* Nix should be used to wrap the application into intetd-like service.

Enhanced design
---------------

* Find a way to convert RSS-encoded query into Vkontakte query. Return posts
  according to query results.

Notes
-----

* One should provide test account
