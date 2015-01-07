VKHS
====

VKHS provides access to [Vkontakte][1] social network, popular mainly in Russia.
Library can be used to login into the network as a standalone application (OAuth
implicit flow as they call it). Having the access token, it is possible to call
various API methods to query audio files or retrieve wall messages. For now,
vkhs offers limited error detection and no captcha support.

Installing
==========

In order to install VKHS, type:

    $ cabal update
    $ cabal install VKHS

Note, that VKHS uses curlhs and should be linked with libcurl.so. Normally,
cabal handles it without problems.

VKQ
===

VKQ is a command line tool which demonstrates API usage. It can be used for
logging in, downloading music and reading wall messages.

Here is an example session: Login first

    $ vkq login user@mail.org pass123
    d785932b871f096bd73aac6a35d7a7c469dd788d796463a871e5beb5c61bc6c96788ec2 11229 32833

VKQ returns three values. First is a access token which is required to execute
future API requests. VKQ reads it from VKQ\_ACCESS\_TOKEN environment variable so
we have to set it up

    $ export VKQ_ACCESS_TOKEN=d785932b871f096bd73aac6a35d7a7c469dd788d796463a871e5beb5c61bc6c96788ec2

Now, lets list first 10 audio files I have:

    $ vkq music -l | head -n 10
    912727_192006572 http://cs1664.vk.me/u20498630/audios/8f68e38fcc49.mp3 Lestasi Delloro The Ecstasy Of Gold
    912727_191904112 http://cs4609.vk.me/u4854343/audios/47dcc3f01122.mp3   Django OST Djano Unchained
    912727_183449471 http://cs4273.vk.me/u2127815/audios/b39b5dc91f85.mp3   Polite Dance Song
    912727_175802783 http://cs1630.vk.me/u132798/audios/c7abcb76b7e1.mp3 Norwegian Wood
    912727_173085876 http://cs4632.vk.me/u1821064/audios/b040e3fd8372.mp3   My Love Is Like A Red Red Rose
    912727_170897203 http://cs4627.vk.me/u14418258/audios/51473dc3583c.mp3  Tunnel Tigers
    912727_169499147 http://cs509.vk.me/u23213659/audios/18ec38fdf057.mp3   Ev sistr ta Laou - запрещена в Великобритании
    912727_166538873 http://cs4655.vk.me/u56769349/audios/ab67c54453ab.mp3  Без названия
    912727_133140937 http://cs4502.vk.me/u21815494/audios/3937c2d7f313.mp3  The Voice
    912727_154914755 http://cs5062.vk.me/u22207393/audios/52508f5d8189.mp3  Molly Malone

Ok, the link can be used to download the file using wget, but vkq offers
some renaming options, so lets use the latter instead:

    $ vkq music -o . 911727_183449471 911727_192006572
    911727_183449471
    Polite Dance Song
    ./The Bird And The Bee - Polite Dance Song.mp3
    911727_192006572
    L'estasi Dell'oro (The Ecstasy Of Gold)
    ./Ennio Morricone - Lestasi Delloro The Ecstasy Of Gold.mp3

Polite dance song and Ecstasy of gold mp3s will appear in the current folder. vkq
allows user to call arbitrary API method. Here is the tempalte:

    $ vkq call METHOD_NAME ARG1=VAL1 ARG2=VAL2 ...

Responses will be returned asis, in JSON or XML format depending on a method.

VKHS library
============

Following example illustrates basic usage (please fill client\_id, email and
password with correct values):

    import Web.VKHS.Login
    import Web.VKHS.API

    main = do
        let client_id = "111111"
        let e = env client_id "user@example.com" "password" [Photos,Audio,Groups]
        (Right at) <- login e

        let user_of_interest = "222222"
        (Right ans) <- api e at "users.get" [
              ("uids",user_of_interest)
            , ("fields","first_name,last_name,nickname,screen_name")
            , ("name_case","nom")
            ]
        putStrLn ans

client\_id is an application identifier, provided by vk.com. Users receive it
after registering their applications after SMS confirmation. Registration form is 
located [here](http://vk.com/editapp?act=create).

Internally, library uses small curl-based HTTP automata and tagsoup for jumping
over relocations and submitting various 'Yes I agree' forms. Curl .so library is
required for vkhs to work. I am using curl-7.26.0 on my system.

Debugging
=========

To authenticate the user, vkhs acts like a browser: it analyzes html but fills
all forms by itself instead of displaying pages. Of cause, would vk.com change
html design, things stop working.

To deal with that potential problem, Ive included some debugging facilities:
changing:

writing

    (Right at) <- login e { verbose = Debug }

will trigger curl output plus html dumping to the current directory. Please,
mail those .html to me if problem appears.

Limitations
===========

* 'Invalid password' answers are ignored (on TODO list)
* Captchas are treated as errors
* Implicit-flow authentication, see [documentation in
  Russian](http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений)
  for details
* Probably, low speed due to restarting curl session on every request. But
  anyway, vk.com limits request rate to 3 per second.

License
=======

BSD3 license

Copyright (c) 2014, Sergey Mironov <grrwlf@gmail.com>

[1]: http://vk.com

