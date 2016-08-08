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


Log in
------

Here is an example session: Login first

    ]$ vkq login user@mail.org pass123
    d8a41221616ef5ba19537125dc0349bad9d529fa15314ad765911726fe98b15185ac41a7ca2c62f3bf4b9

VKQ returns three values. First is a access token which is required to execute
future API requests. VKQ reads it from VKQ\_ACCESS\_TOKEN environment variable so
we have to set it up

    $ export VKQ_ACCESS_TOKEN=d785932b871f096bd73aac6a35d7a7c469dd788d796463a871e5beb5c61bc6c96788ec2

Download audio
--------------

Now, lets list first 10 audio files I have:

    $ vkq music -l | head -n 10
    910727_456939044 http://cs1-29v4.vk-cdn.net/p36/d36d0fac4baf0d.mp3 Алёнушка 1989 муз Никиты Богословского - ст Александра Коваленкова
    910727_456939043 http://cs1-35v4.vk-cdn.net/p7/2629cc1c9e82d7.mp3 Первое апреля
    910727_456939042 http://cs5078.vk.me/u39450508/audios/b65093a7caea.mp3 Травы травы травы не успели от росы серебрянной согнуться и такие нежные напевы почему-то прямо в сердце льются
    910727_456939041 http://cs1-35v4.vk-cdn.net/p12/423bca91340edc.mp3 Moving On
    910727_456939038 http://cs1-37v4.vk-cdn.net/p5/23c658ff1d9a43.mp3  Не для меня придёт весна
    910727_456939037 http://cs1-17v4.vk-cdn.net/p4/e67571789b026e.mp3 Каждый выбирает для себя
    910727_456939034 http://cs536114.vk.me/u262757964/audios/8b0e36ee4ad5.mp3 Black Fortress Kings Bounty Dark Side OST
    910727_456939031 http://cs613118.vk.me/u911727/audios/7bd0a650905e.mp3 Вокализ минус пример
    910727_456939040 http://cs611628.vk.me/u911727/audios/d5a8eff365aa.mp3  Без названия
    910727_456939028 http://cs536217.vk.me/u64604046/audios/a4ab2075af94.mp3 The Extasy of Gold

Ok, the link can be used to download the file using wget, but vkq offers
some renaming options, so lets use the latter instead:

    $ vkq music -o . 910727_456939043 910727_456939031
    910727_456939043
    Polite Dance Song
    ./The Bird And The Bee - Polite Dance Song.mp3
    910727_456939031
    L'estasi Dell'oro (The Ecstasy Of Gold)
    ./Ennio Morricone - Lestasi Delloro The Ecstasy Of Gold.mp3

Polite dance song and Ecstasy of gold mp3s will appear in the current folder.

Custom API calls
----------------

vkq allows user to call arbitrary API method. The format is as follows:

    Usage: vkq call [--verbose] [--req-per-sec N] [--interactive] [--appid APPID]
                    [--user USER] [--pass PASS] [-a ACCESS_TOKEN] METHOD PARAMS


For example, lets call ausio.search method to get some Beatles records:

    $ vkq call audio.search q=Beatles --pretty

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


VKHS library
============

Please, consult the source code of the vkq application.

*Note: Below is the contents of outdated section*

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

`RepatedForm` message means that VKHS tries to fill the web form with available
data, but the form appears again. Typically, that means that the password wa
invalid or captcha is required.

*Note: Below is the contents of outdated section*

To authenticate the user, vkhs acts like a browser: it analyzes html but fills
all forms by itself instead of displaying pages. Of cause, would vk.com change
html design, things stop working.

To deal with that potential problem, Ive included some debugging facilities:
changing:

writing

    (Right at) <- login e { verbose = Debug }

will trigger curl output plus html dumping to the current directory. Please,
mail those .html to me if problem appears.

TODO
====
* Decrypt 'RepeatedForm' errors
* Show capchas to users if required

Limitations
===========
* Implicit-flow authentication, see [documentation in
  Russian](http://vk.com/developers.php?oid=-1&p=Авторизация_клиентских_приложений)
  for details

License
=======

BSD3 license

Copyright (c) 2014, Sergey Mironov <grrwlf@gmail.com>

[1]: http://vk.com

