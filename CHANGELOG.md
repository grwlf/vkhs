Version 1.9.1
-------------
* Fixed user photo uploading using `vkq photo`

Version 1.9
-----------

* Disable Audio API due to upstream changes
* Handle re-login conditions
* Handle too-many-requests condition
* Stop re-exporting `Prelude`, `Imports` provides aliases for popular
  `Data.Text` functions
* Cache access token in file
* Minor fixes

Version 1.7.2
-------------

* Initial support for runhaskell mode

Version 1.7.1
-------------

* Support setting IDv2 tags for downloaded audio files
* CLI: Fix 'call' command output
* CLI: Fix 'music' command naming


Version 1.6.0
-------------

* Use coroutine-based engine, offering smooth error handling
* Use pipes-http as network backend

