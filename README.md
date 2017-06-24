Simple write-only web server
============================

Allows to write a file with HTTP PUT, and retrieve it back with HTTP GET. That's all it does.
File could be written only once to the same URI.

Why?
====

To use with FreeSWITCH `mod_http_cache`, as one example.

