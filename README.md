# Erlang Raptor
Erlang bindings for the Raptor RDF Parser Library
http://librdf.org/raptor/

## warning
This software is at a very early alpha stage. So far only very few features of the raptor library are implemented. There is also no error handling so far.

## build
1. install raptor2
2. (OS X) if `pkg-config raptor2 --libs` gives a libcurl.pc error export PKG_CONFIG_PATH with homebrew pkgconfig environment `/usr/local/Library/ENV/pkgconfig/10.8`
3. compile library ``./rebar compile``

## usage
```erlang
application:start(raptor).
raptor:parse_uri("http://example.com").
application:stop(raptor).
```

## License
This software is licensed under the Apache License Version 2.0.
See http://www.apache.org/licenses/LICENSE-2.0 or the LICENSE file.

Copyright (C) 2013 Jonas Pollok
