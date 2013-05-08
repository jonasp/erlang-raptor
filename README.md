# Erlang Raptor
Erlang bindings for the Raptor RDF Parser Library
http://librdf.org/raptor/

## warning
This software is at a very early alpha stage. So far only very few features of the raptor library are implemented. There is also no error handling so far.

## build
### Linux
1. install libraptor2
2. compile library ``gcc -o raptor_drv.so -fpic -shared `pkg-config raptor2 --cflags --libs` raptor_drv.c``

### OS X
1. install raptor2 with homebrew
2. if `pkg-config raptor2 --libs` gives a libcurl.pc error export PKG_CONFIG_PATH with homebrew pkgconfig environment `/usr/local/Library/ENV/pkgconfig/10.8`
3. compile library 

```
gcc -o raptor_drv.so -undefined dynamic_lookup -fpic -shared
-I/usr/local/Cellar/erlang/R15B03-1/lib/erlang/usr/include
`pkg-config raptor2 --cflags --libs` raptor_drv.c
```

## usage
```erlang
raptor:start().
raptor:parse_uri("http://example.com").
raptor:stop().
```

## License
This software is licensed under the Apache License Version 2.0.
See http://www.apache.org/licenses/LICENSE-2.0 or the LICENSE file.

Copyright (C) 2013 Jonas Pollok
