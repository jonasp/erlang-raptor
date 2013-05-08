# Erlang Raptor
Erlang bindings for the Raptor RDF Parser Library
http://librdf.org/raptor/

## Linux
1. install libraptor2
2. compile library ``gcc -o raptor_drv.so -fpic -shared `pkg-config raptor2 --cflags --libs` port_driver.c``

## OS X
1. install raptor2 with homebrew
2. if `pkg-config raptor2 --libs` gives a libcurl.pc error export PKG_CONFIG_PATH with homebrew pkgconfig environment `/usr/local/Library/ENV/pkgconfig/10.8`
3. compile library 

```
gcc -o raptor_drv.so -undefined dynamic_lookup -fpic -shared
-I/usr/local/Cellar/erlang/R15B03-1/lib/erlang/usr/include
`pkg-config raptor2 --cflags --libs` port_driver.c
```

## usage
```erlang
raptor:start().
raptor:parse_uri("http://example.com").
raptor:stop().
```
