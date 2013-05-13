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
start and stop application as usual
```erlang
application:start(raptor).
application:stop(raptor).
```

parsing URIs
```erlang
raptor:parse_uri(Uri, Format).
raptor:parse_uri(Uri) -> raptor:parse_uri(Uri, default)
```
where Format can be
```erlang
default      % default (RDF/XML) 
rdfxml       % RDF/XML (default)
ntriples     % N-Triples
turtle       % Turtle Terse RDF Triple Language
trig         % TriG - Turtle with Named Graphs
rss_tag_soup % RSS Tag Soup
grddl        % Gleaning Resource Descriptions from Dialects of Languages
guess        % Pick the parser to use using content type and URI
rdfa         % RDF/A via librdfa
nquads       % N-Quadsd
```

## License
This software is licensed under the Apache License Version 2.0.
See http://www.apache.org/licenses/LICENSE-2.0 or the LICENSE file.

Copyright (C) 2013 Jonas Pollok
