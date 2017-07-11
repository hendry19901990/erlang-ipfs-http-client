# IPFS API wrapper library in Erlang

## Install

Simply clone this repo.

## Quickstart

```
%% Add Enpoint /api/v0/add
Method: ipfs:add("ip-address", "port", "fullpathFile", "nameFile.extension").
> ipfs:add("127.0.0.1", "5001", "test.txt", "test.txt").
{[{<<"Name">>,<<"test.txt">>},
  {<<"Hash">>,
   <<"QmYAVSgbjdw1hb96aFev6inrnqYntJLboeg8mumBvuamMe">>}]}

%% Cat Enpoint /api/v0/cat
Method: ipfs:add("ip-address", "port", "hash").
>  ipfs:cat("127.0.0.1", "5001", "QmYAVSgbjdw1hb96aFev6inrnqYntJLboeg8mumBvuamMe").
"Hello World"

%% Cat Enpoint /api/v0/ls
Method: ipfs:ls("ip-address", "port", "hash").
>  ipfs:ls("127.0.0.1", "5001", "QmaaqrHyAQm7gALkRW8DcfGX3u8q9rWKnxEMmf7m9z515w").
{[{<<"Objects">>,
   [{[{<<"Hash">>,
       <<"QmaaqrHyAQm7gALkRW8DcfGX3u8q9rWKnxEMmf7m9z515w">>},
      {<<"Links">>,
       [{[{<<"Name">>,<<"index.html">>},
          {<<"Hash">>,
           <<"QmYftndCvcEiuSZRX7njywX2AGSeHY2ASa7VryCq1mKwEw">>},
          {<<"Size">>,1700},
          {<<"Type">>,2}]},
        {[{<<"Name">>,<<"static">>},
          {<<"Hash">>,
           <<"QmdtWFiasJeh2ymW3TD2cLHYxn1ryTuWoNpwieFyJriGTS">>},
          {<<"Size">>,2428803},
          {<<"Type">>,1}]}]}]}]}]}
          
%% Size Enpoint /api/v0/size
Method: ipfs:ls("ip-address", "port", "hash").
>  ipfs:size("127.0.0.1", "5001", "QmaaqrHyAQm7gALkRW8DcfGX3u8q9rWKnxEMmf7m9z515w").   
{[{<<"Hash">>,
   <<"QmaaqrHyAQm7gALkRW8DcfGX3u8q9rWKnxEMmf7m9z515w">>},
  {<<"NumLinks">>,2},
  {<<"BlockSize">>,108},
  {<<"LinksSize">>,106},
  {<<"DataSize">>,2},
  {<<"CumulativeSize">>,2430611}]}
```

## License

The MIT License (MIT)

Copyright (c) 2017 Hendry Rodriguez

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
