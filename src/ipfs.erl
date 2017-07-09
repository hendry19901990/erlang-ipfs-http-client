%%% @doc ipfs module
%%% @private
%%% @end
%%%
%%% Copyright (c) 2017, Hendry Rodriguez
%%%
%%% The MIT License
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(ipfs).
-compile(export_all).
-import(string,[len/1]).
-define(BOUNDARY, "------------e897glvjfEoq").


add(IP, PORT, PathFile, FileName)->
    URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/add",
    {ok, Cont} = file:read_file(PathFile), 
    Data = binary_to_list(Cont),
    Boundary = ?BOUNDARY,
    Body = format_multipart_formdata(Boundary, [{ FileName, Data}]),
    ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
    Headers = [{"Content-Length", integer_to_list(length(Body))}],
    request(post, {URL, Headers, ContentType, Body}).
    

cat(IP, PORT, HASH) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/cat?arg=" ++ HASH,
  inets:start(),
    ssl:start(),
    R = httpc:request(get, {URL, []}, [{ssl,[{verify,0}]}], []),
    {_, {_, _, R1}} = R,
    R1.


ls(IP, PORT, HASH) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/ls/" ++ HASH,
  request(get, {URL, []}). 


size(IP, PORT, HASH) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/object/stat/" ++ HASH,
  request(get, {URL, []}). 


pinAdd(IP, PORT, HASH) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/pin/add/" ++ HASH,
  request(get, {URL, []}). 


id(IP, PORT, ID) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/id",
  case len(ID) of
  	0 ->
  	  request(get, {URL, []});
  	_ -> 
  	  P = URL ++ "/" ++ ID,
  	  request(get, {P, []})
  end.
id(IP, PORT) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/id",
  request(get, {URL, []}).


version(IP, PORT) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/version",
  request(get, {URL, []}).


bitswap_ledger(IP, PORT, HASH) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/bitswap/ledger?arg=" ++ HASH,
  request(get, {URL, []}).

bitswap_stat(IP, PORT) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/bitswap/stat",
  request(get, {URL, []}).

bitswap_unwant(IP, PORT, HASH) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/bitswap/unwant?arg=" ++ HASH,
  request(get, {URL, []}).

bitswap_wantlist(IP, PORT, HASH) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/bitswap/wantlist?peer=" ++ HASH,
  request(get, {URL, []}).


block_get(IP, PORT, HASH) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/block/get?arg=" ++ HASH,
  request(get, {URL, []}).

block_put(IP, PORT, PathFile, FileName)->
    URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/block/put?format=v0&mhtype=sha2-256&mhlen=-1",
    {ok, Cont} = file:read_file(PathFile), 
    Data = binary_to_list(Cont),
    Boundary = ?BOUNDARY,
    Body = format_multipart_formdata(Boundary, [{ FileName, Data}]),
    ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
    Headers = [{"Content-Length", integer_to_list(length(Body))}],
    request(post, {URL, Headers, ContentType, Body}).

block_rm(IP, PORT, HASH) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/block/rm?arg=" ++ HASH ++ "&force=false&quiet=false",
  request(get, {URL, []}).

block_stat(IP, PORT, HASH) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/block/stat?arg=" ++ HASH,
  request(get, {URL, []}).


bootstrap_add_default(IP, PORT) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/bootstrap/add/default",
  request(get, {URL, []}).

bootstrap_list(IP, PORT) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/bootstrap/list",
  request(get, {URL, []}).  

bootstrap_rm_all(IP, PORT) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/bootstrap/rm/all",
  request(get, {URL, []}).  


commands(IP, PORT) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/commands?flags=false",
  request(get, {URL, []}). 


dag_get(IP, PORT, Key) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/dag/get?arg=" ++ Key,
  request(get, {URL, []}).

dag_put(IP, PORT, PathFile, FileName)->
    URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/dag/put?format=cbor&input-enc=json",
    {ok, Cont} = file:read_file(PathFile), 
    Data = binary_to_list(Cont),
    Boundary = ?BOUNDARY,
    Body = format_multipart_formdata(Boundary, [{ FileName, Data}]),
    ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
    Headers = [{"Content-Length", integer_to_list(length(Body))}],
    request(post, {URL, Headers, ContentType, Body}).


dht_findpeer(IP, PORT, Peerid) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/dht/findpeer?arg=" ++ Peerid ++ "&verbose=false",
  request(get, {URL, []}).

dht_findprovs(IP, PORT, Key) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/dht/findprovs?arg=" ++ Key ++ "&verbose=false",
  request(get, {URL, []}).

dht_get(IP, PORT, Key) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/dht/get?arg=" ++ Key ++ "&verbose=false",
  request(get, {URL, []}).

dht_provide(IP, PORT, Key) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/dht/provide?arg=" ++ Key ++ "&verbose=false&recursive=false",
  request(get, {URL, []}).

dht_put(IP, PORT, Key, Value) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/dht/put?arg=" ++ Key ++ "&arg=" ++ Value ++  "&verbose=false",
  request(get, {URL, []}).

dht_query(IP, PORT, Peerid) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/dht/query?arg=" ++ Peerid ++ "&verbose=false",
  request(get, {URL, []}).


diag_cmds_clear(IP, PORT) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/diag/cmds/clear",
  request(get, {URL, []}). 

diag_cmds_settime(IP, PORT, Time) ->
  URL = "http://" ++ IP ++ ":" ++ PORT ++ "/api/v0/diag/cmds/set-time?arg=" ++ Time,
  request(get, {URL, []}).


format_multipart_formdata(Boundary,  Files) ->
    FileParts = lists:map(fun({FileName, FileContent}) ->
                                  [lists:concat(["--", Boundary]),
                                   lists:concat(["Content-Disposition: file; name=\"","path","\"; filename=\"",FileName,"\""]),
                                   lists:concat(["Content-Type: ", "application/octet-stream"]),
                                   "",
                                   FileContent]
                          end, Files),
    FileParts2 = lists:append(FileParts),
    EndingParts = [lists:concat(["--", Boundary, "--"]), ""],
    Parts = lists:append([ FileParts2, EndingParts]),
    string:join(Parts, "\r\n").

request(Method, Request) ->
    inets:start(),
    ssl:start(),
    R = httpc:request(Method, Request, [{ssl,[{verify,0}]}], []),
    response(R).

response(Response) ->
   {I1, _} =  Response,
   case I1 of
     ok -> {_, {_, _, R1}} = Response,
           B1   = binary:list_to_bin([R1]),
           Resp = jsone:decode(B1, [{object_format, tuple}]),
           Resp;
     _ -> error
   end.
