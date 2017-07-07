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
  request(get, {URL, []}).


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
    httpc:request(Method, Request, [], []).

