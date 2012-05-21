-module(extract).
%-export([start/1]).
-compile([export_all]).

-define(STEP, 8).
-define(CHUNKSIZE, 24576).
-define(FILENAME, "./olczak_iwona.P01").


% get breaks of zeroes
zeroes() ->
  zeroes(?FILENAME).
  
zeroes(Filename) ->
  {file, Bin, Size} = read_file(Filename),
  find_zeroes(Bin, Size, []).
  

read_file(Filename) ->
  {ok, Bin} = file:read_file(Filename),
  Size = filelib:file_size(Filename),
  {file, Bin, Size}.


% extract signals
start() ->
  start([
%    {"olczak_iwona.P01", "olczak_iwona", "signal"},
%    {"skrzycka_ewa.P01", "skrzycka_ewa", "signal"}
%    {"szaniawska_chydzinska_jadwiga.P01", "szaniawska_chydzinska_jadwiga", "signal"}
%    {"domagala_krystyna.P01", "domagala_krystyna", "signal"},
    {"00000252.P01", "hoppe_malgorzata", "signal"}
%    {"lechkun_malgorzata.P01", "lechkun_malgorzata", "signal"}
  ]).

start([]) -> ok;
start([{InFile, OutDir, OutFile} | List]) ->
  extract(InFile, OutFile, OutDir),
  start(List).

extract(Filename, OutName, OutDir) ->
  {file, Bin, Size} = read_file(Filename),
  Chs = find_chunks(Bin, Size, []),
  {_, Sizes} = lists:unzip(Chs),
  io:format("~w~n",[Sizes]),
  save(OutName, OutDir, Chs).

% separate values from binary chunk
decode(<<>>, Values) ->
  Values;
decode(<<Val:?STEP/signed, Sig/binary>>,Values) ->
  decode(Sig, [Val|Values]).


% find chunk which ends with 3008 zero bits
find_signal(Bin, N, MaxN, Zeroes) when N =< MaxN ->
  case Bin of
    <<Data:N/binary, 0:Zeroes, Rest/binary>> ->
      {found, Data, N, Rest};
    _ ->
      find_signal(Bin, N + 8, MaxN, Zeroes)
  end;
find_signal(_,Size,_, _) ->
  {not_found, Size}.
  

% find and separate chunks
find_chunks(<<>>, 0, Ch) ->
  lists:reverse(Ch);
find_chunks(Bin, Size, Ch) ->
  case find_signal(Bin, 0, Size, 2008) of
    {found, Data, DataSize, Rest} -> % regular chunk
      find_chunks(Rest, Size - DataSize, [{Data, DataSize}|Ch]);
    {not_found, S} -> %Size =:= S -> % just last chunk
      io:format("CHs: ~p, actual: ~p~n",[Size, S]),
      %io:format("~w~n", [decode(Bin, [])]),
      lists:reverse([{Bin, unknown}|Ch]);
    _ ->
      io:format("This shouldn't happen. Que pasa?~n")
  end.

% save all chunks to files in directory ./signals
save(BaseName, Dir, Chs) ->
%  io:format("Dir name: ~p~nPath: ~p~n", [Dir, BaseName]),
  Dir1 = lists:append("./", Dir),
%  ok = file:delete(Dir1),
  case file:make_dir(Dir1) of
    ok -> ok;
    {error, eexist} -> ok;
    {error, Error} -> io:format("Unable to create directory, because: ~p~n", [Error])
  end,
  save_signals(lists:append([Dir, "/", BaseName]), Chs, 0).


save_signals(BaseName, [], N) ->
  io:format("Name: ~p, num: ~p~n", [BaseName, N]),
  ok;
save_signals(BaseName, [{Ch, Size}|Chs], N) ->
  case Size of
    0 ->
      save_signals(BaseName, Chs, N);
    _ ->  
      %io:format("Path: ~p~n", [lists:append(BaseName,integer_to_list(N))]),
      ok = file:write_file(lists:append(BaseName,integer_to_list(N)), Ch),
      save_signals(BaseName, Chs, N + 1)
  end.

% find groups of zeroes
find_zeroes(<<>>, 0, Found) ->
  lists:reverse(Found);
find_zeroes(Bin, Size, Found) ->
  {found, _Data, DataSize, Rest1} = find_signal(Bin, 0, Size, 2 * ?STEP),
  {zeroes, Zeroes, Rest2} = get_zeroes(Rest1, 0),
  find_zeroes(Rest2, Size - DataSize - 2 * ?STEP - Zeroes, [2 * ?STEP + Zeroes | Found]).
  
% find how many zeroes are in row
get_zeroes(Bin, N) ->
  case Bin of
    <<0:N, 0:8, _Rest/binary>> ->
      get_zeroes(Bin, N + 8);
    <<0:N, Rest/binary>> ->
      {zeroes, N, Rest}
  end.
