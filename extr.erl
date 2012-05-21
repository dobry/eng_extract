-module(extr).
-compile([export_all]).

%%% it seems that there is nothing interesting in NG.IDX files, so you can ignore next three funs. I leave this part in case it is helpful in the future
start() ->
	{ok, Bin} = file:read_file("GRPTMP.GRP/NG.IDX"),
	loop(Bin, 0).

loop(Bin, Licz) ->
	<<V:8, Rest/binary>> = Bin,
	io:format("'~c', '~w'~n", [V, V]),
	case io:get_chars("",1) of
		"a" -> ok;
		"i" ->
			Rest1 = get_int(Bin),
			io:format("ilosc: ~w~n", [Licz]),
			loop(Rest1, 0);
		_ -> loop(Rest, Licz + 1)
	end.

get_int(<<V:16/little-integer, A:16/little-integer, Rest/binary>>) ->
	io:format("'~w', '~w'~n", [V, A]),
	Rest.

%%% parse all suplied files
st1() ->
  {ok, Files} = file:list_dir("data/"),
  io:format("~p~n", [Files]),
  st1(Files).
  
st1(Names) ->
  lists:map(fun st/1, Names).

  

%%% parse heading of the file, extract signals, save them to separate files
st() ->
  st("szaniawska_chydzinska_jadwiga.P01").

st(FileName) ->
	{ok, Bin} = file:read_file(lists:append("data/", FileName)),
	Coords = parse_head(Bin),
  %io:format("~p~n", [Coords]),
	Signals = extract_signals(Bin, Coords, []),
	{saved, Num} = save(Signals, lists:append("extracted/", filename:basename(FileName, ".P01"))),
	io:format("File: ~s, Entries: ~p, ~p~n", [FileName, lists:foldl(fun (_, Acc) -> Acc + 1 end, 0, Signals), Num]),
	print_sigs(Signals).

% print info about signals
print_sigs([]) ->
  ok;
print_sigs([{Name, Sig} | Ss]) ->
  io:format("Name: ~s, Bytes: ~p~n", [Name, byte_size(Sig)]),
  print_sigs(Ss).

% get Bin, parse informations at the start
parse_head(Bin) ->
	{ok, Pos} = find(Bin, <<"KALIBRACJA">>, 0),
	%io:format("Got pos: ~pB~n", [Pos/8]),
	{ok, Coords} = get_coordinates(Bin, Pos, []),
	%io:format("From: ~w :tO~n", [Coords]).
	calc_correct_lengths(Coords, []).

% find Str in Bin, start at position Pos
find(Bin, Str, Pos) ->
	<<_:Pos, Val:80/bitstring, _Rest/binary>> = Bin,
	case Val of
		Str	->
			{ok, Pos};
		_ ->
			find(Bin, Str, Pos + 8)
	end.

% get info about signals. look for it in Bin, at position Start, accumulate in 
% Coords in tuples {name: Name, start position: Pos, length: Len}
get_coordinates(Bin, Start, Coords) ->
	case Bin of
	  <<_:Start, Name:21/binary, % 21 bajts reserved name of the signal
		Pos:32/little-integer, % start position of the signal
		Len:32/little-integer, % length of the signal
		_Sep:8, _Rest/binary>> % separator sign
		when (Len > 0) ->
      Name1 = get_name(Name),
      get_coordinates(Bin, Start + 30 * 8, [{Name1, Pos, Len}| Coords]);
    _ ->
      {ok, lists:reverse(Coords)}
	end.

% extract name from binary, repair encoding and prepare it to be safe for filename 
get_name(BinName) ->
  [trim_char(X) || X <- trim_spaces(binary_to_list(BinName),[]), X =/= 0].

% change strangely encoded characters, also change spaces ' ' to underscores '_'
trim_char(C) ->
  case C of
    128 -> 65;  %'Ą' -> 'A'
    130 -> 69;  %'Ę' -> 'E'
    32 -> 95;   %' ' -> '_'
    131 -> 76;  %'Ł' -> 'L'
    _ -> C
  end.

% trim surplus spaces
trim_spaces([B | []], Trimmed) ->
  lists:reverse(case B of
    32 ->
      Trimmed;
    _ ->
      [B | Trimmed]
  end);
trim_spaces([A, B | List], Trimmed) when ((A =:= 32) and  (B =:= 32)) ->
  trim_spaces([B | List], Trimmed);
trim_spaces([A | List], Trimmed) ->
  trim_spaces(List, [A | Trimmed]).

% extract signals from binary Bin, given list of positions Pos and lengths Len
extract_signals(Bin, [{Name, Pos, Len}], Signals) ->
  io:format("Tuuuuu: S: ~p, P: ~p, L: ~p~n", [byte_size(Bin), Pos, Len]),
  <<_:Pos/binary, Signal/binary>> = Bin,
  lists:reverse([{Name, Signal} | Signals]);
extract_signals(Bin, [{Name, Pos, Len} | Coords], Signals) ->
  io:format("~s, ~.16# ~.16# ~.16#~n", [Name, Pos, Len, Pos + Len]),
  <<_:Pos/binary, Signal:Len/binary, _Rest/binary>> = Bin,
  extract_signals(Bin, Coords, [{Name, Signal} | Signals]).

% create directory if needed, then save signals to separate files
save(Signals, Dir) ->
  case file:make_dir(Dir) of
    ok -> ok;
    {error, eexist} -> ok;
    {error, Error} -> io:format("Unable to create directory, because: ~p~n", [Error])
  end,
  save_signals(Signals, lists:append(Dir, "/"), 1).

% for every tuple in Signals save signal Sig to file named Name in directory Dir
% N enumerates files to assure uniqueness of the names
save_signals([], _, N) ->
  {saved, N - 1};
save_signals([{Name, Sig} | Signals], Dir, N) ->
  FilePath = io_lib:format("~s~2..0w.~s", [Dir, N, Name]),
  ok = file:write_file(FilePath, Sig),
  save_signals(Signals, Dir, N + 1).

% calculates real length of a signal by substracting positions of two adjacent signals
calc_correct_lengths([Coords], Correct) ->
  lists:reverse([Coords | Correct]);
calc_correct_lengths([{Name1, Pos1, _Len1}, {Name2, Pos2, _Len2} | Coords], Correct) ->
  calc_correct_lengths([{Name2, Pos2, 0} | Coords], [{Name1, Pos1, Pos2 - Pos1} | Correct]).
