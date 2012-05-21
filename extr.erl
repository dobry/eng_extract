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


%%% parse heading of the file
st() ->
  st("data/hoppe_malgorzata.P01").

st(FilePath) ->
	{ok, Bin} = file:read_file(FilePath),
	Coords = parse_head(Bin),
	Signals = extract_signals(Bin, Coords, []),
	print_sigs(Signals),
	save(Signals, "extracted").

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
	Coords.

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
	<<_:Start, Name:21/binary, % 21 bajts reserved name of the signal
		Pos:16/little-integer, % start position of the signal
		_Something1:16/little-integer, % who knows what is it?
		Len:16/little-integer, % length of the signal
		_Something2:16/little-integer, % once again unknown value
		Sep:8, _Rest/binary>> = Bin, % separator sign
  Name1 = get_name(Name),
	io:format("Entry: '~s', Pos: '~p', Len: '~p', Sep: '~p'~n", [Name1, Pos, Len, Sep]),
	case Sep of
		0 -> 
			{ok, lists:reverse(Coords)};
		_ -> %Start + 30 * 8
			get_coordinates(Bin, Start + 30 * 8, [{Name1, Pos, Len}| Coords]) 
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
extract_signals(_, [], Signals) ->
  Signals;
extract_signals(Bin, [{Name, Pos, Len} | Coords], Signals) ->
  <<_:Pos/binary, Signal:Len/binary, _Rest/binary>> = Bin,
  extract_signals(Bin, Coords, [{Name, Signal} | Signals]).

% create directory if needed, then save signals to separate files
save(Signals, Dir) ->
  case file:make_dir(Dir) of
    ok -> ok;
    {error, eexist} -> ok;
    {error, Error} -> io:format("Unable to create directory, because: ~p~n", [Error])
  end,
  save_signals(Signals, lists:append(Dir, "/")).

% for every tuple in Signals save signal Sig to file named Name in directory Dir
save_signals([], _) ->
  ok;
save_signals([{Name, Sig} | Signals], Dir) ->
  ok = file:write_file(lists:append(Dir, Name), Sig),
  save_signals(Signals, Dir).
