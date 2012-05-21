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
  st("hoppe_malgorzata.P01").

st(Filename) ->
	{ok, Bin} = file:read_file(Filename),
	parse_head(Bin).

% get Bin, parse informations at the start
parse_head(Bin) ->
	{ok, Pos} = find(Bin, <<"KALIBRACJA">>, 0),
	io:format("Got pos: ~pB~n", [Pos/8]),
	{ok, Coords} = get_coordinates(Bin, Pos, []),
	io:format("From: ~w :tO~n", [Coords]).

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

