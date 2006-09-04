%%
%% Based on sdp.erl in Yxa
%%

-module(sdp).
-export([parse/1,
	 print/1
	]).

-record(sdp,{origin, session_name, connection, attributes=[], media=[]}).
-record(sdp_origin, {username, session_id, version, network_type, address_type,
	    address}).
-record(sdp_media,{media, port, transport, fmts, connection, attributes=[]}).
-record(sdp_connection,{network_type, address_type, address}).

parse(String) ->
    Packetfixed = siputil:linefix(String),
    Lines = string:tokens(Packetfixed, "\n"),
    Parseheader = fun(Line) ->
			  parseline(Line)
		  end,
    Headerlist = lists:map(Parseheader, Lines),
    parse_list(Headerlist, #sdp{}).

parse_list([{v, Header}|R], Sdp) ->
    case Header of
	"0" ->
	    parse_list(R, Sdp);
	_ ->
	    {error, unsupported_version, Header}
    end;
parse_list([{o, Header}|R], Sdp) ->
    {ok, Origin} = parse_origin(Header),
    parse_list(R, Sdp#sdp{origin=Origin});
parse_list([{s, Header}|R], Sdp) ->
    parse_list(R, Sdp#sdp{session_name=Header});
parse_list([{c, Header}|R], Sdp) ->
    {ok, Connection} = parse_connection(Header),
    parse_list(R, Sdp#sdp{connection=Connection});
parse_list([{t, Header}|R], Sdp) ->
    %% Skip
    parse_list(R, Sdp);
parse_list([{a, Header}|R], Sdp) ->
    {ok, Attrib, Value} = parse_attribute(Header),
    Attributes=[{Attrib, Value}|Sdp#sdp.attributes],
    parse_list(R, Sdp#sdp{attributes=Attributes});
parse_list([{m, Header}|R], Sdp) ->
    {ok, Media} = parse_media(Header, R, Sdp, #sdp_media{}),
    Media_list = Sdp#sdp.media ++ [Media],
    {ok, Sdp#sdp{media=Media_list}}.

parse_origin(Header) ->
    Values = string:tokens(Header, " "),
    [Username, Session_id, Version, Network_type, Address_type, Address] = Values,
    {ok, #sdp_origin{username=Username,
		     session_id=Session_id,
		     version=Version,
		     network_type=list_to_atom(Network_type),
		     address_type=list_to_atom(Address_type),
		     address=Address}}.

parse_media(Header, R, Sdp, Sdp_media) ->
    [Media, Port, Transport | Fmts] = string:tokens(Header, " "),
    FmtList = lists:map(fun (F) -> list_to_integer(F) end, Fmts),
    parse_media(R, Sdp, Sdp_media#sdp_media{media=list_to_atom(Media),
					    port=list_to_integer(Port),
					    transport=Transport,
					    fmts=FmtList}).
    
parse_media([{c, Header}|R], Sdp, Sdp_media) ->
    {ok, Conn} = parse_connection(Header),
    parse_media(R, Sdp, Sdp_media#sdp_media{connection=Conn});
parse_media([{a, Header}|R], Sdp, Sdp_media) ->
    {ok, Attrib, Value} = parse_attribute(Header),
    Attributes=[{Attrib, Value}|Sdp_media#sdp_media.attributes],
    parse_media(R, Sdp, Sdp_media#sdp_media{attributes=Attributes});
parse_media(R, Sdp, Sdp_media) ->
    {ok, Sdp_media}.

parse_connection(Header) ->
    [Network_type, Address_type, Address] = string:tokens(Header, " "),
    {ok, #sdp_connection{network_type=list_to_atom(Network_type),
			 address_type=list_to_atom(Address_type),
			 address=Address}}.

parse_attribute(Header) ->
    parse_attribute(Header, []).

parse_attribute([], Attrib) ->
    {ok, list_to_atom(Attrib), flag};
parse_attribute([$:|R], Attrib) ->
    {ok, list_to_atom(Attrib), R};
parse_attribute([C|R], Attrib) when C =/= ":" ->
    parse_attribute(R, Attrib ++ [C]).


parseline([C, $= | Value]) ->
    {list_to_atom([C]), Value};
parseline(_) ->
    none.


print(Sdp) when is_record(Sdp, sdp) ->
    ["v=0\r\n"] ++
	print_origin(Sdp#sdp.origin) ++
	print_item(s, Sdp#sdp.session_name) ++
	print_connection(Sdp#sdp.connection) ++
	[print_attrib(X) || X <- Sdp#sdp.attributes] ++
	["t=0 0\r\n"] ++
	[print_media(X) || X <- Sdp#sdp.media].


print_item(Type, Item) ->
    [[atom_to_list(Type), $=, Item, "\r\n"]].

print_origin(Origin) when is_record(Origin, sdp_origin) ->
    [["o=",
     Origin#sdp_origin.username, $\ ,
     Origin#sdp_origin.session_id, $\ ,
     Origin#sdp_origin.version, $\ ,
     atom_to_list(Origin#sdp_origin.network_type), $\ ,
     atom_to_list(Origin#sdp_origin.address_type), $\ ,
     Origin#sdp_origin.address, "\r\n"]];
print_origin(undefined) ->
    [].

print_media(Media) when is_record(Media, sdp_media) ->
    ["m=",
     atom_to_list(Media#sdp_media.media), $\ ,
     integer_to_list(Media#sdp_media.port), $\ ,
     Media#sdp_media.transport, $\ ,
     [[print_formats(F), $\ ] || F <- Media#sdp_media.fmts], "\r\n"] ++
	print_connection(Media#sdp_media.connection) ++
	[print_attrib(A) || A <- Media#sdp_media.attributes];
print_media(undefined) ->
    [].

print_connection(Connection) when is_record(Connection, sdp_connection) ->
    [["c=",
     atom_to_list(Connection#sdp_connection.network_type), $\ ,
      atom_to_list(Connection#sdp_connection.address_type), $\ ,
     Connection#sdp_connection.address, "\r\n"]];
print_connection(undefined) ->
    [].

print_attrib({Attrib, flag}) ->
    ["a=", atom_to_list(Attrib), "\r\n"];
print_attrib({Attrib, Value}) ->
    ["a=", atom_to_list(Attrib), $:, Value, "\r\n"].

print_formats(Format) ->
    [integer_to_list(Format)].
