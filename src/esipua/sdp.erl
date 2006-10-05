%%
%% Based on sdp.erl in Yxa
%%
%%
%% Doesn't support multiple t and r headers.
%%

-module(sdp).

-include("sdp.hrl").

-export([parse/1,
	 print/1
	]).

-export([test/0, test_sdp/0]).

parse([]) ->
    {error, empty};
parse(String) when is_list(String) ->
   case catch do_parse(String) of
       {ok, Sdp}=Res ->
	   Res;
       {'EXIT', Reason} ->
	   {error, Reason}
   end.

do_parse(String) when is_list(String) ->
    Packetfixed = siputil:linefix(String),
    Lines = string:tokens(Packetfixed, "\n"),
    Parseheader = fun(Line) ->
			  parseline(Line)
		  end,
    Headerlist = lists:map(Parseheader, Lines),
    Groups = headers_to_mediagroup(Headerlist),
    parse_groups(Groups).


headers_to_mediagroup(Headerlist) ->
    Fun = fun(Line, {List, Sub_list}) ->
		  case Line of
		      {m, Header} ->
			  List1 = [Sub_list|List],
			  Sub_list1 = [{m, Header}],
			  {List1, Sub_list1};
		      _ ->
			  Sub_list1 = [Line|Sub_list],
			  {List, Sub_list1}
		  end
	  end,
			  
    {List, Sub_list} = lists:foldl(Fun,{[],[]},Headerlist),
    [Sub_list|List].
    

%%     parse_list(Headerlist, #sdp{}).

parse_groups(Groups) ->
    parse_groups(Groups, []).

parse_groups([Group], Media_list) ->
    parse_session(Group, Media_list);
parse_groups([Group|R], Media_list) ->
    {ok, Media} = parse_media(Group),
    parse_groups(R, [Media|Media_list]).


parse_session(Group, Media_list) ->
    parse_list(Group, #sdp{media=Media_list}).

parse_list([], Sdp) ->
    {ok, Sdp};
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
parse_list([{c, Header}|R], Sdp) ->
    {ok, Connection} = parse_connection(Header),
    parse_list(R, Sdp#sdp{connection=Connection});
parse_list([{t, _Header}|R], Sdp) ->
    %% Skip
    parse_list(R, Sdp);
parse_list([{a, Header}|R], Sdp) ->
    {ok, Attrib, Value} = parse_attribute(Header),
    Attributes=[{Attrib, Value}|Sdp#sdp.attributes],
    parse_list(R, Sdp#sdp{attributes=Attributes});
parse_list([{s, Header}|R], Sdp) ->
    parse_list(R, Sdp#sdp{session_name=Header});
parse_list([{Type, Header}|R], Sdp) ->
    ExtraHeaders = [{Type, Header} | Sdp#sdp.extra],
    parse_list(R, Sdp#sdp{extra=ExtraHeaders}).

parse_origin(Header) ->
    Values = string:tokens(Header, " "),
    [Username, Session_id, Version, Network_type, Address_type, Address] = Values,
    {ok, #sdp_origin{username=Username,
		     session_id=Session_id,
		     version=Version,
		     network_type=list_to_atom(Network_type),
		     address_type=list_to_atom(Address_type),
		     address=Address}}.

parse_media(Group) ->
    parse_media(Group, undefined, #sdp_media{}).

parse_media([{m, Header}|R], Sdp, Sdp_media) ->
    [Media, Port, Transport | Fmts] = string:tokens(Header, " "),
    parse_media(R, Sdp, Sdp_media#sdp_media{media=list_to_atom(Media),
					    port=list_to_integer(Port),
					    transport=Transport,
					    fmts=Fmts});
    
parse_media([{c, Header}|R], Sdp, Sdp_media) ->
    {ok, Conn} = parse_connection(Header),
    parse_media(R, Sdp, Sdp_media#sdp_media{connection=Conn});
parse_media([{a, Header}|R], Sdp, Sdp_media) ->
    {ok, Attrib, Value} = parse_attribute(Header),
    Attributes=[{Attrib, Value}|Sdp_media#sdp_media.attributes],
    parse_media(R, Sdp, Sdp_media#sdp_media{attributes=Attributes});
parse_media(_R, _Sdp, Sdp_media) ->
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
    [Session | R] = build_headers(Sdp),
    
    Session_list = print_group(session, Session),
    Media_list = lists:map(fun(E) -> print_group(media, E) end, R),
    IOList = [Session_list | Media_list],
    lists:flatten(IOList).


sorted_types(session) ->
    [v, o, s, i, u, e, p, c, b, t, r, z, k, a ];
sorted_types(media) ->
    [m, i, c, b, k, a].

compare_types(Class, Type1, Type2) when is_atom(Type2) ->
    compare_types(Type1, Type2, sorted_types(Class));
compare_types(Type1, _Type2, [Type1|_R]) ->
    false;
compare_types(_Type1, Type2, [Type2|_R]) ->
    true;
compare_types(Type1, Type2, [_Type|R]) ->
    compare_types(Type1, Type2, R).

print_group(Class, Group) when is_atom(Class) ->
    Fun = fun({Type1, _Item1}, {Type2, _Item2}) ->
		  compare_types(Class, Type1, Type2)
	  end,
    Group1 = lists:sort(Fun, Group),
    print_group(Group1, []);
print_group([], Res) ->
    Res;
print_group([{Type, Item}|R], Res) ->
    Line = [atom_to_list(Type), $=, Item, "\r\n"],
    print_group(R, [Line|Res]).

build_headers(Sdp) when is_record(Sdp, sdp) ->
    [[{v, "0"}] ++
	print_origin(Sdp#sdp.origin) ++
 	[print_item(s, Sdp#sdp.session_name)] ++
	print_connection(Sdp#sdp.connection) ++
	[{t, "0 0"}] ++
	[print_attrib(X) || X <- Sdp#sdp.attributes] ++
	[print_item(Type, Item) || {Type, Item} <- Sdp#sdp.extra]] ++
	[print_media(X) || X <- Sdp#sdp.media].

print_item(Type, Item) ->
    {Type, Item}.

print_origin(Origin) when is_record(Origin, sdp_origin) ->
    [{o, [Origin#sdp_origin.username, $\ ,
     Origin#sdp_origin.session_id, $\ ,
     Origin#sdp_origin.version, $\ ,
     atom_to_list(Origin#sdp_origin.network_type), $\ ,
     atom_to_list(Origin#sdp_origin.address_type), $\ ,
     Origin#sdp_origin.address]}];
print_origin(undefined) ->
    [].

print_media(Media) when is_record(Media, sdp_media) ->
    [{m, [atom_to_list(Media#sdp_media.media), $\ ,
     integer_to_list(Media#sdp_media.port), $\ ,
     Media#sdp_media.transport, 
     [[$\ , print_formats(F) ] || F <- Media#sdp_media.fmts]]}] ++
	print_connection(Media#sdp_media.connection) ++
	[print_attrib(A) || A <- Media#sdp_media.attributes];
print_media(undefined) ->
    [].

print_connection(Connection) when is_record(Connection, sdp_connection) ->
    [{c, [atom_to_list(Connection#sdp_connection.network_type), $\ ,
      atom_to_list(Connection#sdp_connection.address_type), $\ ,
     Connection#sdp_connection.address]}];
print_connection(undefined) ->
    [].

print_attrib({Attrib, flag}) ->
    {a, [atom_to_list(Attrib)]};
print_attrib({Attrib, Value}) ->
    {a, [atom_to_list(Attrib), $:, Value]}.

print_formats(Format) when is_integer(Format) ->
    [integer_to_list(Format)];
print_formats(Format) when is_list(Format) ->
    Format.

test_str() ->

%%         v=0
%%         o=mhandley 2890844526 2890842807 IN IP4 126.16.64.4
%%         s=SDP Seminar
%%         i=A Seminar on the session description protocol
%%         u=http://www.cs.ucl.ac.uk/staff/M.Handley/sdp.03.ps
%%         e=mjh@isi.edu (Mark Handley)
%%         c=IN IP4 224.2.17.12/127

%%         t=2873397496 2873404696
%%         a=recvonly
%%         m=audio 49170 RTP/AVP 0
%%         m=video 51372 RTP/AVP 31
%%         m=application 32416 udp wb
%%         a=orient:portrait

%%     SdpStr = "v=0\r\no=mhandley 2890844526 2890842807 IN IP4 126.16.64.4\r\ns=SDP Seminar\r\ni=A Seminar on the session description protocol\r\nu=http://www.cs.ucl.ac.uk/staff/M.Handley/sdp.03.ps\r\ne=mjh@isi.edu (Mark Handley)\r\nc=IN IP4 224.2.17.12/127\r\nt=0 0\r\na=recvonly\r\nm=audio 49170 RTP/AVP 0\r\nm=video 51372 RTP/AVP 31\r\nm=application 32416 udp wb\r\na=orient:portrait\r\n",
%%     SdpStr.


    "v=0\r\no=bob 280744730 28977631 IN IP4 host.example.com\r\ns=\r\nt=0 0\r\na=group:ANAT 1 2\r\nm=audio 25000 RTP/AVP 0\r\nc=IN IP6 2001:DB8::1\r\na= <ICE-encoded additional IPv6 addresses (and ports)>\r\na=mid:1\r\nm=audio 22334 RTP/AVP 0\r\nc=IN IP4 192.0.2.1\r\na= <ICE-encoded additional IPv4 addresses (and ports)>\r\na=mid:2\r\n".


test_sdp() ->
    SdpStr = test_str(),
    {ok, Sdp1} = parse(SdpStr),
    Sdp1.

%%               c=IN IP4 224.5.6.7
%%                 a=type:H332
%%                 m=audio 49230 RTP/AVP 0
%%                 m=video 49232 RTP/AVP 31
%%                 m=application 12349 udp wb
%%                 m=control 49234 H323 mc
%%                 c=IN IP4 134.134.157.81


test() ->
    SdpStr = test_str(),
    Sdp1 = test_sdp(),
    Sdp_out = print(Sdp1),
    SdpStr = lists:flatten(Sdp_out),
    ok.
