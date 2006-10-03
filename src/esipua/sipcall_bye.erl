-module(sipcall_bye).

-behaviour(gen_server).

-include("siprecords.hrl").

%% api
-export([start_link/4, stop/0]).

%% gen_server callbacks
-export([init/1,
	 code_change/3,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2]).

-record(state, {dialog}).

-define(SERVER, ?MODULE).

start_link(Dialog, Contact, Auths, CSeqNo) when is_record(Dialog, dialog),
						is_list(Contact),
						is_list(Auths),
						is_integer(CSeqNo) ->
    gen_server:start_link(?MODULE, [Dialog, Contact, Auths, CSeqNo], []).

stop() ->
    gen_server:cast(?SERVER, stop).

%%
%% gen_server callbacks
%%
init([Dialog, Contact, Auths, CSeqNo]) ->
    ok = sipdialog:register_dialog_controller(Dialog, self()),

    {ok, Ack, Dialog1} = generate_ack(Dialog, Contact, CSeqNo),

    {ok, _SendingSocket, _Dst, _TLBranch} = siphelper:send_ack(Ack, Auths),

    ExtraHeaders = [{"Reason", ["SIP ;cause=200 ;text=\"Call completed elsewhere\""]}],
    {ok, Bye, Dialog3} = generate_bye(Dialog1, Contact, ExtraHeaders),
%% 	generate_new_request("BYE", Dialog1, Contact, ExtraHeaders),
    {ok, _Pid, _Branch} = siphelper:send_request(Bye),

    {ok, #state{dialog=Dialog3}}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


handle_call(Request, _From, State) ->
    error_logger:error_msg("~p: Unhandled call in ~p~n", [?MODULE, Request]),
    {reply, ok, State}.


handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Request, State) ->
    error_logger:error_msg("~p: Unhandled cast in ~p~n", [?MODULE, Request]),
    {noreply, State}.


handle_info(Info, State) ->
    error_logger:error_msg("~p: Unhandled info in ~p~n", [?MODULE, Info]),
    {noreply, State}.


terminate(_Reason, _State) ->
    terminated.


generate_ack(Dialog, Contact, CSeqNo) when is_record(Dialog, dialog), is_list(Contact), is_integer(CSeqNo) ->

    Method = "ACK",
    CSeq = lists:concat([CSeqNo, " ", Method]),
    ExtraHeaders = [
		    {"CSeq", [CSeq]},
		    {"Contact",  [Contact]}
		   ],

    {ok, Ack, Dialog1, _Dst} = sipdialog:generate_new_request(Method, ExtraHeaders, <<>>, Dialog),
    {ok, Ack, Dialog1}.


generate_bye(Dialog, Contact, ExtraHeaders) when is_record(Dialog, dialog), is_list(Contact), is_list(ExtraHeaders) ->

    ExtraHeaders1 = [{"Contact",  [Contact]} | ExtraHeaders ],

    {ok, Bye, Dialog1, _Dst} = sipdialog:generate_new_request("BYE", ExtraHeaders1, <<>>, Dialog),
    {ok, Bye, Dialog1}.
