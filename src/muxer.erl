%% @doc Run system commands via ports, capturing stdout/stderr
%% Links the calling process to the resulting port, exit messages
%% need to be trapped and handled by the caller.
-module(muxer).

%% API
-export([cmd/1,
         cmd/2,
         cmd/3]).

-type port_arg()      :: [{atom(), string()} | string()].
-type port_response() :: {ok | error, integer(), string(), {stdout, string()}, {stderr, string()}} |
                         timeout.

-export_types([port_arg/0,
               port_response/0]).

-define(MUXER, code:priv_dir(muxer) ++ "/muxer").

%%
%% API
%%

-spec cmd(string()) -> port_response().
%% @doc
cmd(Cmd) -> cmd(Cmd, []).

-spec cmd(string(), [port_arg()]) -> port_response().
%% @doc
cmd(Cmd, Args) -> cmd(Cmd, Args, 60 * 1000).

-spec cmd(string(), [port_arg()], pos_integer()) -> port_response().
%% @doc
cmd(Cmd, Args, Timeout) ->
    Muxer = string:join([?MUXER, Cmd, parse_args(Args)], " "),
    execute(string:strip(Muxer, right), Timeout).

%%
%% Private
%%

-spec parse_args([port_arg()]) -> string().
%% @private
parse_args(Args) ->
    IoList = lists:map(fun parse_arg/1, Args),
    binary_to_list(iolist_to_binary(IoList)).

-spec parse_arg(port_arg()) -> iolist().
%% @private
parse_arg({K, V}) -> io_lib:format("-~s ~p ", [K, V]);
parse_arg(V)      -> io_lib:format("~s ", [V]).

-spec execute(string(), pos_integer()) -> port_response().
%% @private
execute(Cmd, Timeout) ->
    Options = [binary, {packet, 4}, exit_status],
    Port = open_port({spawn, Cmd}, Options),
    loop(Port, [], Timeout).

-spec loop(port(), list(), pos_integer()) -> port_response().
%% @private
loop(Port, Data, Timeout) ->
    receive
        {Port, {data, NewData}} ->
            loop(Port, [NewData|Data], Timeout);
        {Port, {exit_status, _Status}} ->
            [H|_] = lists:flatten(Data),
            binary_to_term(H)
    after Timeout ->
            timeout
    end.
