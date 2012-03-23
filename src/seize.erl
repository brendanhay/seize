%% @doc Run system commands via ports.
%% Links the calling process to the resulting port, exit messages
%% need to be trapped and handled by the caller.
-module(seize).

%% API
-export([cmd/1,
         cmd/2,
         cmd/3]).

-type port_arg()      :: [{u | b | m | d | p, string()}].
-type port_response() :: {ok, binary()} |
                         {error, binary()} |
                         timeout.

-export_types([port_arg/0,
               port_response/0]).

-define(CMUXER,  filename:join([code:lib_dir(seize), "priv/seize"])).
-define(TIMEOUT, 120 * 1000).

%%
%% API
%%

-spec cmd(string()) -> port_response().
%% @doc
cmd(Cmd) -> cmd(Cmd, []).

-spec cmd(string(), port_arg()) -> port_response().
%% @doc
cmd(Cmd, Args) -> cmd(Cmd, Args, ?TIMEOUT).

-spec cmd(string(), port_arg(), non_neg_integer()) -> port_response().
%% @doc
cmd(Cmd, Args, Timeout) ->
    Seize = string:join([?CMUXER, Cmd, parse_args(Args)], " "),
    execute(string:strip(Seize, right), Timeout).

%%
%% Private
%%

-spec parse_args(port_arg()) -> string().
%% @private
parse_args(Args) ->
    IoList = lists:map(fun parse_arg/1, Args),
    binary_to_list(iolist_to_binary(IoList)).

-spec parse_arg(port_arg()) -> iolist().
%% @private
parse_arg({K, V}) -> io_lib:format("-~s ~p ", [K, V]);
parse_arg(V)      -> io_lib:format("~s ", [V]).

-spec execute(string(), non_neg_integer()) -> port_response().
%% @private
execute(Cmd, Timeout) ->
    Port = open_port({spawn, Cmd}, [binary, {packet, 2}, exit_status]),
    loop(Port, {[], []}, Timeout).

-spec loop(port(), {iolist(), iolist()}, integer()) -> port_response().
%% @private
loop(Port, State, Timeout) ->
    receive
        {Port, {data, Bin}}          -> loop(Port, prepend_output(Bin, State), Timeout);
        {Port, {exit_status, 0}}     -> completed(ok, State);
        {Port, {exit_status, _Code}} -> completed(error, State);
        Unknown                      -> completed(error, Unknown)
    after
        Timeout                      -> timeout
    end.

-spec prepend_output(binary(), {iolist(), iolist()}) -> {iolist(), iolist()} | tuple().
%% @private
prepend_output(Bin, {Out, Err}) ->
    try binary_to_term(Bin) of
        {stdout, NewOut} -> {[NewOut|Out], Err};
        {stderr, NewErr} -> {Out, [NewErr|Err]};
        Error            -> Error
    catch
        exit:bardarg     -> {badarg, "Unable to parse binary result from mux"}
    end.

-spec completed(ok | error, {iolist(), iolist()}) -> {ok | error, binary()}.
%% @private
completed(ok, {Out, _Err})            -> {ok, normalise(Out)};
completed(error, {error, _Code, Msg}) -> {error, iolist_to_binary(Msg)};
completed(error, {_Out, Err})         -> {error, normalise(Err)};
completed(error, Unknown)             -> {error, term_to_binary(Unknown)}.

-spec normalise(iolist()) -> binary().
%% @private
normalise(IoList) -> iolist_to_binary(lists:reverse(IoList)).
