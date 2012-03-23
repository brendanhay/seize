%% This Source Code Form is subject to the terms of
%% the Mozilla Public License, v. 2.0.
%% A copy of the MPL can be found in the LICENSE file or
%% you can obtain it at http://mozilla.org/MPL/2.0/.
%%
%% @author Brendan Hay
%% @copyright (c) 2012 Brendan Hay <brendan@soundcloud.com>
%% @doc
%%

-module(muxer_test).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

-define(EQC(P), ?assert(proper:quickcheck(P))).

%%
%% Units
%%

incorrect_number_of_args_test() ->
    ?assertEqual({error, <<"incorrect number of arguments">>}, muxer:cmd("")).

non_existing_binary_test() ->
    ?assertEqual({error, <<"binary to execute not found">>}, muxer:cmd("monkkkey_fez")).

no_args_test() ->
    ?assertMatch({ok, _Out}, muxer:cmd("/bin/ls")).

valid_args_test() ->
    ?assertMatch({ok, _Out}, muxer:cmd("/bin/ls", ["/usr"])).

with_invalid_args_test() ->
    ?assertError(badarg, muxer:cmd("/bin/ls", ["asd", 213, atom])).

%%
%% Properties
%%

stdout_test() ->
    ?EQC(?FORALL(E, long_string(),
                 begin
                     {ok, list_to_binary(E ++ "\n")}
                         =:= run("stdout", [E])
                 end)).

stderr_test() ->
    ?EQC(?FORALL({E, C}, {long_string(), pos_integer()},
                begin
                    {error, list_to_binary(E ++ "\n")}
                        =:= run("stderr", [E, integer_to_list(C)])
                end)).

%%
%% Generators
%%

word() ->
    non_empty(list(union(lists:seq($a, $z)
                         ++ lists:seq($A, $Z)
                         ++ lists:seq($0, $9)
                         ++ "_"))).

long_string() -> ?SIZED(Size, resize(Size * 20, word())).

%%
%% Helpers
%%

run(FD, Args) ->
    Path = filename:join([code:lib_dir(muxer), "test/muxer_" ++ FD]),
    muxer:cmd(Path ++ " " ++ string:join(Args, " ")).
