-module(emercurial_misc_tests).

-export([clone_test_a/0]).
-export([do_spawn/0,loop/0]).

-include("emercurial.hrl").
-include_lib("eunit/include/eunit.hrl").

cmdbuilder_test()->
    Cmd = "cmd -a -b",
    ?assertMatch(Cmd,
                 emercurial_misc:cmdbuilder(
                   'cmd',[],[{a,true},{b,true},{c,none}])).
    %% emercurial_misc:process_key_value(a,true), %%-a    
    %% emercurial_misc:process_key_value(str, 'a'), %%--str s
    %% emercurial_misc:process_key_value(d_ash, true), %%--str s
    %% emercurial_misc:process_key_value('-', true), %%--str s
    %% emercurial_misc:process_key_value('list', [1,2]). %%--str s
    %%emercurial_misc:process_key_value_internal(a,true).

replace_test()->
    Result = emercurial_misc:replace('aaa_bbb_ccc',$_,$-),
    ?assertMatch(Result,'aaa-bbb-ccc').

clone_test_a()->
    %%Clone = #clone{},
    try
        Clone = #clone{source='hg-project', dest='hg4-project'},
        emercurial_client:clone(Clone)
    catch A:B->
            error_logger:info_report([clone,A,B])
    end.

%% commit_test_a()->
%%     %% emercurial_client:clone(#clone{source='~/my-project',dest=''}),
%%     append("a","a"),
%%     Commit = #commit{message = 'first',add_remove=true},
%%     {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
%%     {Rev,Node} = emercurial_client:commit(Pid,Commit),
%%     error_logger:info_report([commit_test_a,{Rev,Node}]),
%%     Log = #log{revrange=Node},
%%     Result_b = emercurial_client:log(Pid,Log),
%%     error_logger:info_report([commit_test_b,Result_b]).


binary_to_atom_list_test()->
    Result = emercurial_misc:binary_to_atom_list(<<"1234567853434">>,<<$5>>),
    ?assertMatch(Result,['1234','678','3434']).

binary_to_tuple_test()->
    A = {'12341243',aaaa},
    B =emercurial_misc:binary_to_tuple(<<"12341243 aaaa">>,<<" ">>),
    ?assertMatch(A,B).

binary_to_tuple_list_test()->
    A = [{'12',aa},{'13',bb},{'14',cc}],
    B = emercurial_misc:binary_to_tuple_list(<<"12: aa,13: bb,14: cc">>,<<",">>,
                                             <<":">>),
    ?assertMatch(A,B).

atom_to_atom_list_test()->
    A = [abc,ddfd,eee,fff],
    B = emercurial_misc:atom_to_atom_list('abc ddfd eee fff'," "),
    ?assertMatch(A,B).

loop() ->
     receive
         stop ->
             stopped;
         {test, From, Arg} ->
             timer:sleep(3000),
             From ! Arg,
             loop()
     end.

do_spawn() ->
    %% Equivalent to SpawnProcess:call("hello", "process"),
    Pid = spawn(?MODULE, loop, []),
    Pid.

get_run_command_binary_test()->
    Data = [da,de,df],
    A = emercurial_misc:get_run_command_binary(Data),
    B =  <<0,0,0,8,100,97,0,100,101,0,100,102>>,
    ?assertMatch(A,B).

skiplines_test()->
    A = [<<"b">>,<<"c\n">>],
    B = emercurial_misc:skiplines(<<"a,b,c",$\n>>,<<"a">>),
    %%error_logger:info_report([misc_tests_skiplines_tests,B]).
    ?assertMatch(A,B).

