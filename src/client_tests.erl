-module(client_tests).

-export([tag_a_test_a/0]). %% ,branch_empty_test_a/0]).
-import(emercurial_common_tests,[setup/1,teardown/0,append/2]).

-include("emercurial.hrl").
-include_lib("eunit/include/eunit.hrl").

tag_a_test_a()->
    teardown(),
    setup(tag),
    append("a","a"),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    {Rev,_Node} =emercurial_client:commit(Pid,#commit{message='first',add_remove=true}),
    Tag = #tag{names = 'my tags'},
    Result = emercurial_client:tag(Pid,Tag),
    error_logger:info_report([tag_result,Result]),
    Tag_b = #tag{names = 'local tag',rev = Rev,local = true},
    Result_b = emercurial_client:tag(Pid,Tag_b),
    error_logger:info_report([tag_b_result,Result_b]),
    Tags = emercurial_client:tags(Pid),
    error_logger:info_report([tab_a_result_a,Tags]),
    teardown().

%% branch_empty_test_a()->
%%     teardown(),
%%     setup(tag),
%%     {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
%%     Result = emercurial_client:branch(Pid,#branch{}),
%%     error_logger:info_report([client_branch_result,Result]).
