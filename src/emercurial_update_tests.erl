-module(emercurial_update_tests).

-export([test_setup/0,test_basic/1,test_a/0,test_clean/0,test_cat/1]).
-import(emercurial_common_tests,[setup/1,teardown/0,append/2]).
-include("emercurial.hrl").
-include_lib("eunit/include/eunit.hrl").


test_setup()->
    setup(?MODULE),    
    append("a","a"),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    Commit = #commit{message = 'first',add_remove=true},
    error_logger:info_report([update_tests_setup_1,Pid]),
    {Rev,Node} = emercurial_client:commit(Pid,Commit),
    error_logger:info_report([update_tests_setup_2,Rev,Node]),
    put(rev0,Rev),
    put(node0,Node),
    append("a","b"),
    New_commit = #commit{message = 'second',add_remove=true},
    {Rev_a,Node_a} = emercurial_client:commit(Pid,New_commit),
    put(rev1,Rev_a),
    put(node1,Node_a),
    Pid.

test_basic(Pid)->
    Update = #update{rev=get(rev0)},
    {U,M,R,Ur} = emercurial_client:update(Pid,Update),
    %%error_logger:info_report([test_basic,Data]).
    ?assertMatch(U,1),
    ?assertMatch(M,0),
    ?assertMatch(R,0),
    ?assertMatch(Ur,0).

test_a()->
    Pid = test_setup(),
    test_basic(Pid),
    test_tip(Pid),
    test_cat(Pid).

test_clean()->    
    erase(rev0),
    erase(node0),
    erase(rev1),
    erase(node1),
    teardown().

test_tip(Pid)->
    emercurial_client:update(Pid,#update{rev=get(rev0)}),
    Data = {_U,_M,_R,_Ur} = emercurial_client:update(Pid,#update{}),
    error_logger:info_report([test_tip,Data]),
    ?assertMatch(Data,{1,0,0,0}),
    Out = emercurial_client:parents(Pid,#parents{}), 
    error_logger:info_report([test_tip_2,Out]),
    Node_a = proplists:get_value(node,Out),
    ?assertMatch(Node_a,get(node1)),
    error_logger:info_report([Node_a,get(node1)]).

test_cat(Pid)->
    file:delete("a"),
    Out = emercurial_client:cat(Pid,#cat{files=['a'],output='a'}),
    ?assertMatch(Out,[]),
    error_logger:info_report([test_cat,Out]).
