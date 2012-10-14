-module(emercurial_diff_tests).

-export([test_setup/0,test_basic/1,test_clean/0,test_a/0]).
-import(emercurial_common_tests,[setup/1,teardown/0,append/2]).

-include("emercurial.hrl").
-include_lib("eunit/include/eunit.hrl").

test_setup()->
    setup(?MODULE),    
    %%append("a","a"),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    Pid.

test_basic(Pid)->
    append("a","a\n"),
    emercurial_client:add(Pid,#add{files=['a']}),
    Diff1 =  <<"diff -r 000000000000 a\n--- /dev/null\n+++ b/a\n@@ -0,0 +1,1 @@\n+a\n">>,
    Diff_result = emercurial_client:diff(Pid,#diff{nodates = true}),
    error_logger:info_report([diff_1,Diff1,Diff_result]),
    %%?assertMatch(Diff1,Diff_result),
    Diff_result_a = emercurial_client:diff(Pid,#diff{files=['a'],nodates = true}),
    error_logger:info_report([diff_2,Diff_result_a]),
    %%?assertEqual(Diff1,Diff_result_a),
    {Rev0,_Node0} = emercurial_client:commit(Pid,#commit{message='first'}),
    Diff_result_b = emercurial_client:diff(Pid,#diff{change=Rev0,nodates=true}),
    error_logger:info_report([diff_3,Diff_result_b]). 
    

test_a()->
    Pid = test_setup(),
    test_basic(Pid).

test_clean()->
    teardown().
