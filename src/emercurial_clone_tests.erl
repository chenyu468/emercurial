-module(emercurial_clone_tests).

%%-export([test_setup/0,test_basic/1,test_clean/0,test_a/0]).
-export([test_basic/0,test_clean/0]).
-import(emercurial_common_tests,[setup/1,teardown/0,append/2]).

-include("emercurial.hrl").
-include_lib("eunit/include/eunit.hrl").

test_basic()->
    setup(?MODULE),    
    append("a","a"),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    Result_a =emercurial_client:commit(Pid,#commit{message='first',add_remove=true}),
    error_logger:info_report(['clone_test_basic_commit_1',Result_a]),
    Result_b = emercurial_client:clone(#clone{source='.',dest='cloned'}),
    error_logger:info_report(['clone_test_basic_commit_2',Result_b]),
    {ok,Pid_clone} = Result_b,
    %% {ok,Pid_clone} = emercurial_client:start_link('cloned','UTF-8','none',true),
    %%emercurial_client:open(Pid_clone)
    error_logger:info_report(['clone_test_basic_commit_2_1',Pid_clone]),
    Log_a = emercurial_client:log(Pid,#log{}),
     error_logger:info_report(['clone_test_basic_commit_2_2',Log_a]),
    Log_b = emercurial_client:log(Pid_clone,#log{}),
    error_logger:info_report(['clone_test_basic_commit_3',Log_a,Log_b]).

test_clean()-> 
    teardown().
