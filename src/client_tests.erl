-module(client_tests).

-export([clone_test_a/0,tag_a_test_a/0,branch_empty_test_a/0,
         branch_basic_test_a/0,
         branch_reset_with_name_test_a/0,branch_reset_test_a/0,
        branch_exists_test_a/0,branch_force_test_a/0,
         push_test_a/0,test_all/0]).
-import(emercurial_common_tests,[setup/1,teardown/1,append/2]).

-include("emercurial.hrl").
-include_lib("eunit/include/eunit.hrl").

test_all()->
    clone_test_a(),
    tag_a_test_a(),
    branch_empty_test_a(),
    branch_basic_test_a(),
    branch_reset_with_name_test_a(),
    branch_reset_test_a(),
    branch_exists_test_a(),
    branch_force_test_a(),
    push_test_a().

%%=============================
%% clone test 
%%=============================
clone_test_a()->
    teardown(clone),    
    setup(clone),
    append("a","a"),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    _Result_a =emercurial_client:commit(Pid,#commit{message='first',add_remove=true}),
    Result_b = emercurial_client:clone(#clone{source='.',dest='cloned'}),
    {ok,Pid_clone} = Result_b,
    Log_a = emercurial_client:log(Pid,#log{}),
    Log_b = emercurial_client:log(Pid_clone,#log{}),
    ?assertMatch(Log_a,Log_b).

%%=============================
%% tag test
%%=============================
tag_a_test_a()->
    teardown(tag),    
    setup(tag),
    append("a","a"),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    {Rev,_Node} =emercurial_client:commit(Pid,#commit{message='first',add_remove=true}),
    Tag = #tag{names = 'my tags'},
    ok = emercurial_client:tag(Pid,Tag),
    error_logger:info_report([tag_result,ok,Rev]),
    Tag_b = #tag{names = 'local tag',rev = Rev,local = true},
    ok = emercurial_client:tag(Pid,Tag_b),
    error_logger:info_report([tag_b_result,ok]),
    Tags = emercurial_client:tags(Pid),
    error_logger:info_report([tab_a_result_a,Tags]),
    teardown(tag).

%%=============================
%% branch test
%%=============================
branch_empty_test_a()->
    teardown(branch),
    setup(branch),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    Result = emercurial_client:branch(Pid,#branch{}),
    error_logger:info_report([client_branch_result,Result]),
    teardown(branch).

branch_basic_test_a()->
    teardown(branch),
    setup(branch),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    Result = emercurial_client:branch(Pid,#branch{name='foo'}),
    ?assertMatch(Result,'foo'),
    append("a","a"),
    {_Rev,_Node} =emercurial_client:commit(Pid,#commit{message='first',add_remove=true}),
    Log = #log{},
    Result_a = emercurial_client:log(Pid,Log),
    error_logger:info_report([client_basic_test_a_1,Result_a]),
    First = lists:nth(1,Result_a),
    B_a = love_misc:get_value(branch,First),
    ?assertMatch(B_a, 'foo').

branch_reset_with_name_test_a()->
    teardown(branch),
    setup(branch),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    Throw = #mercurial_value_error{value="cannot use both name and clean"},
    ?assertThrow(Throw,
                 emercurial_client:branch(Pid,#branch{name='foo',clean=true})),
    teardown(branch).    

branch_reset_test_a()-> 
    teardown(branch),
    setup(branch),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    Result = emercurial_client:branch(Pid,#branch{name='foo'}),
    ?assertMatch('foo',Result),
    error_logger:info_report([client_tests_reset_test_1,Result]),
    Result_a = emercurial_client:branch(Pid,#branch{clean=true}),
    error_logger:info_report([client_tests_reset_test_2,Result_a]).
    
branch_exists_test_a()-> %%测试文件已经存在的情况
    teardown(branch),
    setup(branch),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    append("a","a"),
    {_Rev,_Node} =emercurial_client:commit(Pid,#commit{
                                                  message='first',
                                                  add_remove=true}),
    Result = emercurial_client:branch(Pid,#branch{name='foo'}),
    error_logger:info_report([tests_branch_exists_1,Result]),
    append("a","a"),
    {_Rev_1,_Node_1} =emercurial_client:commit(Pid,#commit{
                                                  message='second'
                                                     }),
    Result_a = emercurial_client:branch(Pid,#branch{}),
    ?assertMatch('foo',Result_a),
    teardown(branch).

branch_force_test_a()->
    teardown(branch),
    setup(branch),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    append("a","a"),
    {_Rev,_Node} =emercurial_client:commit(Pid,#commit{
                                             message='first',
                                             add_remove=true}),
    Result = emercurial_client:branch(Pid,#branch{name='foo'}),
    error_logger:info_report([client_force_test_1,Result]),
    append("a","a"),
    {_Rev_a,_Node_a} =emercurial_client:commit(Pid,#commit{
                                                 message='second'
                                                }),    
    Error = {mercurial_command_error,
             [branch,default],
             4294967295,<<>>,"(use 'hg update' to switch to it)"},
    ?assertThrow(Error, 
                 emercurial_client:branch(Pid,#branch{name=default})),
    Result_a = emercurial_client:branch(Pid,#branch{name=default,force=true}),    
    ?assertMatch(default,Result_a).

%%=====================
%% push
%%=====================
push_test_a()->
    teardown(push),
    setup(push),
    {ok,Pid} = emercurial_client:start_link('none','UTF-8','none',true),
    append("a","a"),
    {_Rev,_Node} =emercurial_client:commit(Pid,#commit{
                                             message='first_a',
                                             add_remove=true}),
    Result_b = emercurial_client:clone(#clone{source='.',dest='other'}),
    error_logger:info_report([client_tests_push_test_1,Result_b]),
    {ok,Pid_a} = emercurial_client:open('other'),
    error_logger:info_report([client_tests_push_test_2,Pid_a]),    
    append("a","a"),
    {_Rev_a,_Node_a} =emercurial_client:commit(Pid,#commit{
                                                 message='second_a'%%,
                                                %% add_remove=true
                                                }),    
    Result_c = emercurial_client:push(Pid,#push{dest='other'}),
    Log = emercurial_client:log(Pid,#log{}),
    error_logger:info_report([client_tests_push_test_4,Log]),
    Log_b = emercurial_client:log(Pid_a,#log{}),
    ?assertMatch(Log,Log_b),
    ?assertMatch(true,Result_c).
