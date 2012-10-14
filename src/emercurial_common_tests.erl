-module(emercurial_common_tests).

-export([setup/1,teardown/0,append/2]).


setup(Test_item)->
    %%application: os:genenv("HOME"),
    {ok,Mercurial} = application:get_env(romeo,mercurial_test),
    Home = os:getenv("HOME"),
    Mercurial_path = filename:join([Home,Mercurial,Test_item]),
    Mercurial_test_path =  filename:join([Home,Mercurial]),
    ok = application:set_env(romeo,mercurial_path,Mercurial_test_path),
    ok = make_dir(Mercurial_test_path),
    ok = make_dir(Mercurial_path),
    file:set_cwd(Mercurial_path),
    Result = os:cmd("hg init"),                                   
    error_logger:info_report([setup,Result]),
    Result.
    
teardown()->
    {ok, Path} = application:get_env(romeo,mercurial_path),
    %%del_dir(Path).
    nuke_dir(Path).

append(Name,Content)->
    %%ok = file:set_cwd(Path),
    %%{ok, Io_device}=file:open(Name,write),
    %%file:pwrite(
    file:write_file(Name,list_to_binary(Content)).

nuke_dir(Dir) ->
    FoldFun = fun(File) ->
        Path = filename:join(Dir, File),
        case file:delete(Path) of
            {error, eperm} -> ok = nuke_dir(Path);
            {error, enoent} -> ok;
            ok -> ok
        end
    end,
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(FoldFun, Files),
            ok = file:del_dir(Dir);
        {error, enoent} ->
            ok
    end.

make_dir(Local_path)->
    Result = file:make_dir(Local_path) ,
    case Result of
        ok ->
            ok;
        {error,eexist} ->
            ok;
        _ ->
            error
    end.
