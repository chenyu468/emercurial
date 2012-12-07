%%%-------------------------------------------------------------------
%%% @author chen yu <>
%%% @copyright (C) 2012, chen yu
%%% @doc
%%% modified from python-hglib/hglib/__init__.py,client.py 
%%% @end
%%% Created :  6 Sep 2012 by chen yu <>
%%%-------------------------------------------------------------------
-module(emercurial_client).

-behaviour(gen_server).

%% API
-compile([export_all]).

-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([add/2,branch/2,cat/2,clone/1,clone/2,commit/2,diff/2,init_hg/1,log/2,
         open/3,update/2,parents/2,push/2,tag/2,tags/1]).

-include("emercurial.hrl").

-define(SERVER, ?MODULE). 
-define(HGPATH, 'hg').
-define(RECEIVE_TIMEOUT,5000).
-define(CHANNEL_SIZE,1).
-define(LENGTH_SIZE,4).
-define(CHANNEL_TOTAL_SIZE, CHANNEL_SIZE + LENGTH_SIZE).
-define(TEMPLATE_CHANGESET,'{rev}\\0{node}\\0{tags}\\0{branch}\\0{author}\\0{desc}\\0{date}\\0').

-record(state, {path,encoding,configs,connect,args,port,capabilities}).

%%%===================================================================
%%% API
%%%===================================================================
clone(Clone)->
    Kwargs = get_excluded_list(Clone,[source,dest]),
    Args = [Clone#clone.source, Clone#clone.dest],
    Args_a = cmdbuilder('clone',Args,Kwargs),
    Args_b = atom_to_list(?HGPATH) ++ " " ++ Args_a,
    {Output,Result} = emercurial_misc:cmd_process(Args_b),
    if
        Result =/= 0->
            %%exit({mercurial_clone_error,Output});
            exit(gen_error(Args_b,Result,Output,Output));
        true ->
            ok
    end,
    {ok,Encoding} = application:get_env(emercurial,encoding),
    emercurial_client:start_link(Clone#clone.dest,Encoding,Clone#clone.configs,false).

open(Path,Encoding,Configs)->
    emercurial_client:start_link(Path,Encoding,Configs,false).

init_hg(Clone)->
    Kwargs = get_excluded_list(Clone,[source,dest]),
    Args = [Clone#clone.dest],
    Args_a = cmdbuilder('init',Args,Kwargs),
    Args_b = atom_to_list(?HGPATH) ++ " " ++ Args_a,
    {Output,Result} = emercurial_misc:cmd_process(Args_b),
    if
        Result =/= 0->
            %%exit({mercurial_clone_error,Output})
            exit(gen_error(Args_b,Result,Output,Output));
        true->
            ok
    end,
    emercurial_client:start_link(Clone#clone.dest,Clone#clone.encoding,Clone#clone.configs,false). 

add(Pid,Add)->
    gen_server:call(Pid,{add,Add}).

branch(Pid,Branch)->
    gen_server:call(Pid,{branch,Branch}).

cat(Pid,Cat)->
    gen_server:call(Pid,{cat,Cat}).

clone(Pid,Clone)->
    gen_server:call(Pid,{clone,Clone}).

commit(Pid,Commit)->
    gen_server:call(Pid,{commit,Commit}).


diff(Pid,Diff)->
    gen_server:call(Pid,{diff,Diff}).

log(Pid,Log)->
    gen_server:call(Pid,{log,Log}).

parents(Pid,Parents)->
    gen_server:call(Pid,{parents,Parents}).

push(Pid,Push)->
    gen_server:call(Pid,{push,Push}).

tag(Pid,Tag)->
    gen_server:call(Pid,{tag,Tag}).

tags(Pid)->
    gen_server:call(Pid,{tags}).

update(Pid,Update)->
    gen_server:call(Pid,{update,Update}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Dest,Encoding,Configs,Connect) ->
    gen_server:start_link(?MODULE, [Dest,Encoding,Configs,Connect], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Dest,Encoding,Configs,Connect]) ->
    Args = [?HGPATH,'--config','ui.interactive=True','serve','--cmdserver','pipe'],
    Args_a = add_atom(Args,'-R',Dest),
    Args_b = add_atom(Args_a,'--config',Configs),                
    Command = emercurial_misc:list_to_command(Args_b),
    Port_options =  [stream, binary, exit_status,stderr_to_stdout, hide],
    error_logger:info_report([init,Command]),
    Port = open_port({spawn, Command}, Port_options),    
    application:set_env(emercurial,encoding,Encoding),
    State = #state{path=Dest,encoding=Encoding,configs=Configs,connect=Connect,args=Args_b,port=Port},
    timer:send_after(0,'read_hello'),
    {ok,  State}.




%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_call({add,Add},_From,State)->
    Internal_add = convert(Add),
    Kwargs = get_excluded_list(Internal_add,[files]),
    Files = Add#add.files,
    Args = emercurial_misc:run_command_cmdbuilder('add',[],Kwargs),
    New_args = Args ++ Files,
    ok = emercurial_reterrorhandler:init(Args,none),
    Eh = fun emercurial_reterrorhandler:handle/3,
    Raw_command = #raw_command{args = New_args,error_handler=Eh},
    _Out = raw_command(State,Raw_command),
    Result = emercurial_reterrorhandler:nonzero(),
    {reply,Result,State};         

handle_call({branch,#branch{name=none,clean=true}},_From,_State)->
    throw(emercurial_misc:generate_value_error(
            "cannot use both name and clean"));

handle_call({branch,Branch},_From,State)->
    Internal_branch = convert(Branch),
    Kwargs = get_excluded_list(Internal_branch,[name]),
    Name = Branch#branch.name,
    Args = emercurial_misc:run_command_cmdbuilder('branch',[Name],Kwargs),
    New_args = Args,
    Raw_command = #raw_command{args = New_args},
    Out = raw_command(State,Raw_command),
    %% case Out of
    %%     [] ->
    %%         ok;
    %%     _ ->
    %%         exit(gen_error(New_args,Out,get_out(),get_error()))
    %% end,
    {reply,Out,State};


handle_call({cat,Cat},_From,State)->
    Internal_cat = convert(Cat),
    Kwargs = get_excluded_list(Internal_cat,[files]),
    Files = Cat#cat.files,
    Args = emercurial_misc:run_command_cmdbuilder('cat',[],Kwargs),
    New_args = Args ++ Files,
    Raw_command = #raw_command{args = New_args},
    Out = raw_command(State,Raw_command),
    case Out of
        [] ->
            ok;
        _ ->
            exit(gen_error(New_args,Out,get_out(),get_error()))
    end,
    {reply,Out,State};

handle_call({clone,Clone},_From,State)->
    Internal_clone = convert(Clone),
    Kwargs = get_excluded_list(Internal_clone,[source,dest]),
    Source = Clone#clone.source,
    Dest = Clone#clone.dest,
    Args = emercurial_misc:run_command_cmdbuilder('clone',
                                                  [Source,Dest],Kwargs),
    Raw_command = #raw_command{args = Args},
    Out = raw_command(State,Raw_command),
    error_logger:info_report([handle_call_clone_2,Out]),
    {reply,Out,State};


handle_call({commit,Commit},_From,State)->
    Internal_commit = convert(Commit),
    Kwargs = get_excluded_list(Internal_commit,[]),
    Args = emercurial_misc:run_command_cmdbuilder('commit',[],Kwargs),
    Out = raw_command(State,#raw_command{args=Args}),
    error_logger:info_report([client_handle_call_commit_1,Out]),
    %% List_b = binary:split(Out,<<" ">>,[global]),
    %% Last = lists:last(List_b),
    %% [Rev,Node] = binary:split(Last,<<":">>),
    %% Rev_a = list_to_atom(binary_to_list(Rev)),
    %% Node_a = list_to_atom(lists:sublist(binary_to_list(Node),1,size(Node)-1)),
    %% process_out(Out),
    {reply,process_out(Out),State};

handle_call({diff,Diff},_From,State)->
    Internal_diff = convert(Diff),
    Kwargs = get_excluded_list(Internal_diff,[files]),
    Files = Diff#diff.files,
    Args = emercurial_misc:run_command_cmdbuilder('diff',[],Kwargs),
    New_args = Args ++ Files,
    Raw_command = #raw_command{args = New_args},
    Out = raw_command(State,Raw_command),
    {reply,Out,State};

handle_call({log,Log},_From,State)->
    Internal_log = convert(Log),
    Kwargs = get_excluded_list(Internal_log,[]),
    Args = emercurial_misc:run_command_cmdbuilder('log',[],Kwargs),
    Files = Log#log.files,
    New_args = Args ++ Files,
    error_logger:info_report([handle_call_log_2,New_args]),
    Raw_command = #raw_command{args = New_args},
    Out = raw_command(State,Raw_command),
    List_b = binary:split(Out,<<$\0>>,[global]),
    List_b_a = lists:sublist(List_b,1,length(List_b)-1),
    %% List_c = binary_to_list(lists:last(List_b)),
    Revision = emercurial_misc:generate_revision(List_b_a),
    {reply,Revision,State};

handle_call({parents,Parents},_From,State)->
    Internal_parents = convert(Parents),
    Kwargs = get_excluded_list(Internal_parents,[file]),
    File = Parents#parents.file,
    Args = emercurial_misc:run_command_cmdbuilder('parents',[File],Kwargs),
    Raw_command = #raw_command{args = Args},
    Out = raw_command(State,Raw_command),
    List_b = binary:split(Out,<<$\0>>,[global]),
    List_b_a = lists:sublist(List_b,1,length(List_b)-1),
    Revision = emercurial_misc:generate_revision(List_b_a),
    {reply,Revision,State};

handle_call({push,Push},_From,State)->
    Internal_push = convert(Push),
    Kwargs = get_excluded_list(Internal_push,[dest]),
    Dest = Push#push.dest,
    Args = emercurial_misc:run_command_cmdbuilder('push',[Dest],Kwargs),
    Raw_command = #raw_command{args = Args},
    Out = raw_command(State,Raw_command),
    List_b = binary:split(Out,<<$\0>>,[global]),
    List_b_a = lists:sublist(List_b,1,length(List_b)-1),
    Revision = emercurial_misc:generate_revision(List_b_a),
    {reply,Revision,State};    

handle_call({tag,Tag=#tag{names=Names}},_From,State)->
    case is_list(Names) of
        true ->
            New_names = Names;
        false ->
            New_names = [Names]
    end,
    Internal_tag = convert(Tag),
    Kwargs = get_excluded_list(Internal_tag,[names]),    
    Args = emercurial_misc:run_command_cmdbuilder('tag',[],Kwargs),
    New_args = Args ++ New_names,
    Out = raw_command(State,#raw_command{args=New_args}),
    {reply,process_out(Out),State};

handle_call({tags},_From,State)->
    %% Internal_tag = convert(Tag),
    %% Kwargs = get_excluded_list(Internal_tag,[names]),    
    Internal_tags = #internal_tags{v = true},
    Kwargs = get_excluded_list(Internal_tags,[]), 
    Args = emercurial_misc:run_command_cmdbuilder('tags',[],Kwargs),
    Out = raw_command(State,#raw_command{args=Args}),
    %%开始解析tags
    List_b = binary:split(Out,<<$\0>>,[global]),
    
    {reply,Out,State};


handle_call({update,Update},_From,State)->
    Internal_update = convert(Update),
    if
        Update#update.clean andalso Update#update.check ->
            %%exit({update_error,"clean and check can't be true at the same time",""});
            exit({mercurial_value_error,
                  "clean and check can't be true at the same time"});
        true ->
            ok
    end,
    Kwargs = get_excluded_list(Internal_update,[]),
    Args = emercurial_misc:run_command_cmdbuilder('update',[],Kwargs),
    Eh = fun (Return, Out, Error)->
                 case Return of
                     1 ->
                         Out;
                     _ ->
                         exit(gen_error(Args,Return,Out,Error))
                 end
         end,
    Out = raw_command(State,#raw_command{args=Args,error_handler=Eh}),
    List_a = emercurial_misc:skiplines(Out,<<"merging ">>),
    List_b = lists:map(fun(X)->
                               Y = binary_to_list(X),
                               T = string:tokens(Y," "),
                               N = list_to_integer(hd(T)),
                               N
                       end, List_a),
    Tuple = list_to_tuple(List_b),
    {reply,Tuple,State};

handle_call(Request, _From, State) ->
    error_logger:info_report([handle_call,Request]),
    Reply = ok,
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    error_logger:info_report([handle_cast,Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info('read_hello',State)->
    New_state = read_hello(State),
    {noreply,New_state};

handle_info(Info, State) ->
    error_logger:info_report([handle_info,Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
add_atom(Arg,_Key,none)->
    Arg;
add_atom(Arg,Key,Value) ->
    Arg ++ [Key] ++ [Value].

clear_out()->
    out_write(<<>>).

clear_error()->
    error_write(<<>>).

cmdbuilder(Name,Args_list,Kwarg_list)->
    emercurial_misc:cmdbuilder(Name,Args_list,Kwarg_list).

%% convert(Branch=#branch{})->
%%     #internal_branch{name = Branch#branch.name,
%%                      f = Branch#branch.force,
%%                      'C' = Branch#branch.clean};

convert(Log=#log{})->
    #internal_log{template = ?TEMPLATE_CHANGESET,
                  r=Log#log.revrange,
                  f=Log#log.follow,
                  follow_first=Log#log.follow_first,
                  d=Log#log.date,
                  c=Log#log.copies,
                  k=Log#log.keyword,
                  removed=Log#log.removed,
                  m=Log#log.onlymerge,
                  u=Log#log.user,
                  b=Log#log.branch,
                  'P'=Log#log.prune,
                  h=Log#log.hidden,
                  l=Log#log.limit,
                  'M'=Log#log.nomerge,
                  'I'=Log#log.include,
                  'X'=Log#log.exclude
                 };

convert(Commit=#commit{})->
    #internal_commit{debug = true,
                     m = Commit#commit.message,
                     'A' = Commit#commit.'add_remove',
                     'close_branch' = Commit#commit.'close_branch',
                     d = Commit#commit.date,
                     u = Commit#commit.user,
                     l = Commit#commit.log_file,
                     'I' = Commit#commit.include,
                     'X' = Commit#commit.exclude
                    };  
                   
convert(Update=#update{}) ->
    #internal_update{r = Update#update.rev,
                     'C' = Update#update.clean,
                     c = Update#update.check,
                     d = Update#update.date
                    };
convert(Parents=#parents{}) ->
    #internal_parents{r = Parents#parents.rev,
                     template =  ?TEMPLATE_CHANGESET
                     };

convert(Cat=#cat{}) ->
    #internal_cat{r = Cat#cat.rev,
                  o = Cat#cat.output
                 };

convert(Clone=#clone{}) ->
    #internal_clone{
         b = Clone#clone.branch,
         u = Clone#clone.updaterev,
         r = Clone#clone.rev
        };

convert(Diff=#diff{}) ->
    #internal_diff{
         r = Diff#diff.revs,
         c = Diff#diff.change,
         a = Diff#diff.text,
         g = Diff#diff.git,
         nodates = Diff#diff.nodates,
         p = Diff#diff.showfunction,
         reverse = Diff#diff.reverse,
         w = Diff#diff.ignoreallspace,
         b = Diff#diff.ignorespacechange,
         'B' = Diff#diff.ignoreblanklines,
         'U' = Diff#diff.unified,
         stat = Diff#diff.stat,
         'S' = Diff#diff.subrepos,
         'I' = Diff#diff.include,
         'X' = Diff#diff.exclude
        };

convert(Add=#add{}) ->
    #internal_add{
         n = Add#add.dryrun,
         'S' = Add#add.subrepos,
         'I' = Add#add.include,
         'X' = Add#add.exclude
         };

convert(Tag=#tag{})->
    #internal_tag{
       names = Tag#tag.names,
       r = Tag#tag.rev,
       m = Tag#tag.message,
       f = Tag#tag.force,
       l = Tag#tag.local,
       remove = Tag#tag.remove,
       d = Tag#tag.date,
       u = Tag#tag.user
      };

convert(Branch=#branch{}) ->
    #internal_branch{
       name = Branch#branch.name,
       f = Branch#branch.force,
       'C' = Branch#branch.clean
      };

convert(Push=#push{}) ->
    #internal_push{
       dest = Push#push.dest,
       r = Push#push.rev,
       f = Push#push.force,
       'B' = Push#push.bookmark,
       b = Push#push.branch,
       new_branch = Push#push.newbranch,
       e = Push#push.ssh,
       remotecmd = Push#push.remotecmd,
       insecure = Push#push.insecure
       }.

error_write(Data) ->
    put(error_data,Data).

get_excluded_list(Record,Excluded_list)->
    emercurial_misc:get_excluded_list(Record,Excluded_list).

get_out()->
    get(out_data).

get_error()->
    get(error_data).

get_buffer()->
    case get(buffer) of
        undefined ->
            <<>>;
        Other ->
            Other
    end.

is_i_or_l('I')->
    true;

is_i_or_l('L')->
    true;

is_i_or_l(_) ->
    false.

gen_error(Args,Return,Out,Error)->
    emercurial_misc:generate_mercurial_error(Args,Return,Out,Error).


process_out(Out)->
    List_b = binary:split(Out,<<" ">>,[global]),
    Last = lists:last(List_b),
    [Rev,Node] = binary:split(Last,<<":">>),
    Rev_a = list_to_atom(binary_to_list(Rev)),
    Node_a = list_to_atom(lists:sublist(binary_to_list(Node),1,size(Node)-1)),
    {Rev_a,Node_a}.

out_write(Data)->
    case get(out_data) of
        undefined -> 
            New_data = Data;
        Value ->
            New_data = <<Value/binary,Data/binary>>
                end,
    put(out_data,New_data).

raw_command(State,#raw_command{args=Args,prompt=Prompt,input=Input,error_handler=Error_handler})->
    Out_channels=[{'r',fun error_write/1},{'o',fun out_write/1}],
    clear_out(),
    clear_error(),
    case Prompt of
        none ->
            In_channels=[];
        _ ->
            Fun = fun(Size)->
                          reply = Prompt(Size,get(out_data)),
                          reply
                  end,
            In_channels=[{'L',Fun}]
    end,
    case Input of
        none ->
            New_in_channels = In_channels;
        _ ->
            New_in_channels = [{'I',Input}|In_channels]
    end,
    try
        case run_command(State,Args,New_in_channels,Out_channels) of
            0 ->
                get_out();
            Return->
                case Error_handler of
                    none ->
                        exit(emercurial_misc:generate_mercurial_error(Args,
                                                                      Return,
                                                                      get_out(),
                                                                      get_error()));
                    _->
                        Error_handler(Return,get_out(),get_error())
                end
        end
    catch 
        Error:Reason->
            case Error_handler of
                none ->
                    error_logger:info_report([client_raw_command_1,Error,Reason]),
                    emercurial_misc:generate_mercurial_error(
                      Args,"",get_out(),atom_to_list(Error) 
                      ++ love_misc:to_binary(term_to_binary(Reason))),
                    error_logger:info_report([erlang:get_stacktrace()]);
                _->
                    Error_handler("",get_out(),Reason)
            end
    end. 


read_channel(Port)->
    <<A:1/binary,Size:32/unsigned>> = read_data(Port,5),
    Channel = list_to_atom(binary_to_list(A)),
    case is_i_or_l(Channel) of
        true ->
            {Channel,Size};
        _ ->
            Data = read_data(Port,Size),
            {Channel,Data}
    end.

read_data(Port,Size)->
    %%Buffer = get(buffer),
    Buffer = get_buffer(),
    {Result,New_buffer} = read_data(Port,Size,Buffer),
    put(buffer,New_buffer),
    Result.

read_data(Port,Size,Buffer)->
    %%Buffer = get(buffer),
    Size_of_buffer = size(Buffer),
    case Size_of_buffer >=  Size of
        true ->
            Left_size = Size_of_buffer-Size,
            <<Result:Size/binary,Rest:Left_size/binary>> = Buffer,
            %%put(buffer,Rest);
            {Result,Rest}; %%返回数据，与buffer内容
        _ ->
            receive
                {Port,{data,Data}}->
                    read_data(Port,Size,<<Buffer/binary,Data/binary>>);
                {Port,Error} ->
                   %% exit({read_data_error,no_data,Error})
                    exit({mercurial_server_error,Error})
            after ?RECEIVE_TIMEOUT ->
                    %%exit({read_data_error,timeout,?RECEIVE_TIMEOUT})
                    exit({mercurial_server_error,<<"Timeout">>})
            end
    end.

read_hello(State = #state{port=Port,encoding=Encoding})->
    {'o',Data} = read_channel(Port),
    Tuple_list = emercurial_misc:binary_to_tuple_list(Data,<<$\n>>,<<":">>),
    Capabilities = proplists:get_value('capabilities',Tuple_list),
    case Capabilities of
        undefined ->
            Cap_list = [],
            exit({mercurial_response_error,"bad hello message: expected capabilities:",Data});
        _ ->
            Cap_list = emercurial_misc:atom_to_atom_list(Capabilities," "),
            true = emercurial_misc:list_contain(Cap_list,'runcommand')                        
    end,
    Encoding_new = proplists:get_value('encoding',Tuple_list),
    if
        Encoding_new =/= Encoding ->
            exit({mercurial_response_error,"different coding type:" 
                  ,Capabilities});
        true ->
            ok
    end,
    New_state = State#state{capabilities=Cap_list},
    New_state.               

run_command(State=#state{port=Port},Args, In_channels,Out_channels)->
    B = emercurial_misc:get_run_command_binary(Args),
    Data = <<"runcommand",$\n,B/binary>>,
    write_data(Port,Data),
    run_command_internal(State,In_channels,Out_channels).

run_command_internal(State=#state{port=Port},In_channels,Out_channels)->
    {Channel,Data} = read_channel(Port),
    run_command_internal(Channel,Data,State,In_channels,Out_channels).

run_command_internal(Channel,Data,State=#state{port=Port},In_channels,Out_channels) 
  when Channel=='I' orelse Channel=='L'->
    Fun = proplists:get_value(Channel,In_channels),
    write_block(Port,Fun(Data)),
    run_command_internal(State,In_channels,Out_channels);

run_command_internal(Channel,Data,State,In_channels,Out_channels)
  when Channel=='o' orelse Channel=='e'->
    Fun = proplists:get_value(Channel,Out_channels),
    Fun(Data),
    run_command_internal(State,In_channels,Out_channels);

run_command_internal('r',<<Data:32/unsigned>>,_State,_,_) ->
    error_logger:info_report([run_command_internal_r,Data]),
    Data;

run_command_internal(Channel,_,_,_,_) when Channel >= 'A' andalso Channel =< 'A'  ->
    exit({channel_response_unknown});

run_command_internal(_,_,State,In_channels,Out_channels) ->
    run_command_internal(State,In_channels,Out_channels).

write_data(Port,Data)->
    port_command(Port,Data).

write_block(Port,Data)->
    Size = size(Data),
    write_data(Port,<<Size:32/unsigned,Data>>).

process_tags(List)->
    process_tags(List,[]).

process_tags([],Result)->
    lists:reverse(Result);

process_tags([Line|Rest],Result) ->
    B = binary_to_list(Line),
    A = process_tags_line(B),
    process_tags(Rest,[A|Result]).

process_tags_line(New_list ++"local")->
    %% case List of
    %%     Data ++ " local" ->
    %%         New_list = Data;
    %%     _ ->
    %%         New_list = List
    %% end,
    [Name,Part2] = string:tokens(Data," "),
    [Rev,Node] = string:tokens(Part2,":"),
    {trim(Name),love_misc:to_integer(Rev),
     node,New_list}.
