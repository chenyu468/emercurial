%%%-------------------------------------------------------------------
%%% @author chen yu <>
%%% @copyright (C) 2012, chen yu
%%% @doc
%%% modified from python-hglib/hglib/util.py
%%% @end
%%% Created :  6 Sep 2012 by chen yu <>
%%%-------------------------------------------------------------------
-module(emercurial_misc).

-export([cmdbuilder/3,get_excluded_list/2]).
-export([list_to_command/1]).
%%test
-export([cmd_process/1,process_key_value/2,list_contain/2,binary_to_atom_list/2,
         binary_to_tuple_list/3]).
-export([generate_mercurial_error/4,generate_value_error/1,generate_revision/1,
         receive_data/1,receive_data/2,atom_to_atom_list/2]).
-export([binary_to_tuple/2,run_command_cmdbuilder/3,get_run_command_binary/1,
         replace/3,generate_log_revision/1]).
-export([skiplines/2]).
%%         binary_to_tuple_list/3]).
%%-compile([export_all]).

-include("emercurial.hrl").
?R2P(clone);
?R2P(internal_clone);
?R2P(internal_log);
?R2P(internal_commit);
?R2P(internal_update);
?R2P(internal_parents);
?R2P(internal_cat);
?R2P(internal_diff);
?R2P(internal_add);
?R2P(internal_tag);
?R2P(internal_branch);
?R2P(internal_push);
?R2P(internal_tags);
?R2P(log);
?R2P(commit);
?R2P(update);
?R2P(parents);
?R2P(cat);
?R2P(diff);
?R2P(add);
?R2P(tag);
?R2P(branch);
?R2P(push).


%%%===============================================
%%% API
%%%===============================================

atom_to_atom_list(Atom, Seperator)->
    List = atom_to_list(Atom),
    List_b = string:tokens(List,Seperator),
    Cap_list_c = lists:map(fun(X)->
                                   list_to_atom(string:strip(X,both))
                           end, List_b),
    Cap_list_c.

binary_to_atom_list(List,Seperator)->
    Cap_list_b = binary:split(List,Seperator,[global]),
    Cap_list_c = lists:map(fun(X)->
                                          list_to_atom(string:strip(binary_to_list(X),both))
                                 end, Cap_list_b),
    Cap_list_c.

binary_to_tuple_list(List,Seperator,Tuple_seperate)->
    List_b = binary:split(List,Seperator,[global]),
    List_c = lists:map(fun(X)->
                                   binary_to_tuple(X,Tuple_seperate)
                       end, List_b),
    List_c.

binary_to_tuple(Binary,Tuple_seperate)->
    [Head,Tail]=binary:split(Binary,Tuple_seperate),
    {list_to_atom(string:strip(binary_to_list(Head))),
     list_to_atom(string:strip(binary_to_list(Tail)))}.

cmdbuilder(Name, Args_list, Kwargs_list)->
    List_a = [
              process_key_value(Key,Value)
              ||
                 {Key,Value}
                     <- Kwargs_list],
    Kw_list_a = lists:flatten(List_a),
    List_b = [
              process_arg(Arg)
              ||
                 Arg
                     <- Args_list],
    List_c = lists:flatten(List_b),
    List_d = lists:flatten([Name,Kw_list_a,List_c]),
    List_e = lists:map(fun atom_to_list/1,List_d),
    String = string:join(List_e," "),
    String.

cmd_process(Command)->
    Port_options =  [stream, {line, 4096}, binary, exit_status,stderr_to_stdout, hide],
    Port = open_port({spawn, Command}, Port_options),
    receive_data(Port).

get_excluded_list(Record,Excluded_list)->
    List = record_to_proplist(Record),
    %%List = ?R2P(Record),
    lists:foldl(fun(X,L)->
                      proplists:delete(X,L)
              end, 
              List,Excluded_list).

generate_value_error(Value)->
    #mercurial_value_error{value=Value}.

generate_mercurial_error(Args,Ret,Out,Error)->
    #mercurial_command_error{args=Args,ret=Ret,out=Out,error=Error}.

generate_log_revision(List)->
    generate_log_revision(List,[]).

generate_log_revision([],Result)->
    lists:reverse(Result);

generate_log_revision(List,Result)->
    error_logger:info_report([misc_log_revision_1,List]),
    Revision = lists:sublist(List,7),
    A = generate_revision(Revision),
    case length(List) >= 7 of
        true ->
            Rest = lists:sublist(List,8,length(List)),
            generate_log_revision(Rest,[A|Result]);           
        false ->
            generate_log_revision([],[A|Result])
    end.
  

generate_revision(List)->
    error_logger:info_report([misc_generate_revision_1,List]),
    List_b = lists:map(fun (X)->
                           list_to_atom(binary_to_list(X))
                               end, List),
    lists:zip(?REVISION,List_b).

get_run_command_binary(Args)->
    error_logger:info_report([get_run_command_binary,Args]),
    List = lists:map(fun atom_to_list/1,Args),
    String = string:join(List,[$\0]),
    Size = length(String),
    Binary = list_to_binary(String),
    <<Size:32/unsigned,Binary/binary>>.

list_contain([],_)->
    false;

list_contain([Element|_],Element)->
    true;

list_contain([_|Rest],Element) ->
    list_contain(Rest,Element).     

list_to_command(List)->
    List_e = lists:map(fun atom_to_list/1,List),
    String = string:join(List_e," "),
    String.

process_arg(none)->
    [];

process_arg(Other) ->
    [Other].

process_key_value(_,none)->
    [];

process_key_value(_,false)->
    [];

process_key_value(Key,Value) ->
    New_key = replace(Key,$_,$-),
    New_key_a = process_key(New_key),
    process_key_value_internal(New_key_a,Value).

%% process_key_value_internal(Key,Value) when is_boolean(Value)->
%%     Key;
process_key_value_internal(Key,Value) when is_list(Value) ->
    process_key_value_internal_a(Key,Value);

process_key_value_internal(Key,true)->
    [Key];
    
process_key_value_internal(Key,Value) ->
    [Key,Value].

process_key(Key) when Key =/= '-' ->
    List = atom_to_list(Key),
    List_b = case length(List) of
        1 ->
            "-" ++ List;
        _->
            "--" ++ List
    end,
    list_to_atom(List_b);

process_key(Other) ->
    Other.

%%=======================
%% 解决list的问题
%%=======================
process_key_value_internal_a(Key,Value_list)->
    process_key_value_internal_a(Key,Value_list,[]).

process_key_value_internal_a(_Key,[],Result)->
    lists:reverse(Result);
process_key_value_internal_a(Key,[Value|Rest],Result) ->
    process_key_value_internal_a(Key,Rest,[Value,Key|Result]).



receive_data(Port)->
    receive_data(Port,[]).

receive_data(Port,Acc)->
    receive
        {Port, {exit_status,Status}}->
            error_logger:info_report([exit_status,Status]),
            {Acc,Status};
        {Port,{data,{noeol,Data}}}->
            receive_data(Port,[Data|Acc]);
        {Port,{data,{eol,Data}}}->
            Result = lists:reverse(Acc,Data),
            error_logger:info_report([receive_data,1,Result]),
            receive_data(Port,[Data|Acc]);
        {Port, Data}  ->
            error_logger:info_report([cmd_process,2,Data]),
            receive_data(Port,[Data|Acc]);                
        {Port, Error} ->
            catch port_close(Port),
            error_logger:info_report([port,error,Error]),
            {Acc,Error}
    after 3000 ->
            catch port_close(Port),
            error_logger:info_report([port,timeout,3000]),
            {Acc,time_out}
    end.   

run_command_cmdbuilder(Name, Args_list, Kwargs_list)->
    List_a = [
              process_key_value(Key,Value)
              ||
                 {Key,Value}
                     <- Kwargs_list],
    Kw_list_a = lists:flatten(List_a),
    List_b = [
              process_arg(Arg)
              ||
                 Arg
                     <- Args_list],
    List_c = lists:flatten(List_b),
    List_d = lists:flatten([Name,Kw_list_a,List_c]),
    List_d.
    
%%replace
replace(Key,Original,Replace) when is_atom(Key)->
    List = atom_to_list(Key),
    New_list = replace(List,Original,Replace),
    list_to_atom(New_list);

replace(List,Original,Replace)->
    New_list = lists:foldl(fun(X,L)->
                        New_x = case X of
                            Original ->
                                Replace;
                            Other ->
                                Other
                                    end,
                        [New_x|L]
                end,[],List),    
    lists:reverse(New_list).

skiplines(Binary,Prefix)->
    List = binary:split(Binary,<<$,>>,[global]),
    %%error_logger:info_report([skiplines_1,Binary,List]),
    lists:dropwhile(fun(X)->
                            case binary:match(X,[Prefix]) of
                                nomatch ->
                                    false;
                                _ ->
                                    true
                            end
                    end,List).




    
