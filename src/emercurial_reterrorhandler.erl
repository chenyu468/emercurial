-module(emercurial_reterrorhandler).

-export([init/2,handle/3,nonzero/0]).
-include("emercurial.hrl").


init(Args,Allowed)->
    put(reterrorhandler_args,Args),
    put(reterrorhandler_ret,0),
    case Allowed of
        none ->
            put(reterrorhandler_allowed,[1]);
        _ ->
            put(reterrorhandler_allowed,Allowed)            
    end,
    ok.

handle(Ret,Out,Error)->
    Allowed = get(reterrorhandler_allowed),
    put(reterrorhandler_ret,Ret),
    case emercurial_misc:list_contain(Allowed,Ret) of
        true->
            Out;
        false ->
            Args = get(reterrorhandler_args),
            exit(emercurial_misc:generate_mercurial_error(Args,Ret,Out,Error))
    end.

nonzero()->
    Ret = get(reterrorhandler_ret),
    Ret == 0.

    
