-module(async).

-export([new/2, wait/1, poll/1, loop/3, helperFunction/3]).


% Spawns a main server and a Supervisor and computes Fun(arg) through a helperFunction
new(Fun, Arg) -> 
    spawn(fun () -> 
            Worker = self(),
            Helper = spawn(fun () -> 
                            helperFunction(Fun, Arg, Worker)
            end),
            loop(Helper, 0, 0) 
        end).

% Function that waits for an asynchronous action to complete    
wait(Aid) -> 
    Aid ! {self(), wait}, 
    receive 
        {0, 0} -> wait(Aid); 
        {{ok, Res}, 1} -> Res; 
        {{exception, Ex}, 1} -> {exception, Ex};
        _ -> noCaseMatchedTheMessage
    end. 

% Poll asks if a process is done or not
poll(Aid) -> 
    Aid ! {self(), is_done}, 
    receive 
        {0, 0} -> nothing;
        {{exception, Ex}, 1} -> {exception, Ex};
        {{ok, Res}, 1} -> {ok, Res};
        _ -> noCaseMatchedTheMessage
    end.
% Loop to pattern match the different messages
loop(Id, R, D) -> 
    receive 
        {From, is_done} -> 
            From ! {R, D}, 
            loop(Id, R, D); 
        {From, wait} -> 
            From ! {R, D}, 
            loop(Id, R, D); 
        {Res, res} -> 
            loop(Id, {ok, Res}, 1); 
        {Ex, exception} -> 
            loop(Id, {exception, Ex}, 1); 
        _ -> 
            loop(Id, R, D)
    end. 

% Helper function to compute Fun(arg)
helperFunction(Fun, Arg, Id) ->
    try Id ! {Fun(Arg), res}
    catch 
        throw:Term -> Id ! {Term, exception};
        exit:Reason -> Id ! {Reason, exception};
        error:Reason -> Id ! {Reason, exception};
        _:_ -> Id ! {"No matched case", exception}    
    end. 