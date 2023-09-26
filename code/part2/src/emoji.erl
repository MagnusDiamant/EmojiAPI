-module(emoji).
-compile(export_all).
%%-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2, state_deletion/3, delete_alias/4, setup/0, helperLoop/4,
  %%       analytics/5, get_analytics/2, remove_analytics/3, insert/3, get_analytics/1, retrieve_labels/2, 
    %%     stop/1, loop/3,retrieve_emoji_state/1,_sexists/3, retrieve_aliases/1, validate/5, get_function_from_worker/1,get_state_from_worker/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).

% Creates a new process which runs loop 
start(Initial) -> {ok, spawn(emoji, loop, [Initial, Initial, dict:new()])}. 


% Handles the lists of binaries, shortcodes and analytics 
% Essentially acts as the server 
loop(State, Aliases, Analytics) ->
    receive
         {From, state} ->
            From ! State,
            loop(State, Aliases, Analytics);
        {From, aliases} ->
            From ! Aliases,
            loop(State, Aliases, Analytics);
        {From, analytics} ->
            From ! Analytics,
            loop(State, Aliases, Analytics);
        {Short, Emo, new} ->
            loop([{Short, Emo}|State], Aliases, Analytics);
        {From, Short1, Short2, new_alias} ->
            try 
                New = [{Short2, Short1}|Aliases],
                From ! ok,
                loop(State, New, Analytics)
            catch
                error:Reason -> From ! {error, Reason}, 
                loop(State, Aliases, Analytics)
            end;
        {_From, Alias, Short, alias_deletion} ->
            loop(State, lists:delete({Alias, Short}, Aliases), Analytics);
        {X, delete_from_server} ->
            loop(lists:delete(X, State), Aliases, Analytics);
        {_From, ShortCode, Label, Function, Initial, save} ->
            Dict = dict:store(ShortCode, [], Analytics),
            loop(State, Aliases, dict:append(ShortCode, {Label, spawn(fun() -> server_helper(Label, Function, Initial) end)}, Dict));
        {_From, ShortCode, Label, Function, Initial, concat} ->
            loop(State, Aliases, dict:append(ShortCode, {Label, spawn(fun() -> server_helper(Label, Function, Initial) end)}, Analytics));
        {New_analytics, remove} ->
            loop(State, Aliases, New_analytics);
        _ ->
            io:format("No pattern matched in loop!"),
            loop(State, Aliases, Analytics)
    end.

% Helper function for making calculations 
server_helper(Label, Function, State) ->
    receive 
        {From, calculation} ->
            try
                New = Function(Label, State),
                From ! ok,
                server_helper(Label, Function, New)
            catch
                error:Reason -> 
                    From ! {error, Reason},
                    server_helper(Label, Function, State)
            end;
        {From, state} ->
            From ! State,
            server_helper(Label, Function, State);
        _ ->
            io:format("No case caught the term in ~s~n"),
            server_helper(Label, Function, State)
    end.

% Registers a new shortcode to a binary 
new_shortcode(E, Short, Emo) ->
    case checkIfExists(retrieve_emoji_state(E), Short) of
        true -> {error, "Shortcode already registered in emoji server"};
        _ -> E ! {Short, Emo, new}, ok
    end.


% Gets state (i.e. list of shortcodes and binaries)
retrieve_emoji_state(E) ->
    E ! {self(), state},
    receive
        State -> State
    end.

% Gets aliases
retrieve_aliases(E) ->
    E ! {self(), aliases},
    receive
        Alias -> Alias
    end.

% Gets analytics 
get_analytics(E) ->
    E ! {self(), analytics},
    receive
        Analytics -> Analytics
    end.

% Validates aliases and shortcodes before registering
validate(State, Aliases, Short1, Short2) -> 
    case checkIfExists(State, Short1) of
        false -> case checkIfExists(Aliases, Short1) of
                    false -> "Short1 is not registered";
                    true -> case checkIfExists(State, Short2) of
                        true -> "Short2 is a ShortCode";
                        _ -> case checkIfExists(Aliases, Short2) of
                                true -> "Short2 is already registered as an alias";
                                _ -> {ok, retrieve_short_alias(Short1, State, Aliases)}
                        end
                    end
                end;
        _ -> case checkIfExists(State, Short2) of
                true -> "Short2 is already registered as a ShortCode";
                _ -> case checkIfExists(Aliases, Short2) of
                        true -> "Short2 is already registered as an alias";
                        _ -> true
                    end
            end
    end.

% Retrieves shortcodes for aliases
retrieve_short_alias(Short, State, Aliases) ->
    case checkIfExists(State, Short) of
        true ->  Short;
        _ -> {_ShortAlias, Alias} = lists:keyfind(Short, 1, Aliases),
                retrieve_short_alias(Alias, State, Aliases)
    end.


% Inserts an alias for a shortcode 
insert(Aid, Short1, Short2) ->
    Aid ! {self(), Short1, Short2, new_alias},
    receive
        Message -> Message
    end.

% Uses validate and insert to update state with new alias
alias(E, Short1, Short2) ->
    case validate(retrieve_emoji_state(E), retrieve_aliases(E), Short1, Short2) of
        true ->  insert(E, Short1, Short2);
        {ok, RealShort} -> insert(E, RealShort, Short2);
        Error -> {error, Error}
    end.

% Deletes shortcode from state 
state_deletion(E, State, ShortCode) ->
    case lists:keyfind(ShortCode, 1, State) of
        false -> false;
        {ShortCode, OriginalShort} -> E ! {{ShortCode, OriginalShort}, delete_from_server}
    end.

% Deletes all aliases and shortcode for a given shortcode
delete(E, Short) ->
    Aliases = retrieve_aliases(E),
    State =retrieve_emoji_state(E),

    case checkIfExists(Aliases, Short) of
        true -> ShortCode = retrieve_short_alias(Short, State, Aliases), 
                alias_deletion(E, Aliases, ShortCode, 2),
                state_deletion(E, State, ShortCode); 
        false -> case checkIfExists(State, Short) of
                    true -> alias_deletion(E, Aliases, Short, 2), 
                            state_deletion(E, State, Short);
                    false -> false
                end
    end.
    
% Deletes only alias 
alias_deletion(E, Aliases, ShortAlias, Key) ->
    case lists:keyfind(ShortAlias, Key, Aliases) of
        false -> false;
        {Alias, Short} -> E ! {self(), Alias, Short, alias_deletion},
                            alias_deletion(E, retrieve_aliases(E), ShortAlias, Key)
    end.

% Checks if a shortcode is registered 
checkIfExists(State, Short) ->
    {Shorts, _Binaries} = lists:unzip(State),
    lists:member(Short, Shorts).

% Looks up a shortcode 
lookup(E, Short) ->
    State =retrieve_emoji_state(E),
    Aliases = retrieve_aliases(E),
    Analytics = get_analytics(E),
    case checkIfExists(State, Short) of
        false -> case checkIfExists(Aliases, Short) of
                false -> no_emoji;
                _ -> {_ShortA, ShortO} = lists:keyfind(Short, 1, Aliases), 
                    {ShortCode, Emoji} = lists:keyfind(ShortO, 1, State), 
                    case find_analytics_shortcode(ShortCode, Analytics) of
                        true -> evaluate_functions(E, ShortCode);
                        false -> false
                    end,     
                    {ok, Emoji}
            end;
        _ -> {ShortCode, Emoji} = lists:keyfind(Short, 1, State), 
            case find_analytics_shortcode(ShortCode, Analytics) of
                true -> evaluate_functions(E, ShortCode);
                false -> false
            end,    
            {ok, Emoji}
    end.

% Finds shortcode in state, and evaluates associated functions
evaluate_functions(E, ShortCode) ->
    {ok, Ls} = dict:find(ShortCode, get_analytics(E)),
    lists:foreach(fun({_, Pid}) ->  
                            Pid ! {self(), calculation},
                                receive
                                    Reply -> Reply
                                end 
                                end, Ls).

% Creates analytics function for a given shortcode 
analytics(E, ShortCode, Function, Label, Initial) ->
    Analytics = get_analytics(E),
    State =retrieve_emoji_state(E),
    case checkIfExists(State, ShortCode) of
        true ->
            case dict:is_key(ShortCode, Analytics) of
                false -> E ! {self(), ShortCode, Label, Function, Initial, save}, 
                    receive
                       _ -> ok
                    end;
                true -> E ! {self(), ShortCode, Label, Function, Initial, concat},                   
                    receive
                       _ -> ok
                    end
            end;
        false -> "Analytics can only be created for shortcodes that already exist"
    end.

% Gets the result of all analytics functions 
get_analytics(E, ShortCode) ->
    {ok, Ls} = dict:find(ShortCode, get_analytics(E)),    
    States = lists:map(fun({Label, Pid}) -> 
                        State = retrieve_emoji_state(Pid),
                        {Label, State}
                         end, Ls),
    {ok, States}.

% Check if a shortcode exists in analytics
find_analytics_shortcode(ShortCode, Analytics) ->
    case dict:find(ShortCode, Analytics) of 
        {ok, _} -> true;
        _ -> false
    end.

% Removes analytics function with a given Label 
remove_analytics(E, ShortCode, Label) ->
    Analytics = get_analytics(E),
    case find_analytics_shortcode(ShortCode, Analytics) of
        true ->
            Ls = dict:fetch(ShortCode, Analytics),
            P = lists:keyfind(Label, 1, Ls),
            case P of
                false -> "Could not find a shortcode associated with this label";
                _ -> E ! {dict:update(ShortCode, fun(Lst) -> lists:delete(P, Lst) end, Analytics), remove} 
            end;
        false -> "The shortcode is not registered in remove_analytics."
    end.

% Stops a process and all associated processes 
stop(E) -> 
    PList = dict:fetch_keys(get_analytics(E)),
    Analytics = get_analytics(E),
    lists:foreach(fun(X) -> lists:foreach(fun({_Label, Pid}) -> 
                                            catch exit(Pid, kill) end, dict:fetch(X, Analytics)) end, PList),

    catch exit(kill),
    ok.


    