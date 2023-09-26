-module(test_emoji).
 
-export([test_all/0]).
 
% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").
 
test_all() -> eunit:test(testsuite(), [verbose]).
 
testsuite() ->
    [ {"Basic behaviour", spawn,
       [ 
        test_start(),
        test_new_shortcode_success(),
        test_new_shortcode_error(),
        test_alias(),
        test_alias_2(),
        test_alias_error(),
        test_delete_success(),
        test_delete_nonexisting_shortcode(),
        test_lookup(),
        test_nonexisting_lookup(),
        test_analytics_success(),
        test_analytics_fail(),
        test_get_analytics_success(),
        test_get_analytics_fail(),
        test_remove_analytics_success(),
        test_stop_success()
       ]
      }
    ].
 
test_start() ->
    {"Start the server",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([]))
     end }.
 
test_new_shortcode_success() ->
    {"Register new shortcode - success",
     fun () ->
       {ok, E} = emoji:start([]),
       ?assertEqual(ok, emoji:new_shortcode(E, "package", <<"📦️"/utf8>>))
     end }.
 
test_new_shortcode_error() ->
    {"Register new shortcode - error",
     fun () ->
       {ok, E} = emoji:start([]),
       emoji:new_shortcode(E, "package", <<"📦️"/utf8>>),
       {error, R} = emoji:new_shortcode(E, "package", <<"📦️"/utf8>>),
       ?assertEqual("Shortcode already registered in emoji server", R)
     end }.
 
test_alias() ->
    {"Register an alias",
     fun () ->
       {ok, E} = emoji:start([]),
       emoji:new_shortcode(E, "package", <<"📦️"/utf8>>),
       ?assertEqual(ok, emoji:alias(E, "package", "package2"))
     end }.
 
test_alias_2() ->
    {"Create an alias for the shortcode and for the alias",
     fun () ->
       {ok, E} = emoji:start([]),
       emoji:new_shortcode(E, "package", <<"📦️"/utf8>>),
       emoji:alias(E, "package","newPackage"),
       ?assertEqual(ok, emoji:alias(E, "newPackage","newerPackage"))
     end }.

test_alias_error() ->
    {"Alias registration, shortcode not registered",
     fun () ->
       {ok, E} = emoji:start([]),
       {error, R} =  emoji:alias(E, "newPackage","newerPackage"),
       ?assertEqual(R, "Short1 is not registered")
     end }.
 
test_delete_success() ->
    {"Adding, deleting and adding the same shortcode",
     fun () ->
       {ok, E} = emoji:start([]),
       emoji:new_shortcode(E, "package", <<"📦️"/utf8>>),
       emoji:delete(E, "package"),
       ?assertEqual(ok, emoji:new_shortcode(E, "package", <<"📦️"/utf8>>))
     end }.
 
test_delete_nonexisting_shortcode() ->
    {"Deleting a nonexisting shortcode",
     fun () ->
       {ok, E} = emoji:start([]),
       emoji:delete(E, "package"),
       ?assertEqual(false, emoji:delete(E, "package"))
     end }.
 
test_lookup() ->
    {"Lookup shortcode",
     fun () ->
       {ok, E} = emoji:start([]),
       emoji:new_shortcode(E, "package", <<"📦️"/utf8>>),
       ?assertEqual({ok, <<"📦️"/utf8>>}, emoji:lookup(E, "package"))
     end }.
 
test_nonexisting_lookup() ->
  {"Lookup nonexisting shortcode",
    fun () ->
      {ok, E} = emoji:start([]),
      emoji:new_shortcode(E, "package", <<"📦️"/utf8>>),
      ?assertEqual(no_emoji, emoji:lookup(E, "notpackage"))
    end }.

test_analytics_success() -> 
  {"Register analytics for Short - success",
    fun () -> 
      {ok, E} = emoji:start([]), 
      emoji:new_shortcode(E, "package", <<"📦️"/utf8>>),
      ?assertEqual(ok, emoji:analytics(E, "package", fun(_, N) -> N+1 end, "Counter", 0))
    end }.

test_analytics_fail() ->
  {"Register analytics for Short - fail",
    fun () ->
      {ok, E} = emoji:start([]),
      ?assertEqual({error, "Analytics can only be created for shortcodes that already exist"}, emoji:analytics(E, "package", fun(_, N) -> N+1 end, "Counter", 0))
    end }.

test_get_analytics_success() ->
  {"Get analytics - success",
    fun () ->
      {ok, E} = emoji:start([]),
      emoji:new_shortcode(E, "package", <<"📦️"/utf8>>),
      emoji:analytics(E, "package", fun(_, N) -> N+1 end, "Counter", 0),
      ?assertEqual({ok,[{"Counter",0}]}, emoji:get_analytics(E, "package"))
    end }.

test_get_analytics_fail() ->
  {"Get analytics - fail",
    fun () -> 
      {ok, E} = emoji:start([]),
      emoji:new_shortcode(E, "package", <<"📦️"/utf8>>),
      ?assertEqual({error, "CHANGE THIS TEXT"}, emoji:get_analytics(E, "package"))
    end }.


test_remove_analytics_success() ->
  {"Remove analytics - success", 
    fun () ->
      {ok, E} = emoji:start([]),
      emoji:new_shortcode(E, "package", <<"📦️"/utf8>>),
      emoji:analytics(E, "package", fun(_, N) -> N+1 end, "Counter", 0),
      emoji:remove_analytics(E, "package", "Counter"),
      ?assertEqual({ok,[]}, emoji:get_analytics(E, "package"))
    end }.

test_stop_success() ->
  {"Stop the emojiserver - success",
    fun () -> 
      {ok, E} = emoji:start([]),
      ?assertEqual(ok, emoji:stop(E))
    end }.
