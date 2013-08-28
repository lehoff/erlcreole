%% Author: torben
%% Created: Sep 1, 2009
%% Description: TODO: Add description to test
-module(test).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([test/1,
		 chdir/0, usbdir/0,
		 gen/0, pregen/0,
		 compile/0]).

%%
%% API Functions
%%

chdir() ->
	c:cd("../../hula/erlcreole-0.1/src").

usbdir() ->
	c:cd("f:/hula/erlcreole-0.1/src").

pregen() ->
	{ok,creole_gen} = c:c(creole_gen).

gen() ->
	pregen(),	
	ok = peg_gen:file("creole.peg",[{transform_module,creole_gen}]),
	compile().

compile() ->
	c:c(creole).



%%
%% TODO: Add description of test/function_arity
%%
test(1) -> 
	creole:file("test/test_bold.txt");
test(2) ->
	creole:file("test/test_full.txt");
test(Filename) when is_list(Filename) ->
	creole:file("test/" ++ Filename ).


%%
%% Local Functions
%%

