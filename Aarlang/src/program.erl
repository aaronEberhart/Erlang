%% @author Aaron Eberhart
%% @doc @todo Add description to program.

-module(program).
-author("Aaron Eberhart").

%% ====================================================================
%% API functions
%% ====================================================================
-export([function/1]).



%% ====================================================================
%% Internal functions
%% ====================================================================

function( {X,_} ) -> 
	X, 
	second_result,
	X;
function( _ ) -> 
	os:cmd("sh /home/aaron/actr7/aactron.sh").

