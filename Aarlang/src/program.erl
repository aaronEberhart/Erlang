%% @author Aaron Eberhart
%% @doc @todo Add description to program.

-module(program).
-author("Aaron Eberhart").

%% ====================================================================
%% API functions
%% ====================================================================
<<<<<<< HEAD
-export([function/1]).
=======
-export([list_plus_one/1, funky_add/2]).
>>>>>>> refs/remotes/origin/master

%% ====================================================================
%% External functions
%% ====================================================================
list_plus_one([ ]) -> 
	[ ];
list_plus_one([ H | T ]) when (H + 1) rem 5 == 0 -> 
	list_plus_one( T );
list_plus_one([ H | T ]) -> 
	[ H + 1 | list_plus_one( T ) ];
list_plus_one( _ ) -> 
	"Inappropriate Input".

	

funky_add(X,Y) ->
	if is_function(X) and is_function(Y) -> X() + Y();
	   is_function(X) -> X() + Y;
	   is_function(Y) -> X + Y();
	   true -> X + Y
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================

function( {X,_} ) -> 
	X, 
	second_result,
	X;
function( _ ) -> 
	os:cmd("sh /home/aaron/actr7/aactron.sh").

