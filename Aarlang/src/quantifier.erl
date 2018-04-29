%% @author aaron
%% @doc @todo Add description to quantifier.
-module(quantifier).
-author("Aaron Eberhart").

-record(quantifier,{type}).


%% ====================================================================
%% API functions
%% ====================================================================
-export([newQuantifier/1,showQuantifier/1]).

newQuantifier(Quantifier) -> 
	if Quantifier =:= 1 -> existential();
	   Quantifier =:= 2 -> universal();
	   true -> quantifierError()
	end.

showQuantifier(Quantifier) ->
	if Quantifier#quantifier.type > 0 -> io:format("~s",[Quantifier#quantifier.type]);
	   true -> quantifierError()
	end.

%% ====================================================================
%% Internal functions
%% ====================================================================
existential() -> #quantifier{type="E"}.
universal() -> #quantifier{type="A"}.

quantifierError() -> io:format("\"Not a known Quantifier.\"~n").