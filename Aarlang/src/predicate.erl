% @author aaron
%% @doc @todo Add description to 'Predicate'.

-module('predicate').
-author("Aaron Eberhart").

%% ====================================================================
%% API functions
%% ====================================================================
-export([newPredicate/1,showPredicate/1]).

newPredicate(Predicate) ->
	if is_list(Predicate), length(Predicate) =:= 2 -> matchPredicate(Predicate);
	   true -> creationFail()
	end.

showPredicate(Predicate) ->
	case Predicate of
		{{type,Type},{name,Name},{terms,Terms},{ground,_}} -> io:format("~s("++formatTerms(Terms)++"):~s~n",[Name,atom_to_list(Type)]);
		true -> "Not a known predicate."
	end.
	
%% ====================================================================
%% Internal functions
%% ====================================================================
constant(Name,Terms,Ground) -> 
	{{type,constant},{name,Name},{terms,Terms},{ground,Ground}}.
concept(Name,Terms,Ground) -> 
	{{type,concept},{name,Name},{terms,Terms},{ground,Ground}}.
role(Name,Terms,Ground) -> 
	 {{type,role},{name,Name},{terms,Terms},{ground,Ground}}.
nAryPredicate(Name,Terms,Ground) -> 
	{{type,list_to_atom("arity-"++integer_to_list(length(Terms)))},{name,Name},{terms,Terms},{ground,Ground}}.

matchPredicate(Predicate) ->
	case Predicate of
		[_,Terms] when not is_list(Terms) -> creationFail();
		[Name,Terms] when Terms =:= [] -> constant(termSafe(Name),[],true);
		[Name,Terms] -> makePredicate([termSafe(Name),util:map(fun termSafe/1,Terms),isGround(Terms)])
	end.

makePredicate(Predicate) ->
	case Predicate of
		[Name,Terms,Ground] when length(Terms) =:= 1 -> concept(Name,Terms,Ground);
		[Name,Terms,Ground] when length(Terms) =:= 2 -> role(Name,Terms,Ground);
		[Name,Terms,Ground] when length(Terms) > 2 -> nAryPredicate(Name,Terms,Ground)
	end.

creationFail() -> 
	io:format("\"Not a valid predicate. Try this\" : [\"Name\",[Variables,atoms,\"Text\"]]~n").

isGround(Terms) ->
	case Terms of
		[Head|_] when length(Head) =< 1 -> false;
		[Head|Tail] when is_atom(Head) or is_number(Head) -> isGround(Tail);
		_ -> true
	end.

termSafe(Term) -> 
	case Term of 
		Term when is_atom(Term) -> Term; 
		Term when is_integer(Term) -> termSafe(integer_to_list(Term));
		Term when is_float(Term) -> [Head] = io_lib:format("~8s",[float_to_list(4.5)]),termSafe(Head);
		[H|_] when is_list(Term), length(Term) =:= 1 -> list_to_atom(H);
		[H|_] when not is_function(H), is_list(Term), length(Term) > 1 -> list_to_atom(Term);
		[H|T] when is_function(H) -> termSafe(util:map(H,T));
		_ -> 'null'
	end.

formatTerms(Terms) -> 
	case Terms of
		[Head] when length(Terms) =:= 1, is_atom(Head); is_integer(Head) -> io_lib:format("~w",[Head]);
		[Head] when length(Terms) =:= 1, is_list(Head) -> io_lib:format("\"~s\"",[Head]);
		[Head|Tail] when is_list(Head) -> io_lib:format("\"~s\",",[Head]) ++ formatTerms(Tail);
		[Head|Tail] -> io_lib:format("~w,",[Head]) ++ formatTerms(Tail)
	end.