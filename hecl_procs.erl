-module(hecl_procs).
-export([addproc/3, register_procs/1, fetchproc/2]).

-include_lib("hecl.hrl").

%% Add a procedure, given the Interp, name of the function, and a
%% function.  The function must take two arguments an Interp, and a
%% list of regular arguments.

addproc(Interp, Name, Fun) ->
    ets:insert(Interp#interp.commands, {Name, Fun}).


%% Get a function, given its name.

fetchproc(Interp, Name) ->
    hd(ets:lookup(Interp#interp.commands, Name)).


%% Register built-in procs.  These can, of course, be overridden via
%% 'proc'.

register_procs(Interp) ->

    %% Get bool value of argument.
    addproc(Interp, "bool",
	    fun(I, A) ->
		    #thing{type=bool, val=hecl:tobool(hd(A))}
	    end),

    addproc(Interp, "break",
	    fun(I, A) ->
		    throw(break)
	    end),

    addproc(Interp, "float",
	    fun(I, A) ->
		    #thing{type=float, val=hecl:tofloat(hd(A))}
	    end),

    addproc(Interp,
	    "foreach",
	    fun(I, A) ->
		    Varname = hecl:tostr(lists:nth(1, A)),
		    List = hecl:tolist(lists:nth(2, A)),
		    Body = hecl:tostr(lists:nth(3, A)),
%%%io:format("Varname ~p, List ~p, Body ~p~n", [Varname, List, Body]),
		    lists:map(fun(X) ->
				      hecl:setvar(I, Varname, X),
				      hecl:exec(I, Body)
			      end, List)
	    end),
    addproc(Interp, "format",
	    fun(I, A) ->
		    io:format("Yeah, hello~n"),
		    FmtStr = hecl:tostr(hd(A)),
		    case length(A) of
			1 ->
			    #thing{type=str, val=io_lib:format(FmtStr, [])};
			2 ->
			    Args = lists:map(fun(X) ->
						     io:format("FORMAT ~p~n", [X]),
						     X#thing.val
					     end, tl(A)),
			    %%io:format("FmtStr ~p Args ~p ~n", [FmtStr, Args]),
			    #thing{type=str, val=io_lib:format(FmtStr, Args)}
		    end
	    end),


    addproc(Interp, "if",
	    fun(I, A) ->
		    Code = hd(A),
		    %%io:format("Attempting to hecl:exec ~p~n", [Code]),
		    case hecl:tobool(hecl:exec(I, hecl:tostr(Code))) of
			true ->
			    %%io:format("If condition true, hecl:execing ~p~n",
			    %%[lists:nth(2, A)]),
			    hecl:exec(I, hecl:tostr(lists:nth(2, A)));
			false ->
			    case length(A) of
				2 ->
				    false;
				4 ->
				    hecl:exec(I, hecl:tostr(lists:nth(4, A)))
			    end
		    end
	    end),

    %% int: NumStr -> Integer
    %% This needs types, too.

    addproc(Interp, "index",
	    fun(I, A) ->
		    List = hecl:tolist(hd(A)),
		    Idx = hecl:toint(lists:nth(2, A)),
		    lists:nth(Idx, List)
	    end),

    addproc(Interp, "int",
	    fun(I, A) ->
		    #thing{type=int, val=hecl:toint(hd(A))}
	    end),


    addproc(Interp, "list",
	    fun(I, A) ->
		    %%io:format("A is ~p~n", [A]),
		    #thing{type=list, val=A}
	    end),

    addproc(Interp, "proc",
	    fun(I, A) ->
		    Name = hecl:tostr(lists:nth(1, A)),
		    Args = hecl:tolist(lists:nth(2, A)),
		    Body = hecl:tostr(lists:nth(3, A)),
		    procfunc(I, Name, Args, Body)
	    end),
    addproc(Interp,
	    "puts",
	    fun(I, A) ->
		    io:format("~s", [hecl:tostr(hd(A))]),
		    io:format("~n")
	    end),


    %% rename oldname newname
    addproc(Interp, "rename",
	    fun(I, A) ->
		    OldName = hecl:tostr(lists:nth(1, A)),
		    NewName = hecl:tostr(lists:nth(2, A)),
		    {_, Fun} = fetchproc(I, OldName),
		    addproc(I, NewName, Fun),
		    ets:delete(I#interp.commands, OldName)
	    end),

    addproc(Interp,
	    "set",
	    fun(I, A) ->
		    %%io:format("A is ~p~n", [A]),
		    Varname = hecl:tostr(hd(A)),
		    case length(A) of
			1 ->
			    element(2,
				    hd(ets:lookup(
					 I#interp.vars, Varname)));
			2 ->
			    Val = lists:nth(2, A),
			    %%io:format("Setting ~p to ~p~n", [Varname, Val]),
			    hecl:setvar(I, Varname, Val)
		    end
	    end),

    %% sort list

    addproc(Interp, "sort",
	    fun(I, A) ->
		    List = lists:sort(hecl:tolist(hd(A))),
		    #thing{type=list,
			   val = List}
	    end),


    %% true: -> true

    addproc(Interp, "true",
	    fun(I, A) ->
		    #thing{type=bool,
			   val = true}
	    end),


    addproc(Interp, "while",
	    fun(I, A) ->
		    Cond = hecl:tostr(lists:nth(1, A)),
		    Body = hecl:tostr(lists:nth(2, A)),
		    whilefunc(I, Cond, Body)
	    end),
    addproc(Interp, "==",
	    fun (I, A) ->
		    #thing {type=bool,
			    val=(hecl:tostr(hd(A)) ==
				 hecl:tostr(lists:nth(2, A)))}
	    end),

    addproc(Interp, "+",
	    fun (I, A) ->
		    #thing{type=int, val=(hecl:toint(hd(A)) +
					  hecl:toint(lists:nth(2, A)))}
	    end),
    addproc(Interp, "-",
	    fun (I, A) ->
		    #thing{type=int, val=(hecl:toint(hd(A)) -
					  hecl:toint(lists:nth(2, A)))}
	    end),
    addproc(Interp, "/",
	    fun (I, A) ->
		    #thing{type=int, val=(hecl:toint(hd(A)) /
					  hecl:toint(lists:nth(2, A)))}
	    end),
    addproc(Interp, "<",
	    fun (I, A) ->
		    #thing{type=bool, val=(hecl:toint(hd(A)) <
					   hecl:toint(lists:nth(2, A)))}
	    end),
    addproc(Interp, ">",
	    fun (I, A) ->
		    #thing{type=bool, val=(hecl:toint(hd(A)) >
					   hecl:toint(lists:nth(2, A)))}
	    end).


%% helper function for the proc proc.

procfunc(I, Name, Args, Body) ->
    %%io:format("Name ~p, Args ~p, Body ~p~n",
	%%      [Name, Args, Body]),

    addproc(
      I, Name,
      fun(FI, A) ->
	      %% Create the variables.
	      lists:foldl(
		fun(X, AccIn) ->
			E = hecl:tostr(X),
			hecl:setvar(
			  FI, E, lists:nth(AccIn, A)),
			AccIn + 1
		end, 1, Args),
	      %%io:format("Body is ~p~n", [Body]),
	      hecl:exec(FI, Body)
	      %% FIXME - we need to deal with the idea of stack frames.
      end).

%% helper function for the while proc.

whilefunc(Interp, Cond, Body) ->
 %%io:format("Cond ~p Body ~p", [Cond, Body]),
    case hecl:tobool(hecl:exec(Interp, Cond)) of
	true ->
	    Res = (catch hecl:exec(Interp, Body)),
	    if
		Res == break ->
		    break;
		true ->
		    whilefunc(Interp, Cond, Body)
	    end;
	false ->
	    false
    end.
