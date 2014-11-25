%%%-------------------------------------------------------------------
%%% File    : hecl.erl
%%% Author  : David N. Welton <davidw@dedasys.com>
%%% Description : A small, extensible Tcl-like language interpreter.
%%%
%%% Copyright 2014 David N. Welton


-module(hecl).
-export([parse/1, make_interp/0, del_interp/1, test/0, parsetest/0,
	 interlieve/2, exec/2, tolist/1, tofloat/1, toint/1, tostr/1,
	 tobool/1, setvar/3, string2list/1, thing/2, heclize/1]).
-author('davidw@dedasys.com').

-include_lib("hecl.hrl").

%% checklevel -- do the right thing depending on whether we are inside
%% a { or [.

checklevel(Type, C, Rest, Level, Acc) ->
    if
	Level > 1 ->
	    do_parse(Type, Rest, Level - 1, Acc ++ [C]);
	true ->
	    {{Type, Acc}, Rest}
    end.

%%% Parser

%% Parse variable names.

do_parse(dollar, [], Acc, Inside) ->
    {{dollar, Acc}, []};
do_parse(dollar, [C | Rest], Acc, Inside) ->
    case C of
	$$ ->
	    {{dollar, Acc}, [C|Rest]};
	$  ->
	    {{dollar, Acc}, [C|Rest]};
	$\t ->
	    {{dollar, Acc}, [C|Rest]};
	$\n ->
	    {{dollar, Acc}, [C|Rest]};
	$" ->
	    {{dollar, Acc}, [C|Rest]};
	${ ->
	    do_parse(dollar, Rest, Acc, 1);
	$} ->
	    if
		Inside == 1 ->
		    {{dollar, Acc}, Rest};
		true ->
		    {{dollar, Acc}, [C|Rest]}
	    end;
	Other ->
	    do_parse(dollar, Rest, Acc ++ [C], Inside)
    end;

%% Parse [command substitutions]

do_parse(subst, [], Level, Acc) ->
    {{subst, Acc}, []};
do_parse(subst, [C | Rest], Level, Acc) ->
    case C of
	$[ ->
	    do_parse(subst, Rest, Level + 1, Acc ++ [C]);
	$] ->
	    checklevel(subst, C, Rest, Level, Acc);
	C ->
	    do_parse(subst, Rest, Level, Acc ++ [C])
    end;

%% Parse {blocks}

do_parse(block, [], Level, Acc) ->
    {{block, Acc}, []};
do_parse(block, [C | Rest], Level, Acc) ->
    case C of
	${ ->
	    do_parse(block, Rest, Level + 1, Acc ++ [C]);
	$} ->
	    checklevel(block, C, Rest, Level, Acc);
	C ->
	    do_parse(block, Rest, Level, Acc ++ [C])
    end;

%% Parse words.

do_parse(word, [], Acc, Quoted) ->
    {{word, Acc}, []};
do_parse(word, [C | Rest], Acc, Quoted) ->
    case C of
	$$ ->
	    {{word, Acc}, [C|Rest]};
	$  ->
	    if
		Quoted == 1 ->
		    do_parse(word, Rest, Acc ++ [C], Quoted);
		true ->
		    {{word, Acc}, [C|Rest]}
	    end;
	$[ ->
	    {{word, Acc}, [C|Rest]};
	${ ->
	    {{word, Acc}, [C|Rest]};
	$\n ->
	    {{word, Acc}, [C|Rest]};
	$" ->
	    {{word, Acc}, [C|Rest]};
	$\\ ->
	    [H|T] = Rest,
	    do_parse(word, T, Acc ++ [H], Quoted);
	C ->
	    do_parse(word, Rest, Acc ++ [C], Quoted)
    end.

%% Parse comments.

do_parse(comment, [], Acc) ->
    {Acc, []};
do_parse(comment, [C | Rest], Acc) ->
    case C of
	$\n ->
	    {Acc, [C|Rest]};
	C ->
	    do_parse(comment, Rest, Acc)
    end;

%% Parse text - a series of words, dollars, and substs.

do_parse(text, [], Acc) ->
    {{text, Acc}, []};
do_parse(text, [C | Rest], Acc) ->
    case C of
	$$ ->
	    {NewAcc, NewRest} = do_parse(dollar, Rest, [], 0),
	    do_parse(text, NewRest, Acc ++ [NewAcc]);
	$  ->
	    {{text, Acc}, [C|Rest]};
	$[ ->
	    {NewAcc, NewRest} = do_parse(subst, Rest, 1, []),
	    do_parse(text, NewRest, Acc ++ [NewAcc]);
	${ ->
	    {NewAcc, NewRest} = do_parse(block, Rest, 1, []),
	    do_parse(text, NewRest, Acc ++ [NewAcc]);
	$\n ->
	    {{text, Acc}, [C|Rest]};
	C ->
	    {NewAcc, NewRest} = do_parse(word, [C|Rest], [], 0),
	    do_parse(text, NewRest, Acc ++ [NewAcc])
    end;

%% Parse "quoted text"

do_parse(quoted, [], Acc) ->
    {{quoted, Acc}, []};
do_parse(quoted, [C | Rest], Acc) ->
    case C of
	$$ ->
	    {NewAcc, NewRest} = do_parse(dollar, Rest, [], 0),
	    do_parse(quoted, NewRest, Acc ++ [NewAcc]);
	$"  ->
	    {{quoted, Acc}, Rest};
	$[ ->
	    {NewAcc, NewRest} = do_parse(subst, Rest, 1, []),
	    do_parse(quoted, NewRest, Acc ++ [NewAcc]);
	${ ->
	    {NewAcc, NewRest} = do_parse(block, Rest, 1, []),
	    do_parse(quoted, NewRest, Acc ++ [NewAcc]);
	$  ->
	    {NewAcc, NewRest} = do_parse(word, [C|Rest], [], 1),
	    do_parse(quoted, NewRest, Acc ++ [NewAcc]);
	C ->
	    {NewAcc, NewRest} = do_parse(word, [C|Rest], [], 1),
	    do_parse(quoted, NewRest, Acc ++ [NewAcc])
    end;

%% Parse whole commands.

do_parse(command, [], Acc) ->
    {{command, Acc}, []};
do_parse(command, [C | Rest], Acc) ->
    case C of
	$# ->
	    do_parse(comment, Rest, Acc);
	$  ->
	    do_parse(command, Rest, Acc);
	$\t  ->
	    do_parse(command, Rest, Acc);
	$\n  ->
	    {{command, Acc}, Rest};
	$" ->
	    {NewAcc, NewRest} = do_parse(
				  quoted,
				  Rest, []),
	    do_parse(command, NewRest, Acc ++ [NewAcc]);
	Char ->
	    {NewAcc, NewRest} = do_parse(
				  text,
				  [Char | Rest], []),
	    do_parse(command, NewRest, Acc ++ [NewAcc])
    end;

%% Parse the entire script.

do_parse(script, [], Acc) ->
    Acc;
do_parse(script, Rest, Acc) ->
    {NewAcc, NewRest} = do_parse(command, Rest, []),
    do_parse(script, NewRest, Acc ++ [NewAcc]).

%% User visible command.

parse(Txt) ->
    do_parse(script, Txt, []).

%% Test the parser.

parsetest() ->
    {ok, Data} = file:read_file("sample.tcl"),
    parse(binary_to_list(Data)).

%% Create a string from a list of things.

list2string([]) ->
    [];
list2string([H]) ->
    Type = H#thing.type,
    if
	Type == list ->
	    "{" ++ list2string(H#thing.val) ++ "}";
        true ->
	    tostr(H)
    end;
list2string([H|T]) ->
    Type = H#thing.type,
    %%io:format("Type is ~p : ~p~n", [Type, H#thing.val]),
    if
	Type == list ->
	    tostr(H) ++ " {" ++ list2string(H#thing.val) ++ "}";
        true ->
	    tostr(H) ++ " " ++ list2string(T)
    end.

%% All part of string2list.

sublist([], Acc) ->
    {Acc, []};
sublist([H|T], Acc) ->
    case H of
	$  ->
	    {Acc, [H|T]};
	C ->
	    sublist(T, Acc ++ [C])
    end.


stl([], Acc) ->
    {Acc, []};
stl([H|T], Acc) ->
    case H of
	${ ->
	    {Ret, NewT} = do_parse(block, T, 1, []),
	    {Type, NewAcc} = Ret,
	    Thing = #thing{type=list, val=string2list(NewAcc)},
	    stl(NewT, Acc ++ [Thing]);
	$  ->
	    stl(T, Acc);
	C ->
	    {NewAcc, NewT} = sublist([H|T], []),
	    Thing = #thing{type=str, val=NewAcc},
	    stl(NewT, Acc ++ [Thing])
    end.

string2list(Str) ->
    {Acc, NewT} = stl(Str, []),
%%    io:format("string2list: Acc: ~p~n", [Acc]),
    Acc.

%% String join, more or less.

interlieve([], _)    -> [];
interlieve([H], _)   -> [H];
interlieve([H | T], S) -> [H, S | interlieve(T, S)].

%%% Get various types out of 'things'.

tolist(Arg) ->
    Val = Arg#thing.val,
    case Arg#thing.type of
	int ->
	    [Val];
	float ->
	    [Val];
	str ->
	    string2list(Val);
	text ->
	    string2list(tostr(Val));
	bool ->
	    [Val];
	list ->
	    Val
    end.

toint(Arg) ->
    case Arg#thing.type of
	int ->
	    Arg#thing.val;
	float ->
	    round(Arg#thing.val);
	str ->
	    list_to_integer(Arg#thing.val);
	text ->
	    list_to_integer(tostr(Arg));
	bool ->
	    if
		Arg#thing.val == true ->
		    1;
		true ->
		    0
	    end
    end.

tofloat(Arg) ->
    case Arg#thing.type of
	int ->
	    Arg#thing.val;
	float ->
	    Arg#thing.val;
	str ->
	    list_to_float(Arg#thing.val);
	text ->
	    list_to_float(tostr(Arg));
	bool ->
	    if
		Arg#thing.val == true ->
		    1;
		true ->
		    0
	    end
    end.

tobool(Arg) ->
    case Arg#thing.type of
	int ->
	    if
		Arg#thing.val == 0 ->
		    false;
		true ->
		    true
	    end;
	bool ->
	    Arg#thing.val;
	str ->
	    case Arg#thing.val of
		"false" ->
		    false;
		"0" ->
		    false;
		_ ->
		    true
	    end
    end.

tostr(Arg) ->
    Val = Arg#thing.val,
    case Arg#thing.type of
	int ->
	    integer_to_list(Val);
	float ->
	    float_to_list(Val);
	str ->
	    Val;
	bool ->
	    if
		Val == true ->
		    "true";
		true ->
		    "false"
	    end;
	text ->
	    lists:flatten([tostr(X) || X <- Val]);
	list ->
	    list2string(Val)
    end.

%%% $dollar substitution.

substdollar(Interp, Arg) ->
    Res = ets:lookup(Interp#interp.vars, Arg),
    %%io:format("~p Lookup result is ~p~n", [Arg, Res]),
    element(2,hd(Res)).

%%% [command substitution].

substsubst(Interp, Arg) ->
    Res = exec(Interp, Arg),
    %%io:format("Subst ~p result is ~p~n", [Arg, Res]),
    Res.

%%% Create a 'thing' record.

thing(Type, Val) ->
    #thing{
       type = Type,
       val = Val
      }.

%%% Substitute arguments.

substargs(Interp, []) ->
    [];
substargs(Interp, [A|Args]) ->
    {Type, Arg} = A,
    %%io:format("Type ~p, Arg ~p~n", [Type, Arg]),
    case Type of
	text ->
	    if
		length(Arg) == 1 ->
		    substargs(Interp, Arg);
		true ->
		    [thing(text, substargs(Interp, Arg))]
	    end;
	quoted ->
	    [thing(text, substargs(Interp, Arg))];
	word ->
	    [thing(str, Arg)];
	block ->
	    [thing(str, Arg)];
	subst ->
	    [substsubst(Interp, Arg)];
	dollar ->
	    [substdollar(Interp, Arg)]
    end ++ substargs(Interp, Args).


eval(Interp, []) ->
    0;
eval(Interp, Args) ->
    %%io:format("Command is ~p, ~n", [Command]),
    %%io:format("Args are ~p~n", [Args]),
    Command = substargs(Interp, Args),
    Cmd = tostr(hd(Command)),
    %%io:format("Command is ~p, Cmd is ~p~n", [Command, Cmd]),
    {_, Fun} = hecl_procs:fetchproc(Interp, Cmd),
    erlang:apply(Fun, [Interp, tl(Command)]).

exec(Interp, Data) ->
    Res = [eval(Interp, B) || {A, B} <- parse(Data)],
    %%io:format("Res is ~p~n", [Res]),
    lists:last((Res)).

make_interp() ->
    I = #interp{},
    hecl_procs:register_procs(I),
    I.

del_interp(Interp) ->
    ets:delete(Interp#interp.commands),
    ets:delete(Interp#interp.vars).

setvar(Interp, Name, Value) ->
    %%io:format("Setting ~p to ~p~n", [Name, Value]),
    ets:insert(Interp#interp.vars,
	       {Name, Value}).

test() ->
    {ok, Data} = file:read_file("sample.tcl"),
    I = make_interp(),
    exec(I, binary_to_list(Data)),
    del_interp(I).


%%% Hack alert! FIXME.

tform(Str) ->
    lists:map(fun(X) ->
		      case X of
			  $, ->
			      $ ;
			  $[ ->
			      ${;
			  $] ->
			      $};
			  $\n ->
			      $ ;
			  _ ->
			      X
		      end
		end, Str).

%% Takes a proplist and makes it into a Hecl list.  Sort of... it
%% should be cleaner.

heclize(Proplist) ->
    Str = tform(lists:flatten(
		  io_lib:format("~p", [Proplist]))),
    %% kill the beginning and ending {}.
    string:substr(Str, 2, length(Str) - 2).
%    #thing{type=str,
%	   val=tform(lists:flatten(io_lib:format("~p", [Proplist])))}.
