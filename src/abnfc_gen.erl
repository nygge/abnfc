%%%-------------------------------------------------------------------
%%% @copyright 2009 Anders Nygren
%%% @version  {@vsn} 
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc Code generation.
%%% @end 
%%%-------------------------------------------------------------------
-module(abnfc_gen).

%% API
-export([generate/2]).

-compile(export_all).

-include("abnfc_ast.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec 
%% @doc 
%% @end
%%--------------------------------------------------------------------
generate(AST, Opts) ->
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    DT = lists:flatten(
	   io_lib:format("~w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w", 
			 [Year,Month,Day,Hour,Min,Sec])),
    Body = [gen_rule(R) || R <- AST],
    Module = proplists:get_value(mod,Opts),
    {ok, lists:flatten(io_lib:format(template(), [DT, Module, Module, Body]))}.

%%====================================================================
%% Internal functions
%%====================================================================

gen_rule(#rule{name=Name, body=Element, code=nocode}) ->
    io:format("abnfc_gen: generating rule ~p~n",[Name]),
    Body = lists:flatten(gen_elem(Element, 1)),
    mk_rule_fun(Name, Body);

gen_rule(#rule{name=Name, body=Element, code=Code}) ->
    Vars = gen_vars(Element),
    io:format("abnfc_gen: generating rule ~p~n",[Name]),
    Body = lists:flatten(gen_elem(Element, 1)),
    mk_rule_fun(Name, Body, Vars, Code).

gen_elem(#seq{elements=Elements}, Level) ->
    Str = "'__seq'(~n[~s~n])",
    Body = string:join([gen_elem(Element, Level+1)||Element <- Elements], ",\n"),
    io_lib:format(Str, [Body]);

gen_elem(#alt{alts=Elements}, Level) ->
    Str = "'__alt'(~n[~s~n])",
    Body = string:join([gen_elem(Element, Level+1)||Element <- Elements], ",\n"),
    io_lib:format(Str, [Body]);

gen_elem(#repeat{min=0, max=1, body=Elem}, Level) ->
    Str =
	"fun (Str) ->\n"
	"    case (~s)(Str) of\n"
	"        fail -> {ok, [], Str};\n"
	"        {ok, _Res, _Tail} -> {ok,[_Res],_Tail}\n"
	"    end\n"
	"end\n",
    io_lib:format(Str, [gen_elem(Elem, Level+1)]);

gen_elem(#repeat{min=Min, max=Max, body=Elem}, Level) ->
    Str = "'__repeat'(~n~p, ~p, ~n~s~n)",
    io_lib:format(Str, [Min, Max, gen_elem(Elem, Level+1)]);

gen_elem(#char_val{value=Num}, _Level) ->
    Str = 
	"fun ([C|Tl]) \n"
	"when C==~p ->\n"
	"        {ok, C, Tl};\n"
	"    (_) ->\n"
	"        fail\n"
	"end",
    io_lib:format(Str, [Num]);

gen_elem(#char_range{from=From, to=To}, _Level) ->
    Str = 
	"fun([C|Tl]) when (C>=~p) and (C=<~p) ->~n"
	"           {ok, C, Tl};\n"
 	"         (_) ->~n"
	"           fail~n"
	"    end",
    io_lib:format(Str, [From, To]);

gen_elem(#char_alt{alts=Alts}, _Level) ->
    Guard = num_alt_guard(1,Alts),
    ParList="C1",
    Str =
	"fun ([~s|Tl]) \n"
	"when ~s ->\n"
	"        {ok, ~s, Tl};\n"
	"    (_) ->\n"
	"        fail\n"
	"end",
    io_lib:format(Str, [ParList, Guard, "C1"]);

gen_elem(#char_seq{elements=Nums}, _Level) ->
    Pars=[lists:flatten(io_lib:format("C~p",[N])) || N <- lists:seq(1,length(Nums))],
    ParList = num_par_list(length(Nums)),
    Guard = num_guard(Nums),
    Result = string:join(Pars,","),
    Str =
	"fun (~s) \n"
	"when ~s ->\n"
	"        {ok, [~s], Tl};\n"
	"    (_) ->\n"
	"        fail\n"
	"end",
    io_lib:format(Str, [ParList, Guard, Result]);

gen_elem(#rulename{name=Rule}, _Level) when is_atom(Rule) ->
    Str = "~p()",
    io_lib:format(Str, [Rule]).

mk_rule_fun(Name, Body) ->
    Name1 = list_to_atom(atom_to_list(Name)++"_dec"),
    Str =
	"~p() ->~n"
	"    fun (T) ->~n"
	"        __P=~s,~n"
	"        __P(T)~n"
	"    end.~n"
	"~p(Str) ->~n"
	"    (~p())(Str).~n~n",
    io_lib:format(Str, [Name, Body, Name1, Name]).

mk_rule_fun(Name, Body, Vars, Code) -> 
    Name1 = list_to_atom(atom_to_list(Name)++"_dec"),
    Str =
	"~p() ->~n"
	"    fun (T) -> ~n"
	"        __P=~s,~n"
	"        case __P(T) of~n"
	"            {ok, ~s, __Rest} ->~n"
	"                __Ret = begin~n"
	"                         ~s~n"
	"                      end,~n"
	"                {ok, __Ret, __Rest};~n"
	"            fail ->~n"
	"                fail~n"
	"        end~n"
        "    end.~n~n"
	"~p(Str) ->~n"
	"    (~p())(Str).~n~n",
    io_lib:format(Str, [Name, Body, Vars, Code, Name1, Name]).

gen_vars(#seq{elements=Es}) ->
    Vs = string:join([lists:concat(["_YY",N]) ||N<-lists:seq(1,length(Es))],", "),
    lists:concat(["[",Vs,"]=_YY"]);
gen_vars(_) ->
    "_YY".

num_guard(Cs) ->
    {_,R} = lists:foldl(
	      fun (#char_range{from=From,to=To},{Pos,Acc}) ->
		      {Pos+1, [io_lib:format("((C~p>=~p) and (C~p=<~p))",
					     [Pos,From, Pos, To])|Acc]};
		  (#char_val{value=C}, {Pos,Acc}) ->
		      {Pos+1, [io_lib:format("C~p==~p",[Pos,C])|Acc]};
		  (#char_alt{alts=Alts}, {Pos,Acc}) ->
		      {Pos+1,[num_alt_guard(Pos,Alts)|Acc]}
	      end, {1,[]}, Cs),
    string:join(lists:reverse(R),",\n").

num_par_list(Len) ->
    lists:flatten(
      io_lib:format("[~s|Tl]",
		    [string:join(
		       [io_lib:format("C~p",[N]) || N <- lists:seq(1,Len)],",")])).


num_alt_guard(Var, Alts) ->
    R = lists:foldl(
	  fun (#char_range{from=From,to=To},Acc) ->
		  [io_lib:format("((C~p>=~p) and (C~p=<~p))",
				 [Var,From, Var, To])|Acc];
	      (#char_val{value=C}, Acc) ->
		  [io_lib:format("(C~p==~p)",[Var, C])|Acc]
	  end, [], Alts),
    "("++string:join(lists:reverse(R)," or ")++")".

num_seq_guard(Es) ->
    {_,R} = lists:foldl(
	      fun (E, {Pos,Acc}) ->
		      Var = lists:concat(["C",Pos]),
		      {Pos+1, ["("++num_alt_guard(Var, E)++")"|Acc]}
	      end, {1, []}, Es),
    string:join(lists:reverse(R),"\nand ").


template() ->
    "%% Do not modify this file, it is automatically\n"
	"%% generated by abnfc. All changes will be lost\n"
	"%% when it is regenerated.\n"
	"%% Generated by abnfc_gen on ~s\n\n"
	"-module(~s).\n"
	"-export([]).\n"
	"-compile(export_all).\n\n"
	"-include(\"~s.hrl\").\n\n"
	"~s\n"
	"\n\n"
	"%%================================================\n"
	"%% Run-time support functions\n"
	"%%================================================\n\n"
	"%% Match one of several parsers.\n"
	"'__alt'([P|Ps]) ->\n"
	"    fun (T) ->\n"
	"	    case P(T) of\n"
	"		{ok, R, T1} -> \n"
	"		    {ok, R, T1};\n"
	"		fail -> \n"
	"		    case Ps of\n"
	"			[] ->\n"
	"			    fail;\n"
	"			_ ->\n"
	"			    ('__alt'(Ps))(T)\n"
	"		    end\n"
	"	    end\n"
	"    end.\n"
	"\n\n"
	"%% Match between Min and Max repetitions of parser P\n"
	"'__repeat'(Min, Max, P) ->\n"
	"    '__repeat'(Min, Max, P, 0).\n"
	"'__repeat'(Min, Max, P, Found) ->\n"
	"    fun (T) ->\n"
	"	    case P(T) of\n"
	"		{ok,R1,T1} when Max==Found+1 ->\n"
	"		    {ok, [R1], T1};\n"
	"		{ok,R1,T1} ->\n"
	"		    case ('__repeat'(Min, Max, P, Found+1))(T1) of\n"
	"			{ok,R2,T2} ->\n"
	"			    {ok,[R1|R2],T2};\n"
	"			fail when Found >= Min ->\n"
	"			    {ok,[R1],T1};\n"
	"			fail ->\n"
	"			    fail\n"
	"		    end;\n"
	"		fail when Found >= Min ->\n"
	"		    {ok,[],T};\n"
	"		fail ->\n"
	"		    fail\n"
	"	    end\n"
	"    end.\n"
	"\n\n"
	"%% Match a sequence of parsers.\n"
	"'__seq'([P|Ps]) ->\n"
	"    fun (T) ->\n"
	"	    case P(T) of\n"
	"		{ok, R1, T1} ->\n"
	"		    case ('__seq'(Ps))(T1) of\n"
	"			{ok, R2, T2} ->\n"
	"			    {ok,  [R1|R2], T2};\n"
	"			fail ->\n"
	"			    fail\n"
	"		    end;\n"
	"		fail ->\n"
	"		    fail\n"
	"	    end\n"
	"    end;\n"
	"'__seq'([]) ->\n"
	"    fun(T) ->\n"
	"	    {ok,[],T}\n"
	"    end.\n".

