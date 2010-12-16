%%%-------------------------------------------------------------------
%%% @copyright 2009 Anders Nygren
%%% @version  {@vsn} 
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc 
%%% @end 
%%%-------------------------------------------------------------------
-module(abnfc_ast).

%% API
%%-export([transform/2]).
-compile(export_all).

-include("abnfc_ast.hrl").

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec (AST) -> INT_FORM
%% @doc Transform an AST to the intermediate format.
%% @end
%%--------------------------------------------------------------------
ast_to_int_form(#rulelist{rules=Rules}) ->
    [R#rule{body=transform(R#rule.body)}||R <- Rules].


int_transform(INT, Fun) ->
    [R#rule{body=Fun(R#rule.body)} || R <- INT].

%%====================================================================
%% Internal functions
%%====================================================================

transform(#rule{body=Body}=R) ->
    R#rule{body=transform(Body)};
transform(#alt{alts=Alts}=A) ->
    Alts1 = [transform(Alt)||Alt <- Alts],
    case char_alts(Alts1) of
	[C] when is_record(C,char_alt) ->
	    C;
	Alts2 ->
	    A#alt{alts=Alts2}
    end;
transform(#seq{elements=Elems}=S) ->
    Elems1 = [transform(Elem)||Elem <- Elems],
    S#seq{elements=Elems1};
transform(#repeat{body=Body}=R) ->
    R#repeat{body=transform(Body)};
transform(Element) ->
    Element.

char_alts(Alts) ->
    char_alts(Alts, {[],[]}).

char_alts([Alt|Alts], {Chars,Others}) when is_record(Alt, char_val);
					   is_record(Alt, char_range) ->
    char_alts(Alts,{Chars++[Alt], Others});
char_alts([Alt|Alts], {Chars,Others}) ->
    char_alts(Alts,{Chars, Others++[Alt]});
char_alts([], {[], Others}) ->
    Others;
char_alts([], {Chars, Others}) ->
    As=lists:sort(Chars),
    Merged=merge_alts(As),
    [#char_alt{alts=Merged}|Others].


merge_alts([A|As]) ->
    lists:reverse(lists:foldl(fun merge_alts/2, [A], As)).

merge_alts(#char_val{value=V}, [#char_val{value=Prev}|Acc]) when V==Prev+1 ->
    [#char_range{from=Prev,to=V}|Acc];
merge_alts(#char_val{}=N, [#char_val{}|_]=Acc) ->
    [N|Acc];
merge_alts(#char_val{value=V}, [#char_range{from=From,to=To}|Acc]) when V==To+1 ->
    [#char_range{from=From,to=V}|Acc];
merge_alts(#char_val{}=N, [#char_range{}|_]=Acc) ->
    [N|Acc];
merge_alts(#char_range{from=F1,to=T1}, [#char_val{value=V}|Acc]) when F1==V+1 ->
    [#char_range{from=V,to=T1}|Acc];
merge_alts(#char_range{}=N, [#char_val{}|_]=Acc) ->
    [N|Acc];

merge_alts(#char_range{from=F1,to=T1}, [#char_range{from=From,to=To}|Acc]) when F1==To+1 ->
    [#char_range{from=From,to=T1}|Acc];
merge_alts(#char_range{}=N, [#char_range{}|_]=Acc) ->
    [N|Acc].

%%====================================================================
%% Test functions
%%====================================================================
