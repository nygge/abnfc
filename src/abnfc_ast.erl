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
    [#char_alt{alts=lists:sort(Chars)}|Others].

%%====================================================================
%% Test functions
%%====================================================================
