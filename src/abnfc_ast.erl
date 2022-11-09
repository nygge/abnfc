%%%-------------------------------------------------------------------
%%% @copyright 2009 Anders Nygren
%%% @version  {@vsn}
%%% @author Anders Nygren <anders.nygren@gmail.com>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(abnfc_ast).

%% API
-export(
   [ast_to_int_form/1,
    int_transform/2
   ]).

-include("../include/abnfc_ast.hrl").

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
    Merged = merge_alts(Chars),
    [#char_alt{alts=Merged}|Others].

cmp_chars(#char_val{value=V},#char_range{from=F,to=_T}) when V =< F ->
    true;
cmp_chars(#char_range{from=_F,to=T},#char_val{value=V}) when T =< V->
    true;
cmp_chars(X,Y) ->
    X =< Y.

merge_alts(Chars) ->
    [A|As]=lists:sort(fun cmp_chars/2, Chars),
    sort_alts(lists:foldl(fun merge_alts/2, [A], As)).

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

sort_alts(Alts) ->
    lists:reverse([A||{_,A} <- lists:keysort(1,[{range_size(A),A}||A<-Alts])]).

range_size(#char_range{from=F,to=T}) ->
    T-F+1;
range_size(#char_val{}) ->
    1.

%%====================================================================
%% Test functions
%%====================================================================
