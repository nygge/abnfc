-import(abnfc,[erlangcode/0]).
-import(rfc4234_core, ['ALPHA'/0,
		       'DIGIT'/0,
		       'WSP'/0,
		       'CRLF'/0,
		       'VCHAR'/0,
		       'DQUOTE'/0,
		       'BIT'/0,
		       'HEXDIG'/0]).

-include("abnfc_ast.hrl").

hex_to_int(Ds) ->
    lists:foldl(fun(D,Acc) ->
			Dint = case D of
				   D when D >= $0, D =< $9 ->
				       D-$0;
				   D when D >= $a, D =< $f ->
				       D-$a+10;
				   D when D >= $A, D =< $F ->
				       D-$A+10
			       end,
			Acc*16 + Dint
		end, 0, Ds).

dec_to_int(Ds) ->
    list_to_integer(Ds).

bin_to_int(Ds) ->
    lists:foldl(fun(D,Acc) ->
			Dint = D-$0,
			Acc*2 + Dint
		end, 0, Ds).
