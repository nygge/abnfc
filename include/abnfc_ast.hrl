-record(rulelist, {mod, rules=[]}).

-record(rule, {type, name, body, code}).

-record(rulename, {name}).

-record(alt, {alts=[]}).

-record(seq, {elements=[]}).

-record(repeat, {min=0, max=infinity, body}).

-record(char_range, {from, to}).

-record(char_alt, {alts=[]}).

-record(char_seq, {elements=[]}).

-record(char_val, {value=[]}).
