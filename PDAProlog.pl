jsonparse(JSONString, Object) :- 
  string_chars(JSONString, JSONChars), 
  fix_string(JSONChars, JSONCharsSpace), 
  startNFA(JSONCharsSpace, ObjectChars),
  string_chars(ObjectString, ObjectChars),
  term_string(Object, ObjectString),
  !.


startNFA(Input, Object) :- 
  initial(S), 
  accept(Input, S, [], Object).

accept([I | Is], Q, S, Object) :- 
    delta(Q, I, S, Q1, S1, Temp1, Object), 
    !,
    accept(Is, Q1, S1, Temp1).

accept([], Q, [], []) :- 
  final(Q).

initial(a).
final(c).

%% Gestione degli oggetti

delta(a, '{', P, b, ['{' | P], Object, Object2) :- 
  jsonObjChars(OBJ), append(OBJ, Object, Object2).
delta(a, '[', P, m, ['[' | P], Object, Object2) :- 
  jsonArrayChars(OBJ), append(OBJ, Object, Object2).

delta(b, '}', ['{' | P], c, P, Object, [']', ')' | Object]).
delta(b, '"', P, d, P, Object, ['(', '"' | Object]).

delta(c, '}', ['{' | P], c, P, Object, [']', ')' | Object]).
delta(c, ']', ['[' | P], c, P, Object, [']', ')' | Object]).
delta(c, ',', P, a, P, Object, [',' | Object]).


delta(d, '"', P, e, P, Object, ['"' | Object]) :- !.
delta(d, Value, P, d, P, Object, [Value | Object]).

delta(e, ':', P, f, P, Object, [',' | Object]).

delta(f, '"', P, g, P, Object, ['"' | Object]).
delta(f, '[', P, m, ['[' | P], Object, Object2) :- 
  jsonArrayChars(OBJ), append(OBJ, Object, Object2).
delta(f, ',', P, b, P, Object, [',' | Object]).
delta(f, '{', P, b, ['{' | P], Object, Object2) :- 
  jsonObjChars(OBJ), append(OBJ, Object, Object2).
delta(f, '}', P, c, ['{' | P], Object, [')', ']' | Object]) :- !.
delta(f, Value, P, f, P, Object, [Value | Object]).

delta(g, '"', P, h, P, Object, ['"', ')' | Object]) :- !.
delta(g, Value, P, g, P, Object, [Value | Object]).

delta(h, '}', ['{' | P], c, P, Object, [']', ')' | Object]).
delta(h, ',', P, b, P, Object, [',' | Object]).

%% Gestione degli array

%%delta(i, '"', P, j, P).
%%delta(i, '[', P, m, ['[' | P]).
%%delta(i, '{', P, b, ['{' | P]).

%%delta(j, '"', P, k, P) :- !.
%%delta(j, _, P, j, P).

%%delta(k, ',', P, i, P).
%%delta(k, ']', P, l, P).

%%delta(l, ',', P, b, P).
%%delta(l, '}', P, c, P).

%%delta(m, '[', [], m, ['[' | P], Object, [OBJ | Object]) :- jsonArrayChars(OBJ).
delta(m, '[', P, m, ['[' | P], Object, Object2) :- 
  jsonArrayChars(OBJ), append(OBJ, Object, Object2).
%%delta(m, '{', [], m, ['{' | P]). %% Riconosce i value
%%delta(m, '{', P, m, ['{' | P]).
%%delta(m, '{', [], b, ['{' | P], Object, [OBJ | Object]) :- j(OBJ). %% Riconosce gli oggetti dentro gli array
delta(m, '{', P, b, ['{' | P], Object, Object2) :- 
  jsonObjChars(OBJ), append(OBJ, Object, Object2).
delta(m, '"', P, o, P, Object, ['"' | Object]).
delta(m, ',', P, m, P, Object, [',' | Object]).
delta(m, ']', ['[' | P], c, P, Object, [']', ')' | Object]).
delta(m, '}', ['{' | P], c, P, Object, [']', ')' | Object]) :- !.
delta(m , Value, P, m, P, Object, [Value | Object]).


delta(o, '"', P, p, P, Object, ['"' | Object]) :- !.
delta(o, Value, P, o, P, Object, [Value | Object]).

delta(p, ']', ['[' | P], c, P, Object, [']', ')' | Object]).
delta(p, '}', ['{' | P], c, P, Object, [']', ')' | Object]).
delta(p, ',', P, m, P, Object, [',' | Object]).
%%delta(p, ']', [], l, []).
%%delta(p, '}', [], l, []).



fix_string([], []).

fix_string([' ' | Rest], List) :- 
    fix_string(Rest, List).

fix_string([Head | Tail], [Head | List]) :-
    fix_string(Tail, List).

jsonObjChars(P) :- 
  string_chars('jsonobj([', P).
jsonArrayChars(P) :- 
  string_chars('jsonarray([', P).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

jsonread(FileName, JSON) :-
  open(FileName, read, Stream),
  read_file_to_string(Stream, String),
  close(Stream),
  jsonparse(String, JSON).


jsondump(JSON, FileName) :-
  open(FileName, write, Stream),
  jsonstringify(JSON, String),
  write_string(Stream, String),
  close(Stream).


jsonstringify(json(Object), String) :-
  json_object_string(Object, String).

jsonstringify(json(Array), String) :-
  json_array_string(Array, String).

jsonstringify(json(null), "null").

jsonstringify(json(Boolean), String) :-
  boolean_string(Boolean, String).

jsonstringify(json(Number), String) :-
  number_string(Number, String).

jsonstringify(json(String), String).

boolean_string(true, "true").
boolean_string(false, "false").

number_string(Number, String) :-
  number(Number),
  format(string(String), '~f', [Number]).

json_object_string(Object, String) :-
  maplist(json_pair_string, Object, Pairs),
  atomic_list_concat(Pairs, ',', String).

json_pair_string(Key-Value, String) :-
  jsonstringify(Value, ValueString),
  format(string(String), '"~w":~w', [Key, ValueString]).

json_array_string(Array, String) :-
  maplist(jsonstringify, Array, Elements),
  atomic_list_concat(Elements, ',', String).
