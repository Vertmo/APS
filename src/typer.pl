%% Toplevel function
check_type(P) :- check_prog_type(P, void), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Basic environment
build_basic_env(["true":bool,"false":bool,
                 "not":tfun([bool],bool),"and":tfun([bool,bool],bool),"or":tfun([bool,bool],bool),
                 "eq":tfun([int,int],bool),"lt":tfun([int,int],bool),
                 "add":tfun([int,int],int),"sub":tfun([int,int],int),
                 "mul":tfun([int,int],int),"div":tfun([int,int],int)]).

%% Find type in the environment
check_in_env([S:T|_], S, T).
check_in_env([_|EnvS], S, T) :- check_in_env(EnvS, S, T).

%% Get only the types of a sym/type pair list (env)
get_types_of_env([], []).
get_types_of_env([_:T|EnvS], [T|TypeS]) :- get_types_of_env(EnvS, TypeS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% List to set
set([], []).
set([H|T], [H|T1]) :- remv(H, T, T2), set(T2, T1).
remv(_, [], []).
remv(X, [X|T], T1) :- remv(X, T, T1).
remv(X, [H|T], [H|T1]) :- X \= H, remv(X, T, T1).

%% Find type vars in parameters of a function
find_type_vars([], []).
find_type_vars([tvar(V)|Ts], [V|Vs]) :- find_type_vars(Ts, Vs).
find_type_vars([vec(T)|Ts], Vs) :-
    find_type_vars([T], Vs1), find_type_vars(Ts, Vs2), append(Vs1, Vs2, Vs).
find_type_vars([tprod(T1, T2)|Ts], Vs) :-
    find_type_vars(T1, Vs1), find_type_vars(T2, Vs2), find_type_vars(Ts, Vs3),
    append(Vs1, Vs2, Vs4), append(Vs3, Vs4, Vs).
find_type_vars([tfun(Tin, To)|Ts], Vs) :-
    find_type_vars(Tin, Vs1), find_type_vars(To, Vs2), find_type_vars(Ts, Vs3),
    append(Vs1, Vs2, Vs4), append(Vs3, Vs4, Vs).
find_type_vars([[FT|FTs]|Ts], Vs) :-
    find_type_vars([FT|FTs], Vs1), find_type_vars(Ts, Vs2),
    append(Vs1, Vs2, Vs).
find_type_vars([_|Ts], Vs) :- find_type_vars(Ts, Vs).

%% Add foralls if necessary
add_foralls(T, [], T) :- !.
add_foralls(T, Vs, forall(Vs, T)).
add_foralls(T, Vsi, forall(Vso, T)) :- permutation(Vsi, Vso).

%% Find and add foralls to a function type
forallize(tfun(Ti, To), Fto) :- find_type_vars(Ti, V), set(V, Vs), add_foralls(tfun(Ti, To), Vs, Fto).
forallize(T, T).

%% Find a type variable in list of subs
find_in_subs([], T, T).
find_in_subs([Tn:T|_],tvar(Tn),T).
find_in_subs([_|S], Tn, T) :- find_in_subs(S, Tn, T).

%% Remove type variables
deforallize(S, forall(_, T), To) :- deforallize(S, T, To).
deforallize(_, [], []).
deforallize(S, [vec(T)|Ts], [To|Tso]) :-
    deforallize(S, T, To), deforallize(S, Ts, Tso).
deforallize(S, [tprod(T1, T2)|Ts], [tprod(T1o, T2o)|Tso]) :-
    deforallize(S, T1, T1o), deforallize(T2, T2o), deforallize(S, Ts, Tso).
deforallize(S, [tfun(Fti, Fto)|Ts], [tfun(Ftio, Ftoo)|Tso]) :-
    deforallize(S, Fti, Ftio), deforallize(S, Fto, Ftoo), deforallize(S, Ts, Tso).
deforallize(S, [T|Ts], [To|Tso]) :-
    deforallize(S, T, To), deforallize(S, Ts, Tso).
deforallize(S, T, To) :- find_in_subs(S, T, To).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Algo d'unification

%% Substitute a value to another
substitute([], _, _, []).
substitute([T|Gi], T, T2, [T2|Go]) :- !, substitute(Gi, T, T2, Go).
substitute([vec(Ti)|Gi], T1, T2, [vec(To)|Go]) :- !,
    substitute([Ti], T1, T2, [To]), substitute(Gi, T1, T2, Go).
substitute([tprod(T1i, T2i)|Gi], T1, T2, [tprod(T1o, T2o)|Go]) :- !,
    substitute([T1i], T1, T2, [T1o]), substitute([T2i], T1, T2, [T2o]),
    substitute(Gi, T1, T2, Go).
substitute([tfun(Ftii, Ftio)|Gi], T1, T2, [tfun(Ftoi, Ftoo)|Go]) :- !,
    substitute(Ftii, T1, T2, Ftio), substitute([Ftoi], T1, T2, [Ftoo]),
    substitute(Gi, T1, T2, Go).
substitute([T|Gi], T1, T2, [T|Go]) :- T \= T1, substitute(Gi, T1, T2, Go).

%% Cas delete
uni_delete(Si, [T|G1], [T|G2], So) :- !, unify(Si, G1, G2, So).

%% Cas decompose (vecteurs et fonctions)
uni_decompose(Si, [vec(T1)|G1], [vec(T2)|G2], So) :-
    unify(Si, [T1], [T2], Se), unify(Se, G1, G2, So).
uni_decompose(Si, [tprod(T1, T2)|G1], [tprod(T1o, T2o)|G2], So) :-
    unify(Si, [T1], [T1o], S1), unify(S1, [T2], [T2o], S2), unify(S2, G1, G2, So).
uni_decompose(Si, [tfun(Fti1, Fto1)|G1], [tfun(Fti2, Fto2)|G2], So) :-
    unify(Si, Fti1, Fti2, S1), unify(S1, [Fto1], [Fto2], S2),
    unify(S2, G1, G2, So).
uni_decompose(Si, [tfun(Fti1, Fto1)|G1], [forall(_, tfun(Fti2, Fto2))|G2], So) :-
    unify(Si, Fti1, Fti2, S1), unify(S1, [Fto1], [Fto2], S2),
    unify(S2, G1, G2, So).
uni_decompose(Si, [forall(_, tfun(Fti1, Fto1))|G1], [tfun(Fti2, Fto2)|G2], So) :-
    unify(Si, Fti1, Fti2, S1), unify(S1, [Fto1], [Fto2], S2),
    unify(S2, G1, G2, So).

%% Cas swap
uni_swap(Si, [T1|G1], [tvar(T2)|G2], So) :-
    unify(Si, [tvar(T2)], [T1], Se), unify(Se, G1, G2, So).

%% Cas conflict
uni_conflict(_, T1, T2, _) :-
    length(T1, N1), length(T2, N2), N1 \= N2.

%% Cas check
uni_check(S, [tvar(T1)|_], [T2|_], _) :-
    find_in_subs(S, tvar(T1), T2s), T2s \= tvar(T1), T2s \= T2.

%% Cas eliminate
uni_eliminate(_, [tvar(T1)|_], [T2|_], _) :-
    find_type_vars(T2, V), member(T1, V), !, fail.
uni_eliminate(Si, [tvar(T1)|G1], [T2|G2], So) :-
    substitute(G1, tvar(T1), T2, G1o), unify([T1:T2|Si], [T2|G1o], [T2|G2], So).

%% Unification
unify(S, [], [], S) :- !.
unify(Si, G1, G2, So) :- uni_check(Si, G1, G2, So), !, fail.
unify(Si, G1, G2, So) :- uni_conflict(Si, G1, G2, So), !, fail.
unify(Si, G1, G2, So) :- uni_delete(Si, G1, G2, So), !.
unify(Si, G1, G2, So) :- uni_decompose(Si, G1, G2, So), !.
unify(Si, G1, G2, So) :- uni_swap(Si, G1, G2, So), !.
unify(Si, G1, G2, So) :- uni_eliminate(Si, G1, G2, So), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check type of the program
check_prog_type(P, void) :- build_basic_env(Env), check_cmds_type(Env, P, _, void).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check a series of command
check_cmds_type(Env, [], Env, void).
check_cmds_type(E, [C|CS], NNE, T) :-
    check_dec_type(E, C, NE), check_cmds_type(NE, CS, NNE, T).
check_cmds_type(E, [C|CS], NE, T) :-
    check_stat_type(E, C, void), check_cmds_type(E, CS, NE, T).
check_cmds_type(E, [C|CS], NE, T) :-
    check_stat_type(E, C, union(T,void)), check_cmds_type(E, CS, NE, T).
check_cmds_type(E, [C], E, T) :-
    check_stat_type(E, C, T).
check_cmds_type(E, [return(R)], E, T) :-
    check_expr_type(E, R, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check a declaration
check_dec_type(Env, const(X, Ti, E), [X:T|Env]) :-
    forallize(Ti, T), check_expr_type(Env, E, T).
check_dec_type(Env, fun(X, Touti, Args, Body), [X:T|Env]) :-
    forallize(Touti, Tout),
    get_types_of_env(Args, Tin), forallize(tfun(Tin, Tout), T),
    append(Args, Env, NEnv), check_expr_type(NEnv, Body, Tout).
check_dec_type(Env, funrec(X, Touti, Args, Body), [X:T|Env]) :-
    forallize(Touti, Tout),
    get_types_of_env(Args, Tin), forallize(tfun(Tin, Tout), T),
    append([X:T|Args], Env, NEnv), check_expr_type(NEnv, Body, Tout).
check_dec_type(Env, var(X, Ti), [X:T|Env]) :- forallize(Ti, T).
check_dec_type(Env, proc(X, Args, Body), [X:T|Env]) :-
    get_types_of_env(Args, Tin), forallize(tfun(Tin, void), T),
    append(Args, Env, NEnv), check_cmds_type(NEnv, Body, _, void).
check_dec_type(Env, procrec(X, Args, Body), [X:T|Env]) :-
    get_types_of_env(Args, Tin), forallize(tfun(Tin, void), T),
    append([X:T|Args], Env, NEnv), check_cmds_type(NEnv, Body, _, void).
check_dec_type(Env, funproc(X, Touti, Args, Body), [X:T|Env]) :-
    forallize(Touti, Tout),
    get_types_of_env(Args, Tin), forallize(tfun(Tin, Tout), T),
    append(Args, Env, NEnv), check_cmds_type(NEnv, Body, _, Tout).
check_dec_type(Env, funprocrec(X, Touti, Args, Body), [X:T|Env]) :-
    forallize(Touti, Tout),
    get_types_of_env(Args, Tin), forallize(tfun(Tin, Tout), T),
    append([X:T|Args], Env, NEnv), check_cmds_type(NEnv, Body, _, Tout).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check a statement
check_stat_type(Env, echo(E), void) :- check_expr_type(Env, E, int).
check_stat_type(Env, set(X, E), void) :-
    check_expr_type(Env, X, T), check_expr_type(Env, E, T).
check_stat_type(Env, ifs(E, Then, Else), T) :-
    check_expr_type(Env, E, bool),
    check_cmds_type(Env, Then, _, T), check_cmds_type(Env, Else, _, T).
check_stat_type(Env, ifs(E, Then, Else), union(T,void)) :-
    T \= void, check_expr_type(Env, E, bool),
    check_cmds_type(Env, Then, _, void), check_cmds_type(Env, Else, _, T).
check_stat_type(Env, ifs(E, Then, Else), union(T,void)) :-
    T \= void, check_expr_type(Env, E, bool),
    check_cmds_type(Env, Then, _, T), check_cmds_type(Env, Else, _, void).
check_stat_type(Env, while(E, B), union(T,void)) :-
    check_expr_type(Env, E, bool), check_cmds_type(Env, B, _, T).
check_stat_type(Env, while(E, B), void) :-
    check_expr_type(Env, E, bool), check_cmds_type(Env, B, _, void).
check_stat_type(Env, call(X, Es), void) :-
    check_in_env(Env, X, Ft), check_app_type(Env, Ft, Es, void).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check an expression
check_expr_type(_, E, int) :- number(E).
check_expr_type(Env, sym(S), T) :- check_in_env(Env, S, T).
check_expr_type(Env, sym(S), T) :- check_in_env(Env, S, forall(_, Ti)),
    unify([], Ti, T, _).
check_expr_type(Env, abs(Args, Body), T) :-
    append(Args, Env, NEnv), check_expr_type(NEnv, Body, Tout),
    get_types_of_env(Args, Tin), forallize(tfun(Tin, Tout), T).
check_expr_type(Env, abs(Args, Body), T) :-
    append(Args, Env, NEnv), check_expr_type(NEnv, Body, Tout),
    get_types_of_env(Args, Tin),
    forallize(tfun(Tin, Tout), forall(_, Ti)), unify([], Ti, T, _).
check_expr_type(Env, if(Cond, Then, Else), T) :-
    check_expr_type(Env, Cond, bool),
    check_expr_type(Env, Then, T), check_expr_type(Env, Else, T).
check_expr_type(Env, app(sym("alloc"), [E]), vec(_)) :- check_expr_type(Env, E, int).
check_expr_type(Env, app(sym("nth"), [E1,E2]), T) :-
    check_expr_type(Env, E1, vec(T)), check_expr_type(Env, E2, int).
check_expr_type(Env, app(sym("len"), [E]), int) :-
    check_expr_type(Env, E, vec(_)).
check_expr_type(Env, app(F, Es), RTout) :-
    check_expr_type(Env, F, Ft), check_app_type(Env, Ft, Es, RTout).
check_expr_type(Env, let(X, E, B), T) :-
    check_expr_type(Env, E, T1), check_expr_type([X:T1|Env], B, T).
% Pairs
check_expr_type(Env, pair(E1, E2), tprod(T1, T2)) :-
    check_expr_type(Env, E1, T1), check_expr_type(Env, E2, T2).
check_expr_type(Env, fst(E), T) :- check_expr_type(Env, E, tprod(T, _)).
check_expr_type(Env, snd(E), T) :- check_expr_type(Env, E, tprod(_, T)).

%% Check type for a set of expressions
check_expr_types(_, [], []).
check_expr_types(Env, [E|Es], [T|Ts]) :-
    check_expr_type(Env, E, T), check_expr_types(Env, Es, Ts).

%% Check a function application (by checking passed arguments and return type)
check_app_type(_, tfun([], Tout), [], Tout).
check_app_type(Env, tfun([Ft|Fts], Tout), [E|Es], RTout) :-
    check_expr_type(Env, E, Ftt), unify([], [Ftt], [Ft], _),
    check_app_type(Env, tfun(Fts, Tout), Es, RTout).

% Check a polymorphic function application
check_app_type(Env, forall(_, tfun(Ti, To)), E, RTo) :-
    check_expr_types(Env, E, Et), unify([], Ti, Et, Subs),
    deforallize(Subs, [To], [RTo]).

