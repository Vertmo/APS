%% Toplevel function
check_type(P) :- check_prog_type(P, void), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Basic environment
build_basic_env(["true":bool,"false":bool,
                 "not":[bool,bool],"and":[bool,bool,bool],"or":[bool,bool,bool],
                 "eq":[int,int,bool],"lt":[int,int,bool],
                 "add":[int,int,int],"sub":[int,int,int],
                 "mul":[int,int,int],"div":[int,int,int]]).

%% Find type in the environment
check_in_env([S:T|_], S, T).
check_in_env([_|EnvS], S, T) :- check_in_env(EnvS, S, T).

%% Get only the types of a sym/type pair list (env)
get_types_of_env([], []).
get_types_of_env([_:T|EnvS], [T|TypeS]) :- get_types_of_env(EnvS, TypeS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unify type with a type variable if possible
unify_t(_, T, T).
unify_t([S:T|_], tvar(S), T).
% For vectors
unify_t(TEnv, vec(S), vec(T)) :- unify_t(TEnv, S, T).
% For functions
unify_t(_, [], []).
unify_t(TEnv, [Tn|TnS], [T|TS]) :- unify_t(TEnv, Tn, T), unify_t(TEnv, TnS, TS).
% Pregressing in the list
unify_t([_|TEnvS], tvar(S), T) :- unify_t(TEnvS, tvar(S), T).

%% Bind a type variable
% Applying a passed parametric function
bind_t(TEnv, tvar(Tn), tvar(Tn), TEnv) :- !.
bind_t(TEnv, tvar(Tn), tvar(Tn2), TEnv) :- Tn \= Tn2, !, fail.
% Final cases for a "normal" case
bind_t([], tvar(Tn), T, [Tn:T]).
bind_t([], T, T, []).
% Duplicate variable, either coherent or incoherent
bind_t([Tn:T|TS], tvar(Tn), T2, [Tn:T|TS]) :- !, unify_t([Tn:T|TS], T, T2).
bind_t([Tn2:T2|TS], tvar(Tn), T, [Tn2:T2|TEnvS]) :- Tn \= Tn2, !, bind_t(TS, tvar(Tn), T, TEnvS).
% For vectors
bind_t(TEnv, vec(Tn), vec(T), TEnvo) :- !, bind_t(TEnv, Tn, T, TEnvo).
% For functions
bind_t(TEnv, [], [], TEnv).
bind_t(TEnv, [Tn|TnS], [T|TS], TEnvo) :- !, bind_t(TEnv, Tn, T, TEnvE), bind_t(TEnvE, TnS, TS, TEnvo).
% Progressing in the list
bind_t([TEnvH|TEnvS], T, T2, [TEnvH|TEnvSO]) :- bind_t(TEnvS, T, T2, TEnvSO).

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
check_dec_type(Env, const(X, T, E), [X:T|Env]) :- check_expr_type(Env, E, T).
check_dec_type(Env, fun(X, Tout, Args, Body), [X:T|Env]) :-
    get_types_of_env(Args, Tin), append(Tin, [Tout], T),
    append(Args, Env, NEnv), check_expr_type(NEnv, Body, Tout).
check_dec_type(Env, funrec(X, Tout, Args, Body), [X:T|Env]) :-
    get_types_of_env(Args, Tin), append(Tin, [Tout], T),
    append([X:T|Args], Env, NEnv), check_expr_type(NEnv, Body, Tout).
check_dec_type(Env, var(X, T), [X:T|Env]).
check_dec_type(Env, proc(X, Args, Body), [X:T|Env]) :-
    get_types_of_env(Args, Tin), append(Tin, [void], T),
    append(Args, Env, NEnv), check_cmds_type(NEnv, Body, _, void).
check_dec_type(Env, procrec(X, Args, Body), [X:T|Env]) :-
    get_types_of_env(Args, Tin), append(Tin, [void], T),
    append([X:T|Args], Env, NEnv), check_cmds_type(NEnv, Body, _, void).
check_dec_type(Env, funproc(X, Tout, Args, Body), [X:T|Env]) :-
    get_types_of_env(Args, Tin), append(Tin, [Tout], T),
    append(Args, Env, NEnv), check_cmds_type(NEnv, Body, _, Tout).
check_dec_type(Env, funprocrec(X, Tout, Args, Body), [X:T|Env]) :-
    get_types_of_env(Args, Tin), append(Tin, [Tout], T),
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
    check_in_env(Env, X, Pt), check_app_type(Env, [], Pt, Es, void).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check an expression
check_expr_type(_, E, int) :- number(E).
check_expr_type(Env, sym(S), T) :- check_in_env(Env, S, T).
check_expr_type(Env, abs(Args, Body), T) :-
    append(Args, Env, NEnv), check_expr_type(NEnv, Body, Tout),
    get_types_of_env(Args, Tin), append(Tin, [Tout], T).
check_expr_type(Env, if(Cond, Then, Else), T) :-
    check_expr_type(Env, Cond, bool),
    check_expr_type(Env, Then, T), check_expr_type(Env, Else, T).
check_expr_type(Env, app(sym("alloc"), [E]), vec(_)) :- check_expr_type(Env, E, int).
check_expr_type(Env, app(sym("nth"), [E1,E2]), T) :-
    check_expr_type(Env, E1, vec(T)), check_expr_type(Env, E2, int).
check_expr_type(Env, app(sym("len"), [E]), int) :-
    check_expr_type(Env, E, vec(_)).
check_expr_type(Env, app(F, Es), T) :-
    check_expr_type(Env, F, Ft), check_app_type(Env, [], Ft, Es, T).
check_expr_type(Env, let(X, E, B), T) :-
    check_expr_type(Env, E, T1), check_expr_type([X:T1|Env], B, T).

%% Check a function application (by checking passed arguments and return type)
check_app_type(_, TEnv, [T|[]], [], To) :- unify_t(TEnv, T, To).
check_app_type(Env, TEnvI, [Ft|Fts], [E|Es], T) :-
    check_expr_type(Env, E, Ftr), bind_t(TEnvI, Ft, Ftr, TEnvE),
    check_app_type(Env, TEnvE, Fts, Es, T).
