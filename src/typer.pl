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
%% Check type of the program
check_prog_type(P, void) :- build_basic_env(Env), check_cmds_type(Env, P, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check a series of command
check_cmds_type(Env, [], Env).
check_cmds_type(E, [C|CS], NNE) :-
    check_dec_type(E, C, NE), check_cmds_type(NE, CS, NNE).
check_cmds_type(E, [C|CS], NE) :-
    check_stat_type(E, C, void), check_cmds_type(E, CS, NE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check a declaration
check_dec_type(Env, const(X, T, E), [X:T|Env]) :- check_expr_type(Env, E, T).
check_dec_type(Env, fun(X, Tout, Args, Body), [X:T|Env]) :-
    append(Args, Env, NEnv), check_expr_type(NEnv, Body, Tout),
    get_types_of_env(Args, Tin), append(Tin, [Tout], T).
check_dec_type(Env, funrec(X, Tout, Args, Body), [X:T|Env]) :-
    append([X:T|Args], Env, NEnv), check_expr_type(NEnv, Body, Tout),
    get_types_of_env(Args, Tin), append(Tin, [Tout], T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check a statement
check_stat_type(Env, echo(E), void) :- check_expr_type(Env, E, int).

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

%% Function application
check_expr_type(Env, app(F, Es), T) :-
    check_expr_type(Env, F, Ft), check_arg_types(Env, Ft, Es, T).
check_arg_types(_, [T|[]], [], T).
%% check_arg_types(_, T, [], T) We could use partial application
check_arg_types(Env, [Ft|Fts], [E|Es], T) :-
    check_expr_type(Env, E, Ft), check_arg_types(Env, Fts, Es, T).
