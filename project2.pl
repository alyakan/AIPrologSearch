:- include('File.pl').
:- use_module(library(clpfd)).
:- discontiguous(agent/4).
/*
North, South, East, West.
0001, 0010, 0100, 1000.
*/

/*
    legal(S), endPoint(X, Y), agent(X, Y, _, S).
*/

/*
    $ iterative_deepening((legal(S), endPoint(X, Y), agent(X, Y, 0, S)),1,R).
S = result(moveEast, result(moveEast, result(moveSouth, result(moveWest, result(moveSouth, result(moveEast, result(moveSouth, result(moveWest, result(moveWest, result(moveEast, result(moveWest, result(moveEast, result(moveWest, result(moveEast, result(moveWest, s0))))))))))))))),
R = 34
*/

/* Initial Conditions */
agent(X, Y, Timer, s0):-
    startPoint(X, Y),
    %% TODO: pokemonLeft.
    timer(Timer).

/* Preconditions for actions */
position(moveNorth, S):-
    /*
        To be able to moveNorth, we decrease the Y by 1,
        however, we have to check if the P, which is the
        available directions from the agent's position, is
        XXX1 in binary where it can be expressed as:
            modulus(P, 2) == 1 ?
    */
    agent(X, Y, Timer, S),
    Y1 is Y - 1,
    mazeConfig(L),
    member(pos(X, Y, P, D), L),
    1 is mod(P, 2). %% It means it can move north.

position(moveSouth, S):-
    /*
        To be able to moveSouth, P must be XX1X in binary
        which can be expressed as:
            P1 = P / 2 and
            modulus(P1, 2) == 1 ?
    */
    agent(X, Y, Timer, S),
    Y1 is Y + 1,
    mazeConfig(L),
    member(pos(X, Y, P, D), L),
    P1 is div(P, 2),
    1 is mod(P1, 2). %% It means it can move South.

position(moveEast, S):-
    /*
        To be able to moveEast, P must be X1XX in binary
        which can be expressed as:
            P1 = P / 4 and
            modulus(P1, 2) == 1 ?
    */
    agent(X, Y, Timer, S),
    X1 is X + 1,
    mazeConfig(L),
    member(pos(X, Y, P, D), L),
    P1 is div(P, 4),
    1 is mod(P1, 2). %% It means it can move East.

position(moveWest, S):-
    /*
        To be able to moveEast, P must be 1XXX in binary
        which can be expressed as:
            P1 = P / 8 and
            modulus(P1, 2) == 1 ?
    */
    agent(X, Y, Timer, S),
    X1 is X - 1,
    mazeConfig(L),
    member(pos(X, Y, P, D), L),
    P1 is div(P, 8),
    1 is mod(P1, 2). %% It means it can move West.

position(grabPokemon(X, Y), S):-
    agent(X, Y, Timer, S),
    pokemon(X, Y, S).

/* Successor-state axioms for fluents */
/*

 (robot(x,s) (a avanzar a retroceder))
 (robot(x 1,s) a retroceder)
robot(x,do(a,s)) (robot(x 1,s) a avanzar)
∧ ¬ = ∨ =
+ ∧ = ∨
↔ − ∧ = ∨
agent(X, Y, T, result(A, S)) ↔ 
    [
        (T1 = T - 1 ∧ biggerThan(T1, 0) ∨ T = 0) ∧
        (
            (agent(X - 1, Y, T1, S) ∧ A=moveWest) ∨
            (agent(X + 1, Y, T1, S) ∧ A=moveEast) ∨
            (agent(X, Y - 1, T1, S) ∧ A=moveNorth) v∨
            (agent(X, Y + 1, T1, S) ∧ A=moveSouth)
        )
    ] ∨
    [   
        agent(X, Y, T, S) ∧
        ¬(A=moveNorth) ∧
        ¬(A=moveSouth) ∧
        ¬(A=moveEast) ∧
        ¬(A=moveWest)
    ]
*/
agent(C, B, T, result(A,S)):- 
    agent(W, Z, T1, S),
    (
        T1 > 0 -> T is T1 - 1 ; T is 0, writeln('T1 = 0')
        %% T is T1 - 1
    ),
    (
        %% ((T is T1 - 1, T > 0); T is T1; T1 is 0),
        (A=moveNorth, B is Z-1, C is W);
        (A=moveSouth, B is Z+1, C is W);
        (A=moveEast, C is W+1, B is Z);
        (A=moveWest, C is W-1, B is Z)
    );
    (
        agent(C, B, T, S),
        \+A=moveNorth,
        \+A=moveSouth,
        \+A=moveEast,
        \+A=moveWest
    ).

pokemon(X, Y, result(A,S)):-
    A = grabPokemon(X, Y);
    pokemon(X, Y, S),
    \+A=grabPokemon(X, Y).

/* legal Axioms */
legal(s0).
legal(result(A, S)):- legal(S), position(A,S).

iterative_deepening(Goal, Limit, Result):-
    call_with_depth_limit(Goal,Limit,Result),
    Result \= depth_limit_exceeded.

iterative_deepening(Goal,Limit,Result):-
    call_with_depth_limit(Goal,Limit,Result),
    Result == depth_limit_exceeded,
    Limit2 is Limit + 1,
    writeln(Limit2),
    iterative_deepening(Goal,Limit2,R2).


main.