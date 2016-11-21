:- include('File.pl').
:- use_module(library(clpfd)).
/*
North, South, East, West.
0001, 0010, 0100, 1000.
*/

/*
    legal(S), endPoint(X, Y), agent(X, Y, _, S).
*/

/*
    $ iterative_deepening((legal(S), agent(2, 3, 0, S)),50,R).
    $ call_with_depth_limit((legal(S), agent(2, 3, 0, S)),50,R).
S = result(moveEast, result(moveEast, result(moveSouth, result(moveWest, result(moveSouth, result(moveEast, result(moveSouth, result(moveWest, result(moveWest, result(moveEast, result(moveWest, result(moveEast, result(moveWest, result(moveEast, result(moveWest, s0))))))))))))))),
R = 34
*/

/* Preconditions for actions */
position(moveNorth, S):-
    agent(X, Y, Timer, S),
    Y1 is Y - 1,
    %% TODO: can we move forward ?
    mazeConfig(L),
    member(pos(X, Y, P, D), L),
    1 is mod(P, 2). %% It means it can move north.

position(moveSouth, S):-
    agent(X, Y, Timer, S),
    Y1 is Y + 1,
    %% TODO: can we move forward ?
    mazeConfig(L),
    member(pos(X, Y, P, D), L),
    P1 is div(P, 2),
    1 is mod(P1, 2). %% It means it can move South.

position(moveEast, S):-
    agent(X, Y, Timer, S),
    X1 is X + 1,
    %% TODO: can we move forward ?
    mazeConfig(L),
    member(pos(X, Y, P, D), L),
    P1 is div(P, 4),
    1 is mod(P1, 2). %% It means it can move East.

position(moveWest, S):-
    agent(X, Y, Timer, S),
    X1 is X - 1,
    %% TODO: can we move forward ?
    mazeConfig(L),
    member(pos(X, Y, P, D), L),
    P1 is div(P, 8),
    1 is mod(P1, 2). %% It means it can move West.


/* Initial Conditions */
agent(X, Y, Timer, s0):-
    startPoint(X, Y),
    %% TODO: pokemonLeft.
    timer(Timer).

/* Successor-state axioms for fluents */

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