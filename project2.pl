:- include('File.pl').
:- use_module(library(clpfd)).
/*
North, South, East, West.
0001, 0010, 0100, 1000.
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
    agent(W, Z, T, S),
    (
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


main.