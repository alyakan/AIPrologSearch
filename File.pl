mazeConfig(X):- 
 X = [pos(0,0,6,0),pos(0,1,3,1),pos(0,2,5,0),pos(0,3,4,0),pos(1,0,12,0),pos(1,1,6,0),pos(1,2,9,1),pos(1,3,12,1),pos(2,0,8,0),pos(2,1,12,0),pos(2,2,6,0),pos(2,3,13,1),pos(3,0,2,0),pos(3,1,11,0),pos(3,2,9,1),pos(3,3,8,0)].

startPoint(2,0).

endPoint(2,1).

timer(14).

pokemon(0,1,s0).
pokemon(1,2,s0).
pokemon(1,3,s0).
pokemon(2,3,s0).
pokemon(3,2,s0).
pokemonLeft(5).