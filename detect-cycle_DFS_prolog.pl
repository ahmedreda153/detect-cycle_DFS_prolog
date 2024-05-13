% To start the program, call predicate 1 or 2:
% 1. start_detect_cycle(3, 3, [[1, 1, blue], [1, 2, yellow], [1, 3, blue], [2, 1, red], [2, 2, blue], [2, 3, blue], [3, 1, red], [3, 2, blue], [3, 3, blue]]).
% 2. start_detect_cycle(4, 4, [[1, 1, yellow], [1, 2, yellow], [1, 3, yellow], [1, 4, red], [2, 1, blue], [2, 2, yellow], [2, 3, blue], [2, 4, yellow], [3, 1, blue], [3, 2, blue], [3, 3, blue], [3, 4, yellow], [4, 1, blue], [4, 2, blue], [4, 3, blue], [4, 4, yellow]).

% This program implements a cycle detection algorithm using Depth-First Search (DFS) for a grid with colored nodes.
% The algorithm operates as follows:
% 1. Initialize with the first node, exploring adjacent same-colored nodes.
% 2. Track all visited nodes to prevent revisits and ensure cycle detection.
% 3. Upon revisiting a node started from, confirm a cycle if it includes at least four nodes.
% 4. Report the presence of a cycle along with its path.
% 5. If no cycle is found, report that no cycles exist.

% Entry point to start cycle detection in a grid.
start_detect_cycle(N, M, Grid):-
    detect_cycle(N, M, Grid, Grid).

start_detect_cycle(_, _, _):-
    write("No Cycles Found!"), nl.

% Attempts to find a cycle starting from any node in the grid.
detect_cycle(_, _, [], _):- fail.
detect_cycle(N, M, [H|_], Grid):-
    search([[H, null]], [], H, N, M, Grid),
    !.
detect_cycle(N, M, [_|T], Grid):-
    detect_cycle(N, M, T, Grid).

% Checks if the cycle is detected by comparing the current node with the goal.
search(Stack, Visited, [XStart, YStart, _], _, _, _):-
    getState(Stack, [[X, Y, Color], Parent], _),
    length(Stack, CycleLength),
    CycleLength >= 4,
    X is XStart + 1,
    Y is YStart,
    !,
    write("Cycle is Found!"), nl,
    printSolution([[X, Y, Color], Parent], Visited).

% Performs DFS to search for cycles.
search(Stack, Visited, MyGoal, N, M, Grid):-
    getState(Stack, CurrentNode, _),
    getNextState(CurrentNode, Stack, Visited, Children, N, M, Grid),
    addChildren([Children], Stack, NewStack),
    append(Visited, [CurrentNode], NewVisited),
    search(NewStack, NewVisited, MyGoal, N, M, Grid).

% Gets the current state from the stack. (which is the top of the stack)
getState([CurrentNode|Rest], CurrentNode, Rest).

% Gets all valid next states from the current state.
getNextState([State, _], Stack, Visited, [NextNode, State], N, M, Grid):-
    move(State, Next, N, M),
    search_getColor(Next, Grid, NextNode),
    checkColor(State, NextNode),
    not(member([NextNode, _], Stack)),
    not(member([NextNode, _], Visited)).

% Adds children to the stack.
addChildren(Children, Stack, NewStack):-
    append(Children, Stack, NewStack).

% Movement rules within the grid.
move(State, Next, N, M):-
    left(State, Next, M);
    right(State, Next, M);
    up(State, Next, N);
    down(State, Next, N).
    
left([X,Y,_], [X1,Y1,_], M):-
    Y1 is Y-1, X1 is X, Y1 > 0, Y1 =< M.

right([X,Y,_], [X1,Y1,_], M):-
    Y1 is Y+1, X1 is X, Y1 > 0, Y1 =< M.

up([X,Y,_], [X1,Y1,_], N):-
    X1 is X-1, Y1 is Y, X1 > 0, X1 =< N.

down([X,Y,_], [X1,Y1,_], N):-
    X1 is X+1, Y1 is Y, X1 > 0, X1 =< N.

% Finds the color of a node given its coordinates.
search_getColor(_, [], _):- fail.
search_getColor([X, Y, _], [[X1, Y1, Color]|_], Result) :-
    X is X1, Y is Y1, Result = [X1, Y1, Color],
    !.
search_getColor([X, Y, _], [[_, _, _]|T], Result) :-
    search_getColor([X, Y, _], T, Result).

% Helper to check if two nodes have the same color.
checkColor([_, _, Color], [_, _, Color1]):- 
    Color == Color1.

% Prints the solution path recursively.
printSolution([State, null],_):-
    write(State), nl.
printSolution([State, Parent], Visited):-
    member([Parent, GrandParent], Visited),
    printSolution([Parent, GrandParent], Visited),
    write(State), nl.