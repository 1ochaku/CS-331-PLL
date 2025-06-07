% Prolog by default shows truncated answer so to avoid it.
:- set_prolog_flag(answer_write_options, [max_depth(0)]).

% To handle the nodes to mark them as faulty or not faulty during run time.
% These are predicates which refer to collection of rules and facts.
:- dynamic faultynode/1.
:- dynamic faulty/1.
:- dynamic repaire

% Loading the maze data
:- consult('Mazedata.pl').

/*
 Checking if faulty or not
 Since two types of node: predefined faulty nodes and one defined during run time
 So to handle both of them two separate functions declared.
 Since static file one can't be updated so we keep the repaired list separately.
*/
is_faulty(Node) :-
    faultynode(Node),
    \+ repaired(Node).

is_faulty(Node) :-
    faulty(Node).

% Mark an initial fault as repaired (fact inserted)
repair_node(Node) :-
    assertz(repaired(Node)).

% Revert a repaired node back to faulty
unrepair_node(Node) :-
    retract(repaired(Node)).

% Add new faulty node at runtime
add_fault(Node) :-
    assertz(faulty(Node)).

% Remove runtime-added fault
remove_fault(Node) :-
    retract(faulty(Node)).

% BFS
shortest_path(Src, Dst, Path) :-
    % First check if start or end nodes are faulty
    ( is_faulty(Src) -> 
        format('Error: Start node ~w is faulty~n', [Src]), fail
    ; is_faulty(Dst) -> 
        format('Error: Destination node ~w is faulty~n', [Dst]), fail
    ; % Otherwise proceed with BFS
      bfs_queue([[Src]], Dst, [Src], RevPath),
      reverse(RevPath, Path)
    ).

% Base case: path found
bfs_queue([[Dst|Path]|_], Dst, _, [Dst|Path]).

% Base case: queue empty - no path
bfs_queue([], _, _, _) :- !, fail.

% Recursive case: expand current node
bfs_queue([Path|RestQueue], Dst, Visited, FinalPath) :-
    Path = [Current|_],
    findall([Next|Path],
            (   (mazelink(Current, Next) ; mazelink(Next, Current)),
                \+ is_faulty(Next),
                \+ member(Next, Visited)
            ),
            NewPaths),
    % Get all new nodes we are adding
    findall(Next, member([Next|_], NewPaths), NextNodes),
    % Update visited set
    append(NextNodes, Visited, NewVisited),
    % Add new paths to queue
    append(RestQueue, NewPaths, UpdatedQueue),
    bfs_queue(UpdatedQueue, Dst, NewVisited, FinalPath).

% Printing the Path
print_path(Path) :-
    length(Path, Length),
    format('Shortest path (~d steps):~n[', [Length]),
    write_path(Path),
    write(']'), nl.

write_path([X]) :- write(X).
write_path([H|T]) :-
    write(H), write(', '),
    write_path(T).

% Test cases
testCase :-
    /*
    % for 10x10
    shortest_path(0, 99, Path1),
    print_path(Path1),
    repair_node(67),
    shortest_path(0, 99, Path2),
    print_path(Path2),
    unrepair_node(67).
    */

    % for 6x6
    ( shortest_path(0, 35, Path1) ->
    print_path(Path1)
    ; 
        writeln('Path doesn\'t exists.')
    ),
    repair_node(15),
    ( shortest_path(0, 35, Path2) ->
        print_path(Path2)
    ; 
        writeln('Path doesn\'t exists.')
    ),
    unrepair_node(15).

