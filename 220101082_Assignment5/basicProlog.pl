/*
    Task 1: to check for duplicates in the given list
    > To run the program: swipl
    > [basicProlog].
    > has_duplicates([.]).
*/
has_duplicates([]) :- 
    write('No'), nl.
has_duplicates([H|T]) :- 
    member(H, T),
    !, 
    write('Yes'), nl.
has_duplicates([_|T]) :- 
    has_duplicates(T).

/*
    Task 2: to find the square root of the number
    > To run the program: swipl
    > [basicProlog].
    > squareroot(X,result,accuracy).
*/

squareroot(X,Result,Accuracy) :-
    X>=0,
    Accuracy>0,
    Guess is X/3,
    find(X,Guess,Result,Accuracy).

find(X,Guess,Result,Accuracy) :-
    Diff is abs(Guess*Guess-X),
    Diff < Accuracy,
    Result is Guess, !.

find(X,Guess,Result,Accuracy) :-
    Diff is abs(Guess*Guess-X),
    Diff >= Accuracy,
    NextGuess is (Guess + X/Guess) / 2,
    find(X,NextGuess,Result,Accuracy).

