# How to run the Program

Open the terminal
Enter to compile the maze generator: g++ generateMaze.cpp -o generateMaze
To generate a maze: ./generateMaze Height:int Width:int FaultProbability:fp<0,1> SrcX SrcY DstX DstY
Example $./generateMaze 10 10 0.3 1 1 9 9
Next Enter: swipl
Load the file: [shortestPath].
Call the function: shortest_path(src, dst, Path).
To run test cases: testCase.
To exit the interactive program: halt.

NOTE: No of arguments strictly required is 3. And the src and dst should be INT, and Path as Path.
