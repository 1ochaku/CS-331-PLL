// This is the Main class of this java program
public class PartB {
    public static void main(String args[]) {
        // reading input
        if (args.length != 1) {
            System.out.println("Usage: java PartB [number_of_threads]");
            return;
        }

        // converting it to string
        int threadCount = Integer.parseInt(args[0]);

        // checking if valid thread count
        if (threadCount <= 0) {
            System.out.println("Thread count should be a non zero positive value.");
            return;
        }

        // initialising the variables
        int n = 1000;
        int[][] A = new int[n][n];
        int[][] B = new int[n][n];
        int[][] C = new int[n][n];
        int perThread = (int) (n / threadCount);

        Thread[] threads = new Thread[threadCount];
        PartB_Fill_Matrix[] initialisation = new PartB_Fill_Matrix[threadCount];
        PartB_Matrix_Multiplication[] product = new PartB_Matrix_Multiplication[threadCount];

        long startTime = System.currentTimeMillis();

        // FILLING MATRIX A
        for (int i = 0; i < threadCount; i++) {
            int start = i * perThread;
            int end = (i == (threadCount - 1)) ? (int) n : ((i + 1) * perThread);

            initialisation[i] = new PartB_Fill_Matrix(n, start, end);
            threads[i] = new Thread(initialisation[i]);
            threads[i].start();
        }

        // now joining every threads that are created
        for (int i = 0; i < threadCount; i++) {
            try {
                threads[i].join();
                int[][] matrix = initialisation[i].getMatrix();

                // replacing the values in the matrix
                int start = i * perThread;
                int end = (i == (threadCount - 1)) ? (int) n : ((i + 1) * perThread);
                for (int j = start; j < end; j++) {
                    for (int k = 0; k < n; k++) {
                        A[j][k] = matrix[j - start][k];
                    }
                }

            } catch (InterruptedException e) {
                // TODO: handle exception
                e.printStackTrace();
            }
        }

        // FILLING MATRIX B
        for (int i = 0; i < threadCount; i++) {
            int start = i * perThread;
            int end = (i == (threadCount - 1)) ? (int) n : ((i + 1) * perThread);

            initialisation[i] = new PartB_Fill_Matrix(n, start, end);
            threads[i] = new Thread(initialisation[i]);
            threads[i].start();
        }

        // now joining every threads that are created
        for (int i = 0; i < threadCount; i++) {
            try {
                threads[i].join();
                int[][] matrix = initialisation[i].getMatrix();

                // replacing the values in the matrix
                int start = i * perThread;
                int end = (i == (threadCount - 1)) ? (int) n : ((i + 1) * perThread);
                for (int j = start; j < end; j++) {
                    for (int k = 0; k < n; k++) {
                        B[j][k] = matrix[j - start][k];
                    }
                }

            } catch (InterruptedException e) {
                // TODO: handle exception
                e.printStackTrace();
            }
        }

        // for debugging
        // for (int i = 0; i < n; i++) {
        //     for (int j = 0; j < n; j++) {
        //         System.out.printf(A[i][j] + " ");
        //     }
        //     System.out.println('\n');
        // }

        // System.out.println("-------------------------------");

        // for (int i = 0; i < n; i++) {
        //     for (int j = 0; j < n; j++) {
        //         System.out.printf(B[i][j] + " ");
        //     }
        //     System.out.println('\n');
        // }

        long endTime = System.currentTimeMillis();

        // displaying the time taken to initialise the matrix
        System.out.println("Time taken: " + (endTime - startTime) + " milliseconds using " + (threadCount)
                + " threads for initialising the matrix.");

        // computing the matrix product
        startTime = System.currentTimeMillis();

        for (int i = 0; i < threadCount; i++) {
            int start = i * perThread;
            int end = (i == (threadCount - 1)) ? (int) n : ((i + 1) * perThread);

            product[i] = new PartB_Matrix_Multiplication(n, start, end, A, B);
            threads[i] = new Thread(product[i]);
            threads[i].start();
        }

        // now joining every threads that are created
        for (int i = 0; i < threadCount; i++) {
            try {
                threads[i].join();
                int[][] matrix = product[i].getMatrix();

                // filling the values that are computed
                int start = i * perThread;
                int end = (i == (threadCount - 1)) ? (int) n : ((i + 1) * perThread);
                for (int j = start; j < end; j++) {
                    for (int k = 0; k < n; k++) {
                        C[j][k] = matrix[j - start][k];
                    }
                }

            } catch (InterruptedException e) {
                // TODO: handle exception
                e.printStackTrace();
            }
        }

        endTime = System.currentTimeMillis();

        // for debugging
        // for (int i = 0; i < n; i++) {
        //     for (int j = 0; j < n; j++) {
        //         System.out.printf(C[i][j] + " ");
        //     }
        //     System.out.println('\n');
        // }

        // printing the performance
        System.out.println("Time taken: " + (endTime - startTime) + " milliseconds using " + (threadCount)
                + " threads for computing Matrix C.");
    }
}
