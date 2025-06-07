// This is the Main class of this program.

public class PartA {
    public static void main(String[] args) {
        // checking if the threadCount is given
        if (args.length != 1) {
            System.out.println("Usage: java PartA [number_of_threads]");
            return;
        }

        // converting it to string
        int threadCount = Integer.parseInt(args[0]);

        // bounding the thread count between 4 to 16
        if (threadCount < 4 || threadCount > 16) {
            System.out.println("Number of threads must be between 4 and 16");
            return;
        }

        // initialising the limits and n for the computation
        double a = -1;
        double b = 1;
        int n = 100000000;

        if (n % 2 != 0) {
            n++;
        }

        double h = (b - a) / n;
        int perThread = (int) (n / threadCount);

        // the computed value
        double sum = 0;
        Thread[] threads = new Thread[threadCount];
        PartA_Multithreading[] values = new PartA_Multithreading[threadCount];

        long startTime = System.currentTimeMillis();

        // create threads for parallel summation
        for (int i = 0; i < threadCount; i++) {
            int start = i * perThread;
            int end = (i == (threadCount - 1)) ? (int) n : ((i + 1) * perThread - 1);

            // here we are creating threads and assigning the values
            values[i] = new PartA_Multithreading(start, end, h, n, a);
            threads[i] = new Thread(values[i]);
            threads[i].start();
        }
        
        // now joining every threads that are created
        for (int i = 0; i < threadCount; i++) {
            try {
                threads[i].join();
                sum += values[i].getPartialSum();
            } catch (InterruptedException e) {
                // TODO: handle exception
                e.printStackTrace();
            }
        }

        sum = (h / 3.0) * sum;
        long endTime = System.currentTimeMillis();

        // displaying the calculated sum and the performance metrics
        // OBSERVATION:
        // if we keep n small, it results in long time if we use threads
        // for large values of n, it helps
        System.out.printf("Computed Sum is: %f%n", sum);
        System.out.println("Time taken: " + (endTime - startTime) + " milliseconds using " + (threadCount) + " threads for n = " + n);
    }
}