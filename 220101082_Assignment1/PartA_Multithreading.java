public class PartA_Multithreading implements Runnable {
    private final int start;
    private final int end;
    private final double h;
    private final int n;
    private final double a;
    private static final double coefficient = 1.0 / (Math.sqrt(2.0 * Math.PI));
    private double exponent;
    private double partialSum = 0;

    public PartA_Multithreading(int start, int end, double h, int n, double a) {
        this.start = start;
        this.end = end;
        this.h = h;
        this.n = n;
        this.a = a;
    }

    public double getPartialSum() {
        return partialSum;
    }
    
    @Override
    public void run() {
        // computes the partial sum for the particular the set it receives
        for (int i = start; i <= end; i++) {
            double x = a + i * h;
            exponent = Math.exp(-(Math.pow(x, 2)) / 2);

            double sum = coefficient * exponent;

            if (i == 0 || i == n) {
                partialSum += sum;
            } else if (i % 2 == 0) {
                partialSum += 2 * sum;
            } else {
                partialSum += 4 * sum;
            }
        }
    }
}
