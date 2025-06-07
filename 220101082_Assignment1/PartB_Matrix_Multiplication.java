public class PartB_Matrix_Multiplication implements Runnable {
    private final int size;
    private final int start;
    private final int end;
    private int[][] A;
    private int[][] B;
    private int[][] matrix;

    public PartB_Matrix_Multiplication(int n, int start, int end, int[][] A, int[][]B) {
        this.size = n;
        this.start = start;
        this.end = end;
        this.A = A;
        this.B = B;
        this.matrix = new int[end-start][size];
    }

    public int[][] getMatrix() {
        return matrix;
    }
    
    @Override
    public void run() {
        // computes the product of two matrices
        for (int i = start; i < end; i++) {
            for (int j = 0; j < size; j++) {
                for (int col = 0; col < size; col++) {
                    matrix[i - start][j] += (A[i][col] * B[col][j]);
                }
            }
        }
    }
}
