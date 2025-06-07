
import java.util.Random;

public class PartB_Fill_Matrix implements Runnable {
    private final int size;
    private final int start;
    private final int end;
    private int[][] matrix;

    public PartB_Fill_Matrix(int n, int start, int end) {
        this.size = n;
        this.start = start;
        this.end = end;
        this.matrix = new int[end-start][size];
    }

    public int[][] getMatrix() {
        return matrix;
    }
    
    @Override
    public void run() {
        Random rand = new Random();
        // initialises the matrix
        for (int i = 0; i < (end-start); i++) {
            for (int j = 0; j < size; j++) {
                matrix[i][j] = rand.nextInt(11);
            }
        }
    }
}
