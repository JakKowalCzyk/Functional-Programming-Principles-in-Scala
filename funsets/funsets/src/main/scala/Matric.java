import java.time.Duration;
import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.concurrent.TimeUnit;

class Matric {


    public static void main(String[] args) {
        Instant old = Instant.now().truncatedTo(ChronoUnit.MICROS);
        final long startTime = System.currentTimeMillis();

        int mat1[][] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};

        int mat2[][] = {{10, 11, 12}, {13, 14, 15}, {16, 17, 18}};
        int result[][] = new int[3][3];

        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                result[i][j] = 0;
                for (int k = 0; k < 3; k++)
                    result[i][j] += mat1[i][k] * mat2[k][j];
            }
        }

        final long endTime = System.currentTimeMillis();
        long time = endTime - startTime;
        System.out.println("Time: " + Duration.between( old , Instant.now().truncatedTo(ChronoUnit.MICROS)) + "\n");


        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++)
                System.out.print(result[i][j] + " ");
        }
    }
} 