/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package masl2;

import java.awt.Color;
import java.awt.Graphics;
import javax.swing.JPanel;

public class GridDrawer implements Consts {

    int[][] Grid;
    int N, sqSize;
    private JPanel box;

    public GridDrawer(JPanel b) {
        box = b;
    }

    public void setGrid(int N) {
        this.N = N;
                sqSize = ScreenSize / N;
        Grid = new int[N + 2][N + 2];
        for (int i = 0; i < N + 2; i++) {
            Grid[0][i] = Grid[N + 1][i] = Grid[i][0] = Grid[i][N + 1] = W;
        }
    }

        int code(int agx, int agy, int tx, int ty) {
        agx -= 1;
        agy -= 1;
        if (tx == 0 && ty == 0) {
            return agx + agy * N;
        }

        tx -= 1;
        ty -= 1;
        return agx + agy * N + tx * N * N + ty * N * N * N;
    }

    int[] decode(int cod) {
        int[] ret = new int[4];
        for (int i = 0; i < 4; i++) {
            ret[i] = cod % N;
            cod = cod / N;
        }
        return ret;
    }

    public void draw() {
        Graphics g = box.getGraphics();
        g.setColor(Color.YELLOW);
        g.fillRect(0, 0, sqSize * N, sqSize * N);
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                drawOneNoDispose(i + 1, j + 1);
            }
        }
        g.dispose();
    }

    public void drawOne(int i, int j) {
        drawOneNoDispose(i, j);
        box.getGraphics().dispose();
    }

    public void drawOneNoDispose(int i, int j) {
        Graphics g = box.getGraphics();
        if (Grid[i][j] == E) {
            g.setColor(Color.YELLOW);
            g.fillRect((i - 1) * sqSize, (j - 1) * sqSize, sqSize, sqSize);
            g.setColor(Color.WHITE);
            g.drawRect((i - 1) * sqSize, (j - 1) * sqSize, sqSize, sqSize);
        } else {
            if (Grid[i][j] == O) {
                g.setColor(Color.PINK);
            } else if (Grid[i][j] == T) {
                g.setColor(Color.LIGHT_GRAY);
            } else if (Grid[i][j] == H) {
                g.setColor(Color.BLACK);
            } else if (Grid[i][j] == U) {
                g.setColor(Color.BLUE);
            }
            g.fillRect((i - 1) * sqSize, (j - 1) * sqSize, sqSize, sqSize);
        }
    }

    public void drawAg(Agent ag) {
        Graphics g = box.getGraphics();
        g.setColor(ag.col);
        g.fillOval((ag.x - 1) * sqSize, (ag.y - 1) * sqSize, sqSize, sqSize);
        g.dispose();
    }

    public void drawNew(int x, int y, Color c) {
        Graphics g = box.getGraphics();
        g.setColor(c);
        g.fillRect((x - 1) * sqSize, (y - 1) * sqSize, sqSize, sqSize);
        g.dispose();
    }

    public void drawEmpty(int x, int y) {
        Graphics g = box.getGraphics();
        g.setColor(Color.YELLOW);
        g.fillRect((x - 1) * sqSize, (y - 1) * sqSize, sqSize, sqSize);
        g.setColor(Color.WHITE);
        g.drawRect((x - 1) * sqSize, (y - 1) * sqSize, sqSize, sqSize);
        g.dispose();
    }
}
