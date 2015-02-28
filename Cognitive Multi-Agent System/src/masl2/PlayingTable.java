/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package masl2;

import java.util.Random;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.swing.JPanel;

public class PlayingTable extends GridDrawer implements Runnable {

    int ObstNr = 10, TileNr = 15, HoleNr = 10, sleepTime = 300,
            holeReplaceTime = 5;
    private int maxR = 1000000;
    Vector<Integer> TableHoles = null;
    CogAgFrame Parent;
    int Turn, PlayerNr;
    Agent[] ag;

    public PlayingTable(JPanel b, CogAgFrame parent) {
        super(b);
        this.Parent = parent;
        this.PlayerNr = parent.PlayerNr;
    }

    public void updatePoints(int points, int nr) {
        Parent.updatePoints(points, nr);
    }

    public void emptyGrid() {
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                Grid[i + 1][j + 1] = E;
            }
        }
    }

    int code(int agx, int agy) {
        agx -= 1;
        agy -= 1;
        return agx + agy * N;
    }

    public void generate(int n, int obstNr, int holeNr, int tileNr, int holeReplaceTime) {
        setGrid(n);
        ObstNr = obstNr;
        HoleNr = holeNr;
        TileNr = tileNr;
        this.holeReplaceTime = holeReplaceTime;
        genArtf();

        draw();
    }

    public int genArtf() {
        int ob = 0, i, j, max = 0;
        boolean cont;
        Random rand = new Random();
        ob = max = 0;
        while (ob < ObstNr && max < maxR) {
            max++;
            i = rand.nextInt(N) + 1;
            j = rand.nextInt(N) + 1;
            if (i == 1 || i == N || j == 1 || j == N) {
                continue;
            }
            cont = false;
            for (int x = -1; x < 2; x++) {
                for (int y = -1; y < 2; y++) {
                    if (Grid[i + x][j + y] == O) {
                        cont = true;
                    }
                }
            }
            if (!cont) {
                Grid[i][j] = O;
                ob++;
            }
        }
        if (max == maxR) {
            System.out.print("Maybe n can not fit so many obstacles");
            return -1;
        }
        ob = max = 0;
        while (ob < TileNr && max < maxR) {
            max++;
            i = rand.nextInt(N - 2) + 2;
            j = rand.nextInt(N - 2) + 2;
            if (Grid[i][j] == E && (i != 1 && j != 1 && i != N && j != N)) {
                Grid[i][j] = T;
                ob++;
            }
        }

        if (max == maxR) {
            System.out.print("Maybe n can not fit so many cookies");
            return -1;
        }

        ob = max = 0;
        TableHoles = new Vector<Integer>();
        while (ob < HoleNr && max < maxR) {
            max++;
            i = rand.nextInt(N) + 1;
            j = rand.nextInt(N) + 1;
            if (Grid[i][j] == E && (i != 1 && j != 1 && i != N && j != N)) {
                Grid[i][j] = H;
                TableHoles.add(code(i, j));
                ob++;
            }

        }
        return 0;
    }

    public int move_ag(int agNr) {
        int ret = Parent.ag[agNr].move_ag();
        int nr_in_square = 0;
        int point_sum = 0;
        Parent.ag[agNr].SameSquare.clear();
        for (int i = 0; i < PlayerNr; i++) {
            if (Parent.ag[i].x == Parent.ag[agNr].x &&
                    Parent.ag[i].y == Parent.ag[agNr].y) {
                point_sum += Parent.ag[i].Points;
                nr_in_square++;
                if (!Parent.ag[agNr].SameSquare.contains(i))
                    Parent.ag[agNr].SameSquare.add(i);
                if (!Parent.ag[i].SameSquare.contains(agNr))
                    Parent.ag[i].SameSquare.add(agNr);

            }
        }
        for (int i = 0; i < PlayerNr; i++) {
            if (Parent.ag[i].x == Parent.ag[agNr].x &&
                    Parent.ag[i].y == Parent.ag[agNr].y) {
                Parent.ag[i].Points = point_sum / nr_in_square;
            }
        }
        return ret;
    }

    public void replace_hole() {
        Random rand = new Random();
        int i;
        HoleNr = TableHoles.size();
        for (i = 0; i < TableHoles.size(); i++) {
            int[] dec = decode(TableHoles.elementAt(i));
            Grid[dec[0] + 1][dec[1] + 1] = E;
            drawEmpty(dec[0] + 1, dec[1] + 1);
        }
        TableHoles.clear();
        i = 0;
        boolean go;
        int ob = 0, newHoleNr = HoleNr, max = 0, j = 0;
        while (ob < newHoleNr && max < 1000000) {
            max++;
            i = rand.nextInt(N) + 1;
            j = rand.nextInt(N) + 1;
            go = true;

            if (Grid[i][j] != E) {
                go = false;
            } else {
                for (int k = 0; k < PlayerNr; k++) {
                    if (ag[k].x == i && ag[k].y == j) {
                        go = false;
                    }
                }
            }
            if (go) {
                TableHoles.add(code(i, j, 0, 0));
                Grid[i][j] = H;
                drawOne(i, j);
                ob++;
            }
        }
        if (max == 1000000) {
            System.out.print("Can not replace so many holes");
        }

    }

    public void run() {
        int ret = 0, holereplace = 0;
        while (ret != 1) {

            ret = move_ag(0);

            ret *= move_ag(1);
            ret *= move_ag(2);
            ret *= move_ag(3);

            if (TileNr == 0) {
                break;
            }

            if (holereplace == holeReplaceTime) {
                replace_hole();
                holereplace = 0;
            }
            holereplace ++;
            try {
                Thread.sleep(sleepTime);
            } catch (InterruptedException ex) {
                Logger.getLogger(PlayingTable.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        int tiles[] = new int[4];
        for (int i = 0; i < 4; i++) {
            for (int j = 0; j < Parent.ag[i].Tiles.size(); j++) {
                if (Parent.ag[i].Tiles.elementAt(j) != null) {
                    tiles[i]++;
                }
            }
            System.out.print(tiles[i] + " " + i + "\n");
        }
    }
}
