/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package masl2;

import java.awt.Color;
import java.util.LinkedList;
import java.util.Random;
import java.util.Stack;
import java.util.Vector;
import javax.swing.JPanel;

/**
 *
 * @author Monti
 */
public class Agent extends GridDrawer {

    PlayingTable Table;
    int x, y, AgentNr;
    Color col;
    boolean moveAg = false, fooled = false, newhole = false;
    Vector<Integer> AccesibleUnknown = new Vector<Integer>();
    Vector<Integer> Tiles = new Vector<Integer>();
    Vector<Integer> Holes = new Vector<Integer>();
    int Points = 0, moveType = -1, movingTileX, movingTileY;
    int OpponentX[], OpponentY[];
    Vector<Integer> SameSquare = new Vector<Integer>();

    Agent(PlayingTable t, JPanel AgentPanel, int nr) {
        super(AgentPanel);
        this.Table = t;
        AgentNr = nr;
    }

    void setAgent(int x, int y) {
        fooled = false;
        this.x = x;
        this.y = y;
        setGrid(Table.N);
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                Grid[i + 1][j + 1] = U;
            }
        }
        Grid[x][y] = E;
        discover();
        draw();
        drawAg();
        OpponentX = new int[Table.PlayerNr];
        OpponentY = new int[Table.PlayerNr];
        for (int i = 0; i < Table.PlayerNr; i++) {
            OpponentX[i] = OpponentY[i] = -1;
        }
    }

    public void drawAg() {
        Table.drawAg(this);
        super.drawAg(this);
    }



    Stack<Integer> go_stack(int agx, int agy, int tox, int toy) {
        int[][] g = Grid;
        int c, n = Table.N, dim = n * n * n * n;
        int[] parent = new int[dim], len = new int[dim];
        for (int i = 0; i < dim; i++) {
            parent[i] = -1;
            len[i] = 0;
        }
        LinkedList<Integer> tovisit = new LinkedList<Integer>();
        Stack<Integer> ret = new Stack<Integer>();
        c = code(agx, agy, 0, 0);
        parent[c] = -2;// the first position is the only one that has parent -2
        tovisit.push(c);
        while (!tovisit.isEmpty()) {
            int now = tovisit.getLast();
            tovisit.removeLast();
            int[] dec = decode(now);
            agx = dec[0] + 1;
            agy = dec[1] + 1;
            if (agx == tox && agy == toy) {
                now = parent[now];
                while (parent[now] != -2) {
                    ret.push(now);
                    now = parent[now];
                }
                //         ret.push(now);
                return ret;

            }
            if (g[agx + 1][agy] == E || (agx + 1 == tox && agy == toy && g[agx + 1][agy] == U)) {
                c = code(agx + 1, agy, 0, 0);
                if (parent[c] == -1 || (len[now] + 1 < len[c])) {
                    parent[c] = now;
                    len[c] = len[now] + 1;
                    tovisit.push(c);
                }

            }
            if (g[agx - 1][agy] == E || (agx - 1 == tox && agy == toy && g[agx - 1][agy] == U)) {
                c = code(agx - 1, agy, 0, 0);
                if (parent[c] == -1 || (len[now] + 1 < len[c])) {
                    parent[c] = now;
                    len[c] = len[now] + 1;
                    tovisit.push(c);
                }

            }
            if (g[agx][agy + 1] == E || (agx == tox && agy + 1 == toy && g[agx][agy + 1] == U)) {
                c = code(agx, agy + 1, 0, 0);
                if (parent[c] == -1 || (len[now] + 1 < len[c])) {
                    parent[c] = now;
                    len[c] = len[now] + 1;
                    tovisit.push(c);
                }
            }
            if (g[agx][agy - 1] == E || (agx == tox && agy - 1 == toy && g[agx][agy - 1] == U)) {
                c = code(agx, agy - 1, 0, 0);
                if (parent[c] == -1 || (len[now] + 1 < len[c])) {
                    parent[c] = now;
                    len[c] = len[now] + 1;
                    tovisit.push(c);
                }

            }
        }
        return null;
    }

    void move(Stack<Integer> s) {
        if (s.isEmpty()) {
            return;
        }

        int[] dec = decode(s.pop());
        Table.drawEmpty(x, y);
        drawEmpty(x, y);
        if (Grid[dec[0] + 1][dec[1] + 1] != E) {
            return;
        }

        x = dec[0] + 1;
        y = dec[1] + 1;
        //    discover();
        drawAg();

        Points -= 1;
        Table.updatePoints(Points, AgentNr);
    }

    Stack<Integer> build(int agx, int agy, int tx, int ty) {
        int[][] g = Grid;
        int c, n = Table.N, dim = n * n * n * n;
        int[] parent = new int[dim], len = new int[dim];
        for (int i = 0; i < dim; i++) {
            parent[i] = -1;
            len[i] = 0;
        }

        LinkedList<Integer> tovisit = new LinkedList<Integer>();
        Stack<Integer> ret = new Stack<Integer>();
        c = code(agx, agy, tx, ty);
        parent[c] = -2;// the first position is the only one that has parent -2
        tovisit.push(c);
        while (!tovisit.isEmpty()) {
            int now = tovisit.getLast();
            tovisit.removeLast();
            int[] dec = decode(now);
            agx = dec[0] + 1;
            agy = dec[1] + 1;
            tx = dec[2] + 1;
            ty = dec[3] + 1;
            if (g[tx][ty] == H) {
                while (parent[now] != -2) {
                    ret.push(now);
                    now = parent[now];
                }
                //              ret.push(now);
                return ret;

            }
            if (g[agx + 1][agy] == E && (agx + 1 != tx || agy != ty)) {
                c = code(agx + 1, agy, tx, ty);
                if (parent[c] == -1 || (len[now] + 1 < len[c])) {
                    parent[c] = now;
                    len[c] = len[now] + 1;
                    tovisit.push(c);
                }

            }
            if (g[agx - 1][agy] == E && (agx - 1 != tx || agy != ty)) {
                c = code(agx - 1, agy, tx, ty);
                if (parent[c] == -1 || (len[now] + 1 < len[c])) {
                    parent[c] = now;
                    len[c] = len[now] + 1;
                    tovisit.push(c);
                }

            }
            if (g[agx][agy + 1] == E && (agx != tx || agy + 1 != ty)) {
                c = code(agx, agy + 1, tx, ty);
                if (parent[c] == -1 || (len[now] + 1 < len[c])) {
                    parent[c] = now;
                    len[c] = len[now] + 1;
                    tovisit.push(c);
                }
            }
            if (g[agx][agy - 1] == E && (agx != tx || agy - 1 != ty)) {
                c = code(agx, agy - 1, tx, ty);
                if (parent[c] == -1 || (len[now] + 1 < len[c])) {
                    parent[c] = now;
                    len[c] = len[now] + 1;
                    tovisit.push(c);
                }
            }
            if ((tx == agx + 1) && (ty == agy) && ((g[tx + 1][ty] == E) || g[tx + 1][ty] == H)) {
                c = code(agx + 1, agy, tx + 1, ty);
                if (parent[c] == -1 || (len[now] + 1 < len[c])) {
                    parent[c] = now;
                    len[c] = len[now] + 1;
                    tovisit.push(c);
                }
            }
            if ((tx == agx - 1) && (ty == agy) && (g[tx - 1][ty] == E || g[tx - 1][ty] == H)) {
                c = code(agx - 1, agy, tx - 1, ty);
                if (parent[c] == -1 || (len[now] + 1 < len[c])) {
                    parent[c] = now;
                    len[c] = len[now] + 1;
                    tovisit.push(c);
                }
            }
            if ((tx == agx) && (ty == agy + 1) && (g[tx][ty + 1] == E || g[tx][ty + 1] == H)) {
                c = code(agx, agy + 1, tx, ty + 1);
                if (parent[c] == -1 || (len[now] + 1 < len[c])) {
                    parent[c] = now;
                    len[c] = len[now] + 1;
                    tovisit.push(c);
                }
            }
            if ((tx == agx) && (ty == agy - 1) && (g[tx][ty - 1] == E || g[tx][ty - 1] == H)) {
                c = code(agx, agy - 1, tx, ty - 1);
                if (parent[c] == -1 || (len[now] + 1 < len[c])) {
                    parent[c] = now;
                    len[c] = len[now] + 1;
                    tovisit.push(c);
                }
            }
        }
        return null;
    }

    void removeFromV(Vector<Integer> V, int tx, int ty) {
        int cod = code(tx, ty, 0, 0);
        for (int i = 0; i < V.size(); i++) {
            if (V.elementAt(i) != null && V.elementAt(i) == cod) {
                V.set(i, null);
            }
        }
    }

    public void discover() {
        for (int i = -2; i < 3; i++) {
            for (int j = -2; j < 3; j++) {
                if (0 < x + i && x + i < N + 1 && 0 < y + j && y + j < N + 1) {
                    if (Grid[x + i][y + j] == U) {
                        Grid[x + i][y + j] = Table.Grid[x + i][y + j];
                        if (Grid[x + i][y + j] == T) {
                            Tiles.add(code(x + i, y + j, 0, 0));
                            newhole = true;
                        } else if (Grid[x + i][y + j] == H) {
                            Holes.add(code(x + i, y + j, 0, 0));
                            newhole = true;
                        }

                        AccesibleUnknown.removeElement(code(x + i, y + j, 0, 0));

                        drawOne(x + i, y + j);
                    } else if (Grid[x + i][y + j] != Table.Grid[x + i][y + j]) {
                        fooled = true;
                        if (Grid[x + i][y + j] == T) {
                            removeFromV(Tiles, x + i, y + j);
                        }
                        if (Grid[x + i][y + j] == H) {
                            removeFromV(Holes, x + i, y + j);
                        }
                        Grid[x + i][y + j] = Table.Grid[x + i][y + j];
                        if (Grid[x + i][y + j] == T) {
                            Tiles.add(code(x + i, y + j, 0, 0));
                            newhole = true;
                        } else if (Grid[x + i][y + j] == H) {
                            Holes.add(code(x + i, y + j, 0, 0));
                            newhole = true;
                        }
                        drawOne(x + i, y + j);
                    }
                    for (int k = 0; k < Table.PlayerNr; k++) {
                        if (k != AgentNr && Table.ag[k].x == x + i && Table.ag[k].y == y + j) {
                            if (OpponentX[k] != -1) {
                                drawEmpty(OpponentX[k], OpponentY[k]);
                            }
                            OpponentX[k] = x + i;
                            OpponentY[k] = y + j;
                            super.drawAg(Table.ag[k]);
                        }
                    }

                }
            }
        }
        for (int i = -3; i < 4; i++) {
            if (y - 3 > 0 && 0 < x + i && x + i < N + 1 && Grid[x + i][y - 3] == U &&
                    !AccesibleUnknown.contains(code(x + i, y - 3, 0, 0))) {
                AccesibleUnknown.add(code(x + i, y - 3, 0, 0));
            }
            if (y + 3 < N + 1 && 0 < x + i && x + i < N + 1 && Grid[x + i][y + 3] == U &&
                    !AccesibleUnknown.contains(code(x + i, y + 3, 0, 0))) {
                AccesibleUnknown.add(code(x + i, y + 3, 0, 0));
            }
            if (x - 3 > 0 && 0 < y + i && y + i < N + 1 && Grid[x - 3][y + i] == U &&
                    !AccesibleUnknown.contains(code(x - 3, y + i, 0, 0))) {
                AccesibleUnknown.add(code(x - 3, y + i, 0, 0));
            }
            if (x + 3 < N + 1 && 0 < y + i && y + i < N + 1 && Grid[x + 3][y + i] == U &&
                    !AccesibleUnknown.contains(code(x + 3, y + i, 0, 0))) {
                AccesibleUnknown.add(code(x + 3, y + i, 0, 0));
            }
        }
    }

    public void fill_hole_one(Stack<Integer> m) {
        boolean holeFilled = false;
        int newx, newy, newtx, newty;
        int dec[];

        dec = decode(m.pop());
        newx = dec[0] + 1;
        newy = dec[1] + 1;
        newtx = dec[2] + 1;
        newty = dec[3] + 1;

        if (newtx != movingTileX || newty != movingTileY) {
            for(int o = 0; o < SameSquare.size(); o++) {
                if (SameSquare.elementAt(o) != AgentNr && Points>0){
                    Table.ag[o].Points += Points;
                    Points = 0;
                }
            }
            Table.drawNew(newtx, newty, Color.LIGHT_GRAY);
            Table.drawEmpty(movingTileX, movingTileY);
            drawNew(newtx, newty, Color.LIGHT_GRAY);
            drawEmpty(movingTileX, movingTileY);
            if (Table.Grid[newtx][newty] == H) {
                holeFilled = true;
            }
            Table.Grid[movingTileX][movingTileY] = Grid[movingTileX][movingTileY] = E;
            Table.Grid[newtx][newty] = Grid[newtx][newty] = T;
        }
        Table.drawEmpty(x, y);
        drawEmpty(x, y);
        x = newx;
        y = newy;
        drawAg();
        movingTileX = newtx;
        movingTileY = newty;
        if (holeFilled) {
            vanish_hole(movingTileX, movingTileY);
        }


    }

    void vanish_hole(int tx, int ty) {
        Table.Grid[tx][ty] = Grid[tx][ty] = E;
        Table.HoleNr--;
        Table.TileNr--;
        Holes.removeElement(code(tx, ty, 0, 0));

        Table.TableHoles.removeElement(code(tx, ty, 0, 0));
        Table.drawEmpty(tx, ty);
        drawEmpty(tx, ty);
        Points += 100;
        Table.updatePoints(Points, AgentNr);
    }
    Stack<Integer> m = null;

    public int execute_one_move() {
        if (m != null && m.isEmpty()) {
            m = null;
        }
        int newcode, i;
        int[] dec = new int[4];
        discover();
        if (fooled) {
            if (moveType == 1 && m != null &&
                    Table.Grid[movingTileX][movingTileY] == T &&
                    !Tiles.contains(code(movingTileX, movingTileY, 0, 0))) {
                Tiles.add(code(movingTileX, movingTileY, 0, 0));
            }
            m = null;
            fooled = false;
        }
        if (m == null) {
            for (i = Tiles.size() - 1; i >= 0 && !Holes.isEmpty(); i--) {
                if (Tiles.elementAt(i) != null) {
                    dec = decode(Tiles.elementAt(i));
                    movingTileX = dec[0] + 1;
                    movingTileY = dec[1] + 1;
                    m = build(x, y, movingTileX, movingTileY);
                    if (m != null) {
                        moveType = 1;
                        Tiles.set(i, null);
                        break;
                    }
                }
            }
        }
        if (m != null) {
            if (moveType == 1) {
                fill_hole_one(m);
            } else {
                move(m);
            }
        } else {
            for (i = AccesibleUnknown.size() - 1; i >= 0; i--) {
                newcode = AccesibleUnknown.elementAt(i);
                dec = decode(newcode);
                m = go_stack(x, y, dec[0] + 1, dec[1] + 1);
                if (m != null) {
                    moveType = 2;
                    move(m);
                    break;
                }
            }
            if (m == null) {
                return 1;
            }
        }
        return 0;
    }

    public int move_ag() {
        Points -= 1;
        Table.updatePoints(Points, AgentNr);
        return execute_one_move();
    }
}