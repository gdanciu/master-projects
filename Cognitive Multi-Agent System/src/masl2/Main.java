package masl2;

import java.awt.Color;
import java.awt.Container;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

interface Consts {

    static final int W = 10,  O = 1,  T = 2,  E = 0,  H = 3,  U = 4, //              wall    obstacle  tile    empty  hole   unknown
             ScreenSize = 250,  Player1 = 5,  Player2 = 6,  Player3 = 7,  Player4 = 8;
}

class Main {

    public static void main(String[] args) {
        CogAgFrame frame = new CogAgFrame();
        GridLayout l = new GridLayout(2, 3);
        frame.setLayout(l);
        frame.setVisible(true);
        frame.getGraphics().dispose();
    }
}

class CogAgFrame extends JFrame implements Consts {

    JTextField NText = new JTextField(3), HolesText = new JTextField(3),
            TilesText = new JTextField(3), ObstText = new JTextField(3),
            HoleReplaceFrequencyText = new JTextField(3),
            SleepTimeText = new JTextField(3);
    JLabel NLabel = new JLabel("Dimension"), HolesLabel = new JLabel("Holes"),
            TilesLabel = new JLabel("Tiles"), ObstLabel = new JLabel("Obstacles"),
            HoleReplaceFrequencyLabel = new JLabel("HoleReplaceFrequency"),
            ErrorLabel = new JLabel(""), SleepTimeLabel = new JLabel("Sleep Time");
    JLabel[] PlayerNamePoints, PointsLabel;
    private JPanel PlayingTablePanel;
    private JPanel[] agTablePanel;
    PlayingTable Table;
    Agent[] ag;
    CogAgFrame thisFrame = this;
    int PlayerNr = 4;

    public CogAgFrame() {

        setSize((int) (2.5 * ScreenSize), (int) (2.5 * ScreenSize));
        setTitle("Cognitive Agent");
        NText.setText("15");
        HolesText.setText("15");
        TilesText.setText("15");
        ObstText.setText("15");
        HoleReplaceFrequencyText.setText("10");
        SleepTimeText.setText("200");
        addWindowListener(new WindowAdapter() {

            public void windowClosing(WindowEvent e) {
                System.exit(0);
            }
        });

        Container contentPane = getContentPane();


        JPanel p = new JPanel();
        addButton(p, "Start", new ActionListener() {

            public void actionPerformed(ActionEvent evt) {
                Thread th;
                // if (ag == null) {

                ag[0].moveAg = true;
                ag[1].moveAg = true;
                th = new Thread(Table);
                th.start();

            //       th1.start();
            // }
            // else {
            //    if

            //  ag.moveAg = !ag.moveAg;
            //  }
            }
        });


        addButton(p, "Close", new ActionListener() {

            public void actionPerformed(ActionEvent evt) {
                PlayingTablePanel.setVisible(false);
                System.exit(0);
            }
        });

        addButton(p, "Generate", new ActionListener() {

            public void actionPerformed(ActionEvent evt) {
                Table = new PlayingTable(PlayingTablePanel, thisFrame);
                Table.generate(Integer.parseInt(NText.getText()),
                        Integer.parseInt(ObstText.getText()),
                        Integer.parseInt(HolesText.getText()),
                        Integer.parseInt(TilesText.getText()),
                        Integer.parseInt(HoleReplaceFrequencyText.getText()));
                ag = new Agent[PlayerNr];
                Table.ag = ag;
                for (int i = 0; i < PlayerNr; i++) {
                    ag[i] = new Agent(Table, agTablePanel[i], i);
                }
                ag[0].col = Color.CYAN;
                ag[1].col = Color.GREEN;
                ag[2].col = Color.ORANGE;
                ag[3].col = Color.MAGENTA;
                ag[0].setAgent(1, 1);
                ag[1].setAgent(Integer.parseInt(NText.getText()),
                        Integer.parseInt(NText.getText()));
                ag[2].setAgent(Integer.parseInt(NText.getText()), 1);
                ag[3].setAgent(1, Integer.parseInt(NText.getText()));
            }
        });


        p.add(ErrorLabel);
        p.add(NLabel);
        p.add(NText);
        p.add(HolesLabel);
        p.add(HolesText);
        p.add(TilesLabel);
        p.add(TilesText);
        p.add(ObstLabel);
        p.add(ObstText);
        p.add(SleepTimeLabel);
        p.add(SleepTimeText);
        addButton(p, "Update Speed", new ActionListener() {

            public void actionPerformed(ActionEvent evt) {
                Table.sleepTime = Integer.parseInt(SleepTimeText.getText());

            }
        });
        p.add(HoleReplaceFrequencyLabel);
        p.add(HoleReplaceFrequencyText);
        PlayingTablePanel = new JPanel();
        contentPane.add(PlayingTablePanel);

        agTablePanel = new JPanel[PlayerNr];
        for (int i = 0; i < PlayerNr; i++) {
            agTablePanel[i] = new JPanel();
            contentPane.add(agTablePanel[i]);
        }
        PlayerNamePoints = new JLabel[PlayerNr];
        PointsLabel = new JLabel[PlayerNr];
        JPanel newP;
        for (int i = 0; i < PlayerNr; i++) {
            newP = new JPanel();
            PlayerNamePoints[i] = new JLabel("Points" + i);
            PointsLabel[i] = new JLabel("0");
            newP.add(PlayerNamePoints[i]);
            newP.add(PointsLabel[i]);
            p.add(newP, "South");
        }
        contentPane.add(p, "North");
    }

    public void updatePoints(int points, int nr) {
        PointsLabel[nr].setText(Integer.toString(points));

    }

    public void addButton(Container c, String title, ActionListener a) {
        JButton b = new JButton(title);
        c.add(b);
        b.addActionListener(a);
    }
}

