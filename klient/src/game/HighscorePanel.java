package game;

import javax.swing.*;
import java.awt.event.*;
import java.awt.*;

/**
 * The panel that shows the highscores when these are received from the server.
 * 
 * @author Jesper Birkestrøm
 * @author Kim Rostgaard Christensen
 * @author Rasmus Sloth Jensen
 * @author Jesper Nyerup
 * @version 0.1
 */
public class HighscorePanel extends JPanel {
    private JPanel heading, scores, buttons;
    private JLabel headingLabel;
    private JButton okButton;
    
    private ProgramWindow window;
    
    /**
     * Constructor. Takes a reference to the instantiating 
     * {@link ProgramWindow} and an array of Strings as arguments.
     * 
     * @param w the ProgramWindow.
     * @param highscores the array of Strings that contain the highscores.
     */
    public HighscorePanel(ProgramWindow w) {
        window = w;
        
        this.setPreferredSize(new Dimension(450,300));
        
        heading = new JPanel();
        headingLabel = new JLabel("Highscores");
        headingLabel.setFont(new Font("Lucida", 0, 25));
        heading.add(headingLabel);
                
        buttons = new JPanel();
        okButton = new JButton("OK");
        okButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                window.showWelcomePanel();
            }
        });
        
        buttons.add(okButton);
        
        
        this.setLayout(new BorderLayout());
        this.add(new JPanel(), BorderLayout.WEST);
        this.add(heading, BorderLayout.NORTH);
        this.add(buttons, BorderLayout.SOUTH);
    }
    
    public void updateScores(String[] highscores) {
        scores = new JPanel();
        scores.setLayout(new GridLayout(10,3));
        int j = 1;
        for (int i = 1; i < highscores.length; i++) {
            scores.add(new JLabel(Integer.toString(j) + ".  " + highscores[i]));
            i++; j++;
            scores.add(new JLabel(highscores[i]));
        }
        
        this.add(scores, BorderLayout.CENTER);
    }
}
