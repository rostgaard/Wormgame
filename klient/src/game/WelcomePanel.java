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
public class WelcomePanel extends JPanel {
    private ProgramWindow window;
    
    /**
     *
     */
    public WelcomePanel(ProgramWindow w) {
        window = w;
        String text = "alalallalalalalal!";
        this.setPreferredSize(new Dimension(300,400));
        
        JPanel heading = new JPanel();
        JLabel headingLabel = new JLabel("Ormespil");
        headingLabel.setFont(new Font("Lucida", 0, 25));
        heading.add(headingLabel);
        
        JPanel content = new JPanel(new FlowLayout(FlowLayout.CENTER));
        content.add(new JLabel(text));
        
        JPanel buttons = new JPanel(new FlowLayout(FlowLayout.CENTER));
        
        JButton joinButton = new JButton("Join");
        joinButton.requestFocus();
        joinButton.setIcon(new ImageIcon("game.png"));
        joinButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                window.getConnection().joinGame();
            }
        });
        buttons.add(joinButton);
        
        JButton highscoreButton = new JButton("Highscore");
        highscoreButton.setIcon(new ImageIcon("highscore.png"));
        highscoreButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                window.getConnection().requestHighscores();
            }
        });
        buttons.add(highscoreButton);
        
        this.setLayout(new BorderLayout());
        this.add(new JPanel(), BorderLayout.WEST);
        this.add(heading, BorderLayout.NORTH);
        this.add(content, BorderLayout.CENTER);
        this.add(buttons, BorderLayout.SOUTH);
    }
}
