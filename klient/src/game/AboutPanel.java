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
public class AboutPanel extends JPanel {
    private ProgramWindow window;
    
    /**
     *
     */
    public AboutPanel(ProgramWindow w) {
        String text = "Hej hej\nmed dig!";
        this.window = w;
        this.setPreferredSize(new Dimension(480,250));
        
        JPanel heading = new JPanel();
        JLabel headingLabel = new JLabel("Om Ormespil");
        headingLabel.setFont(new Font("Lucida", 0, 25));
        heading.add(headingLabel);
        
        JPanel content = new JPanel(new FlowLayout(FlowLayout.CENTER));
        content.add(new JLabel(text));
        
        JPanel buttons = new JPanel();
        JButton okButton = new JButton("OK");
        okButton.requestFocus();
        okButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent evt) {
                switch (window.inState()) {
                    case OFFLINE:
                        window.showLoginPanel();
                        break;

                    case NOT_LOGGED_IN:
                        window.showLoginPanel();
                        break;

                    case LOGGED_IN:
                        window.showWelcomePanel();
                        break;
                }
            }
        });
        buttons.add(okButton);
        
        
        this.setLayout(new BorderLayout());
        this.add(new JPanel(), BorderLayout.WEST);
        this.add(heading, BorderLayout.NORTH);
        this.add(content, BorderLayout.CENTER);
        this.add(buttons, BorderLayout.SOUTH);
    }
}
