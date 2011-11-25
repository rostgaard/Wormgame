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
public class PendingPanel extends JPanel {
    ProgramWindow window;
    
    /**
     *
     */
    public PendingPanel(ProgramWindow p) {
        window = p;
        this.setPreferredSize(new Dimension(480,250));
        
        JLabel message = new JLabel("Venter på andre spillere....");
        message.setIcon(window.fetchImage("images/pending.gif"));
        
        this.setLayout( new FlowLayout(FlowLayout.CENTER));
        add(message);
        
    }
}
