package game;

import javax.swing.*;
import javax.swing.border.*;
import java.awt.*;

import util.State;

public class StatusBar extends JPanel {
    private ProgramWindow window;
    private JLabel status;
    
    public StatusBar(ProgramWindow w) {
        window = w;
        
        status = new JLabel(" ");
        status.setFont(new Font("Lucida", 0, 10));
        
        this.setBorder(BorderFactory.createBevelBorder(BevelBorder.LOWERED));
        this.setLayout(new BorderLayout());
        this.add(status, BorderLayout.EAST);
    }
    
    public void changeState(State state) {
        switch (state) {
            case OFFLINE:
                status.setText("Offline");
                status.setIcon(window.fetchImage("images/server_offline.png"));
                break;
                
            case NOT_LOGGED_IN:
                status.setText("Not logged in");
                status.setIcon(window.fetchImage("images/server_pending.png"));
                break;
                
            case LOGGED_IN:
                status.setText("Logged in");
                status.setIcon(window.fetchImage("images/server_linked.png"));
                break;
                
            case IN_QUEUE:
                status.setText("Waitin for game to start");
                status.setIcon(window.fetchImage("images/server_queue.png"));
                break;
                
            case IN_GAME:
                status.setText("Playing");
                status.setIcon(window.fetchImage("images/game.png"));
                break;
        }
    }
}
