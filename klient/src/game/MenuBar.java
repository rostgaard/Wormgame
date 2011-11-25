package game;

import javax.swing.*;
import java.awt.event.*;

import util.State;

/**
 * The menu bar in the {@link ProgramWindow}.
 * 
 * @author Jesper Birkestrøm
 * @author Kim Rostgaard Christensen
 * @author Rasmus Sloth Jensen
 * @author Jesper Nyerup
 * @version 0.1
 */

public class MenuBar extends JMenuBar implements ActionListener {
    private JMenuItem logout        = createMenuItem("Log ud", "Klik hér for at logge ud", 'U', "logout" );
    private JMenuItem highscore     = createMenuItem("Highscore", "Se highscores", 'H', "highscore" );
    private JMenuItem join          = createMenuItem("Join Game", "Join game", 'J', "join" );
    private JMenuItem close         = createMenuItem("Afslut", "Klik her for at afslutte spillet", 'A', "close" );
    private JMenuItem about         = createMenuItem("Om", "Læs noget her..", 'O', "about" );
    
    private ProgramWindow window;

    /**
     * Constructor. Sets up the menu bar, and creates a reference to the
     * associated {@link ProgramWindow}.
     * @param w associated {@link ProgramWindow}.
     */
    public MenuBar(ProgramWindow w) {
        this.add(FileMenu());
        this.add(helpMenu());
        
        window = w;
        
        logout.setIcon(window.fetchImage("images/key.png"));
        highscore.setIcon(window.fetchImage("images/highscore.png"));
        join.setIcon(window.fetchImage("images/game.png"));
        close.setIcon(window.fetchImage("images/close.png"));
        about.setIcon(window.fetchImage("images/help.png"));
    }

    /**
     * Private method that sets up the "File" menu in the menu bar.
     * 
     * @return the "file" menu.
     */
    private JMenu FileMenu(){
        JMenu menu = new JMenu("Filer");
        menu.getPopupMenu().setLightWeightPopupEnabled(false);
        menu.setMnemonic( 'F' );
        
        menu.add(logout);
        menu.addSeparator();
        menu.add(highscore);
        menu.add(join);
        menu.addSeparator();
        menu.add(close);

        return menu;
    }

    /**
     * Private method that sets up the "Help" menu in the menu bar.
     * 
     * @return the "help" menu.
     */
    private JMenu helpMenu(){
        JMenu menu = new JMenu("Hjælp");
        menu.getPopupMenu().setLightWeightPopupEnabled(false);
        menu.setMnemonic( 'H' );

        menu.add(about);
        
        return menu;
    }
    
    public void changeState(State s) {
        switch (s) {
            case OFFLINE:
            case NOT_LOGGED_IN:
                logout.setEnabled(false);
                highscore.setEnabled(false);
                join.setEnabled(false);
                about.setEnabled(true);
                break;
                
            case LOGGED_IN:
                logout.setEnabled(true);
                highscore.setEnabled(true);
                join.setEnabled(true);
                about.setEnabled(true);
                break;
                
            case IN_QUEUE:
            case IN_GAME:
                logout.setEnabled(true);
                highscore.setEnabled(false);
                join.setEnabled(false);
                about.setEnabled(false);
                break;
        }
    }
    
    /**
     * Invoked when e menu item is chosen. Implemented from 
     * {@link java.awt.event.ActionListener}.
     * @param a the ActionEvent caught by the 
     * {@link java.awt.event.ActionListener}.
     */
    public void actionPerformed(ActionEvent a){
        if (a.getActionCommand().equals("logout")) {
            window.logout();
        }
        if (a.getActionCommand().equals("close")) {
            window.stopProgram();
        }
        if (a.getActionCommand().equals("highscore")) {
           window.getConnection().requestHighscores();
           //window.showHighscores(scores);
        }
        if (a.getActionCommand().equals("about")) {
            window.showAboutPanel();
        }
        if (a.getActionCommand().equals("join")) {
            window.getConnection().joinGame();
            
        }
    }

    /**
     * This method is a shorthand way of creating an item for a menu, and takes
     * a bunch of parameters to customize the returned item.
     * 
     * @param name the name of the menu item.
     * @param toolTip the tooltip for the menu item.
     * @param image the ImageIcon for the menu item.
     * @param key the keyboard shortcut in the menu.
     * @param actionCommand the actionCommand the item sends.
     * @return JMenuItem object.
     */
    public JMenuItem createMenuItem(String name,
                                    String toolTip,
                                    int key,
                                    String actionCommand) 
    {
        JMenuItem menuItem = new JMenuItem();
        menuItem.setText( name );
        if( key > 0 ) menuItem.setMnemonic( key );
        if( toolTip != null ) menuItem.setToolTipText( toolTip );
        menuItem.addActionListener( this );
        if( actionCommand != null) menuItem.setActionCommand(actionCommand);
        return menuItem;
    }
}
