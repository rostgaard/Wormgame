package game;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.*;  // indeholder klasser der er specielle for Java2D
import java.awt.event.*;

import net.Connection;
import util.State;
import util.Direction;

/**
 * The main window in the application, that holds all other elements, panels and components.
 * 
 * @author Jesper Birkestrøm
 * @author Kim Rostgaard Christensen
 * @author Rasmus Sloth Jensen
 * @author Jesper Nyerup
 * @version 0.1
 */

public class ProgramWindow extends JFrame {
    private GameCanvas game;
    private Connection connection = null;
    
    private MenuBar menu;
    private StatusBar status;
    private JPanel cardPanel;
    private CardLayout cards;
    
    LoginPanel loginPanel;
    WelcomePanel welcomePanel;
    HighscorePanel highscorePanel;
    AboutPanel aboutPanel;
    PendingPanel pendingPanel;
    
    private KeyListener keyListener;
    
    String host;
    int port;
    boolean debug;
    
    /**
     * Constructor. Creates a new instance of this class, and sets up the window for the application.
     */
    
    public ProgramWindow(String h, int p, boolean d) {
        
        host = h;
        port = p;
        debug = d;
        
        menu      = new MenuBar(this);
        status    = new StatusBar(this);
        cardPanel = new JPanel();
        cards     = new CardLayout();
        cardPanel.setLayout(cards);
        
        loginPanel      = new LoginPanel(this);
        welcomePanel    = new WelcomePanel(this);
        highscorePanel  = new HighscorePanel(this);
        aboutPanel      = new AboutPanel(this);
        pendingPanel    = new PendingPanel(this);
        cardPanel.add(loginPanel, "login");
        cardPanel.add(welcomePanel, "welcome");
        cardPanel.add(highscorePanel, "highscore");
        cardPanel.add(aboutPanel, "about");
        cardPanel.add(pendingPanel, "pending");
        
        this.setIconImage(fetchImage("images/icon.png").getImage().getScaledInstance(20, 20, Image.SCALE_SMOOTH));
        
        keyListener = new KeyListener() {
            public void keyPressed(KeyEvent e) {
                if (connection.getState() == State.IN_GAME) {
                    switch (e.getKeyCode()) {
                        case KeyEvent.VK_UP:
                            game.directionPressed(Direction.NORTH);
                            break;
                        case KeyEvent.VK_RIGHT:
                            game.directionPressed(Direction.EAST);
                            break;
                        case KeyEvent.VK_DOWN:
                            game.directionPressed(Direction.SOUTH);
                            break;
                        case KeyEvent.VK_LEFT:
                            game.directionPressed(Direction.WEST);
                            break;
                    }
                }
            }
            public void keyReleased(KeyEvent e) {}
            public void keyTyped(KeyEvent e) {}
        };
        
        this.changeState(State.OFFLINE);


        this.setLayout(new BorderLayout());
        this.setJMenuBar(menu);
        this.add(cardPanel, BorderLayout.CENTER);
        this.add(status, BorderLayout.SOUTH);

        this.showLoginPanel();
        this.pack();
        this.centerWindow();

        this.setVisible(true);
        this.setResizable(false);
        this.setTitle("Ormespil");

        this.setDefaultCloseOperation(DO_NOTHING_ON_CLOSE);
        this.addWindowListener(new WindowListener() {
            public void windowDeactivated(WindowEvent evt) { }
            public void windowActivated(WindowEvent evt) { }
            public void windowDeiconified(WindowEvent evt) { }
            public void windowIconified(WindowEvent evt) { }
            public void windowClosed(WindowEvent evt) { }
            public void windowClosing(WindowEvent evt) {stopProgram(); }
            public void windowOpened(WindowEvent evt) { }
        });
        
    }
    
    /**
     * Method to instantiate an object of the class {@link net.Connection}. 
     * A connection is established to the default server on the default port, 
     * and a login is attempted with the specified username and password.
     * 
     * @param create the flag that defines whether or not the specified user 
     * information should be used to create a new user, or merely to log in.
     * @param user the desired username.
     * @param pass the desired password.
     * @see #connectToServer (String, boolean, String, String)
     * @see #connectToServer (String, int, boolean, String, String)
     */
    public void createOrLogin(boolean create, String user, String pass) {
        if (this.connection == null) this.connection = new Connection(this,host,port,debug);
        
        
        if (connection.getState() != State.OFFLINE) {
            if (create) {
                connection.createUser(user, pass);
            } else {
                connection.login(user, pass);
            }
        }
    }
    
    public void logout() {
        connection.logout();
        connection = null;
    }
    
    /**
     * Changes the state of the application. Often called from 
     * {@link net.Connection} in response to a message from the server.
     * 
     * @param s the new @link util.State.
     */
    public void changeState(State s) {
        status.changeState(s);
        menu.changeState(s);
        
        switch (s) {
            case OFFLINE:
                this.showLoginPanel();
                break;
                
            case NOT_LOGGED_IN:
                this.showLoginPanel();
                break;
                
            case LOGGED_IN:
                this.showWelcomePanel();
                break;
                
            case IN_QUEUE:
                this.showPendingPanel();
                break;
                
            case IN_GAME:
                this.showGame();
                break;
        }
    }
    
    public State inState() {
        if (connection == null) {
            return State.OFFLINE;
        }
        
        return connection.getState();
    }
    
    /**
     * Stops the program nicely. This is done by formally closing the connection
     * to the server, if any.
     */
    public void stopProgram() {
        this.setVisible(false);
        if (connection != null) connection.close();
        System.exit(0);
    }
    
    /**
     * The method that is invoked when the game ends. At this point only called
     * from {@link net.Connection}, when the appropriate message is received 
     * from the server.
     */
    public void gameEnded() {
        cardPanel.remove(game);
        game = null;
        this.removeKeyListener(keyListener);
        connection.requestHighscores();
    }
    
    /**
     * The method that shows highscores received from the server. This method is
     * called from {@link net.Connection}, when a set of highscores is received 
     * in response to a request from this application.
     * 
     * @param scores an array of Strings - first value is the control message
     * from the server, and the following are altering between a username and
     * and the corresponding score.
     */
    public void showHighscores(String[] scores) {
        sizeTo(highscorePanel);
        highscorePanel.updateScores(scores);
        cards.show(cardPanel, "highscore");
        //this.centerWindow();
    }
    
    public void showLoginPanel() {
        sizeTo(loginPanel);
        sizeTo(loginPanel);
        cards.show(cardPanel, "login");
        //this.centerWindow();
    }
    
    /**
     * Shows the default panel in this application window.
     */    
    public void showWelcomePanel() {
        sizeTo(welcomePanel);
        cards.show(cardPanel, "welcome");
        //this.centerWindow();
    }
    
    /**
     * About panel
     */
    public void showAboutPanel() {
        sizeTo(aboutPanel);
        cards.show(cardPanel, "about");
        //this.centerWindow();
    }
    
    /**
     * Pending panel
     */
    public void showPendingPanel() {
        sizeTo(pendingPanel);
        cards.show(cardPanel, "pending");
        //this.centerWindow();
    }
    
    public void showGame() {
        sizeTo(game);
        cards.show(cardPanel, "game");
        this.addKeyListener(keyListener);
        this.setFocusable(true);
        this.requestFocusInWindow();
        this.requestFocus();
        //this.centerWindow();
    }
    
    private void sizeTo(Component c) {
        cardPanel.setPreferredSize(c.getPreferredSize());
        cardPanel.setSize(c.getPreferredSize());
        this.pack();
    }
    
    /**
     * Shows an error dialog.
     * 
     * @param error the error to show.
     */
    public void showError(String error) {
        JOptionPane.showMessageDialog(this, error, "Der opstod en fejl", JOptionPane.ERROR_MESSAGE);
    }
    
    /**
     * This method sets up a new game in the application. It instantiates an
     * object of the class {@link GameCanvas}, and is mainly invoked by the
     * {@link net.Connection} when a new map is received from the server.
     * 
     * @param x the number of fields in horizontal direction.
     * @param y the number of fields in vertical direction.
     */
    public void newMap(int x, int y) {
        game = new GameCanvas(this, connection, x, y);
        cardPanel.add(game, "game");
    }
    
    /**
     * Returns the {@link net.Connection} object currently associated with this
     * window. May return <code>null</code>.
     *
     * @return Connection object.
     */
    public Connection getConnection() {
        return connection;
    }
    
    /**
     * Returns the {@link GameCanvas} object that figurates in this window, if 
     * any. May return <code>null</code>.
     * 
     * @return GameCanvas object.
     */
    public GameCanvas getGameCanvas() {
        return game;
    }
    
    /**
     * Returns the {@link MenuBar} of this window.
     * 
     * @return MenuBar object.
     */
    public MenuBar getMenu() {
        return menu;
    }
    
    /**
     * Returns the {@link StatusBar} of this window.
     * @return StatusBar object.
     */
    public StatusBar getStatusBar() {
        return status;
    }
    
    /**
     * Uses the default toolkit to center this window on the screen,
     * both horizontally and vertically.
     */
    private void centerWindow() {
        Toolkit tk = Toolkit.getDefaultToolkit();
        Dimension screen = tk.getScreenSize();
        this.setLocation((screen.width-this.getWidth())/2, (screen.height-this.getHeight())/2);
    }
    
    public ImageIcon fetchImage(String path) {
        java.net.URL imageurl = this.getClass().getResource("/"+path);
        if (imageurl != null) {
            return new ImageIcon(imageurl);
        } else {
            System.out.println("Image resource "+path+" not found");
            return null;
        }
    }
}