package net;

import java.net.Socket;

import game.ProgramWindow;
import game.GameCanvas;
import util.Position;
import util.State;

/**
 * The class that manages a connection to a server. It uses a
 * {@link DownstreamThread} and an {@link UpstreamThread} to manage the
 * exchange of data with the server.
 * 
 * @author Jesper Birestrøm
 * @author Kim Rostgaard Christensen
 * @author Rasmus Sloth Jensen
 * @author Jesper Nyerup
 * @version 0.1
 */

public class Connection {
    private DownstreamThread downstream;
    private UpstreamThread upstream;
    private ProgramWindow window;
    private GameCanvas game;
    
    private Socket socket;

    private boolean debug = false;
    private boolean connected = false;
    
    private final String DEFAULT_HOST = "localhost";
    private final int DEFAULT_PORT = 22144;
    
    private String host = DEFAULT_HOST;
    private int port = DEFAULT_PORT;
    
    private State state = State.OFFLINE;
    
    /**
     * Constructor, that connects to the default host on the default port. Takes
     * a reference to a ProgramWindow object as an argument.
     * @param w ProgramWindow object.
     * @see #Connection (ProgramWindow, String)
     * @see #Connection (ProgramWindow, String, int)
     */
    public Connection(ProgramWindow w) {
        window = w;
        initiateConnection();
    }
    
    /**
     * Constructor, that connects to the specified host on the default port.
     * Takes a reference to a ProgramWindow object and a hostname as arguments.
     * 
     * @param w ProgramWindow object.
     * @param host the hostname of the server.
     * @see #Connection (ProgramWindow)
     * @see #Connection (ProgramWindow, String, int)
     */
    public Connection(ProgramWindow w, String host) {
        window = w;
        this.host = host;
        initiateConnection();
    }
    
    /**
     * Constructor, that connects to the specified host on the specified port.
     * Takes a reference to a ProgramWindow object, at hostname and a port
     * number as arguments.
     * @param w ProgramWindow object.
     * @param host the hostname of the server.
     * @param port the port to connect through.
     * @see #Connection (ProgramWindow)
     * @see #Connection (ProgramWindow, String)
     */
    public Connection(ProgramWindow w, String host, int port, boolean debug) {
        window = w;
        this.host = host;
        this.port = port;
        this.debug = debug;
        initiateConnection();
    }
    
    /**
     * Attempts to create a user account on the server.
     * @param user the desired username.
     * @param pass the desired password.
     */
    public void createUser(String user, String pass) {
        upstream.sendMessage("C," + user + "," + pass);
    }
    
    /**
     * Attempts to login to the server with the specified user informations.
     * @param user the username.
     * @param pass the password.
     */
    public void login(String user, String pass) {
        upstream.sendMessage("U," + user + "," + pass);
    }
    
    /**
     * Sends logout command to server.
     */
    public void logout() {
        upstream.sendMessage("Q");
        this.window.changeState(State.OFFLINE);
        close();
    }
    
    /**
     * Attempts to join a new game on the server.
     */
    public void joinGame() {
        upstream.sendMessage("J");
        changeState(State.IN_QUEUE);
    }
    
    /**
     * Instructs the worm to turn left.
     */
    public void turnLeft() {
        upstream.sendMessage("L");
    }
    
    /**
     * Instructs te worm to turn right.
     */
    public void turnRight() {
        upstream.sendMessage("R");
    }
    
    /**
     * Requests highscores from the server.
     */
    public void requestHighscores() {
        upstream.sendMessage("H");
    }
    
    /**
     * Sends a custom command message to the server. For test purposes only.
     * @param message
     */
    public void sendFreestyleMessage(String message) {
        upstream.sendMessage(message);
        if (debug) System.out.println("Data sendt: " + message);
    }
    
    /**
     * Processes a line of data received from the server. The line is chopped up
     * in bits at every ',' (comma) in the data line, parsed according to the
     * protocol, and finally distributed to an appropriate part of the
     * application.
     * @param data the line of data received from the server.
     */
    void processData(String data) {
        //System.out.println("Fokus: " + window.getFocusOwner().getName());
        
        String[] dataBits = data.split(",");
        char command = data.charAt(0);
        
        switch (command) {
            case 'A':
                if (state == State.NOT_LOGGED_IN) {
                    changeState(State.LOGGED_IN);
                    if (debug) System.out.println("Du er nu logget ind.");
                } else {
                    throwSyncError("Login accepteret.");
                }
                break;
            case 'T':
                if (state == State.IN_QUEUE) {
                    int x = Integer.valueOf(dataBits[1].trim());
                    int y = Integer.valueOf(dataBits[2].trim());
                    window.newMap(x, y);

                    if (debug) System.out.println("Du har modtaget en tom bane, og står nu i kø til spillet.");
                } else {
                    throwSyncError("Bane modtaget.");
                }
                break;
            case 'B':
                if (state == State.IN_QUEUE || state == State.IN_GAME) {
                    for (int i = 1; i < dataBits.length; i++) {
                        int x = Integer.valueOf(dataBits[i].trim()); i++;
                        int y = Integer.valueOf(dataBits[i].trim());
                        game.drawWall(new Position(x, y));
                        if (debug) System.out.println("Serveren har sat en væg på banen: " + x + ", " + y);
                    }
                } else {
                    throwSyncError("Væg modtaget.");
                }
                break;
            case 'F':
                if (state == State.IN_QUEUE || state == State.IN_GAME) {
                    for (int i = 1; i < dataBits.length; i++) {
                        int x = Integer.valueOf(dataBits[i].trim()); i++;
                        int y = Integer.valueOf(dataBits[i].trim());
                        game.drawCake(new Position(x, y));
                        if (debug) System.out.println("Serveren har lagt en kage på banen: " + x + ", " + y);
                    }
                } else {
                    throwSyncError("Kage modtaget.");
                }
                break;
            case 'E':
                if (state == State.IN_QUEUE || state == State.IN_GAME) {
                    for (int i = 1; i < dataBits.length; i++) {
                        int x = Integer.valueOf(dataBits[i].trim()); i++;
                        int y = Integer.valueOf(dataBits[i].trim());
                        game.erase(new Position(x, y));
                        if (debug) System.out.println("Serveren har slettet et felt på banen: " + x + ", " + y);
                    }
                } else {
                    throwSyncError("Felt slettet.");
                }
                break;
            case 'N':
                if (state == State.IN_QUEUE) {
                    changeState(State.IN_GAME);
                    game.tagOwnWorm(dataBits[1]);
                    if (debug) System.out.println("Spillet er startet.");
                } else {
                    throwSyncError("Spil startet.");
                }
                break;
            case 'S':
                if (state == State.IN_QUEUE || state == State.IN_GAME) {
                    game.updateWorm(dataBits[1], new Position(Integer.valueOf(dataBits[2].trim()), Integer.valueOf(dataBits[3].trim())), Integer.valueOf(dataBits[4].trim()));
                    if (debug) System.out.println("En orm blev opdateret.");
                } else {
                    throwSyncError("Orm opdateret.");
                }
                break;
            case 'D':
                if (state == State.IN_GAME) {
                    game.removeWorm(dataBits[1]);
                    if (debug) System.out.println("En slange er død.");
                } else {
                    throwSyncError("Slange død.");
                }
                break;
            case 'G':
                if (state == State.IN_GAME) {
                    window.gameEnded();
                    if (debug) System.out.println("Spillet er slut.");
                } else {
                    throwSyncError("Spil slut.");
                }
                break;
            case 'P':
                window.showHighscores(dataBits);
                if (debug) System.out.println("Highscoreliste modtaget.");
                break;
            case 'M':
                if (debug) {
                    System.out.print("Besked modtaget: ");
                    for (int i = 1; i < dataBits.length; i++) {
                        if (i > 1) System.out.print(",");
                        System.out.print(dataBits[i]);
                    }
                    System.out.println();
                }
                break;
            default:
                if (debug) System.err.print("Unimplemented message received: ");
                for (int i = 0; i < dataBits.length; i++) {
                    if (i > 0) if (debug) System.err.print(",");
                    if (debug) System.err.print(dataBits[i]);
                }
        }
    }
    
    /**
     * Tags this Connection with a reference to a {@link game.GameCanvas} 
     * object. Thus, making the Connection aware of a game taking place in the
     * application.
     * @param g GameCanvas object.
     */
    public void tag(GameCanvas g) {
        game = g;
    }
    
    /**
     * Returns the current state of this Connection.
     * @return current {@link util.State}.
     */
    public State getState() {
        return state;
    }
    
    /**
     * Attempts to close this connection.
     */
    public void close() {
        if (    this.state == State.LOGGED_IN ||
                this.state == State.IN_QUEUE ||
                this.state == State.IN_GAME ) {
            upstream.sendMessage("Q");
        }
        
        if (upstream != null) {
            upstream.stop();
            upstream = null;
        }
        if (downstream != null) {
            downstream.stop();
            downstream = null;
        }
        this.changeState(State.OFFLINE);
    }
    
    /**
     * Performing actions common for the three constructors.
     * @see #Connection(ProgramWindow)
     * @see #Connection(ProgramWindow, String)
     * @see #Connection(ProgramWindow, String, int)
     */
    private void initiateConnection() {
        window.changeState(State.OFFLINE);
        
        try {
            socket = new Socket(host, port);
            downstream = new DownstreamThread(this);
            upstream = new UpstreamThread();
        
            upstream.start(socket);
            downstream.start(socket);
        
            changeState(State.NOT_LOGGED_IN);
        } catch (Exception e) {
            window.showError(e.getMessage());
        }
    }
    
    /**
     * Changes the state of this Connection. This is a private function, as it
     * is only called in response to certain Connection actions and commands
     * received from the server.
     * @param s the new {@link util.State}.
     */
    private void changeState(State s) {
        state = s;
        window.changeState(s);
    }
    
    /**
     * Prints a sync error to stderr. A sync error occurs when a message is
     * received from the server that is not logically possible in the current
     * state.
     * @param cause the error message to print.
     */
    private void throwSyncError(String cause) {
        if (debug) System.err.println("Sync error: " + cause);
    }
    
    /**
     * Prints a data error to stderr. A data error occurs when a message is
     * received from the server that is not valid according to the protocol.
     * @param data the unparsed data.
     */
    private void throwDataError(String data) {
        if (debug) System.err.println("Data error: " + data);
    }
}
