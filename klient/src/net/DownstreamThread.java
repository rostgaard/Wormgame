package net;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.Socket;
import java.net.SocketTimeoutException;

/**
 * The thread that handles the data received from the server. This class is 
 * package friendly, and is only instantiated by a {@link Connection} object, 
 * through witch all contact with the rest of the application is handled.
 * 
 * @author Jesper Birkestrøm
 * @author Kim Rostgaard Christensen
 * @author Rasmus Sloth Jensen
 * @author Jesper Nyerup
 * @version 0.1
 */
class DownstreamThread implements Runnable {
    private Socket socket;
    private BufferedReader downstream;
    private Thread thread;
    
    private Connection con;
    
    private String input;
    private int i = 0;
    
    private boolean running = false;
    
    /**
     * Constructor. Takes a reference to the {@link Connection} object that owns
     * this thread as an argument.
     * @param c a {@link Connection} object.
     */
    DownstreamThread(Connection c) {
        con = c;
        thread = new Thread(this);
    }
    
    /**
     * Implemented from {@link java.lang.Runnable}. The actual sequence of this
     * thread.
     */
    public void run() {
        try {
            if (socket != null && socket.isConnected()) {
                downstream = new BufferedReader(new InputStreamReader(socket.getInputStream()));

                input = "";
                input = downstream.readLine();
                while (input != null && running) {
                    con.processData(input);
                    input = downstream.readLine();
                }

            }
        } catch (SocketTimeoutException e) {
            System.out.println("Modtog forbindelsestimeout.");
        } catch (Exception e) {
            e.printStackTrace();
        }
        con.close();
    }
    
    /**
     * Starts the thread. Takes the socket to use for the connection as an
     * argument.
     * 
     * @param s the Socket.
     */
    void start(Socket s) {
        socket = s;
        
        running = true;
        thread.start();
    }
    
    /**
     * Attempts to stop the thread. Should be accompanied by removal of the
     * reference to this thread in the instantiating object, due to the nature
     * of thread handling i Sun Java Development Kit.
     */
    void stop() {
        running = false;
    }
}