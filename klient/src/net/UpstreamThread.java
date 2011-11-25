package net;

import java.io.PrintWriter;
import java.net.Socket;

class UpstreamThread implements Runnable {
    private Socket socket;
    private PrintWriter upstream;
    private Thread thread;
    
    UpstreamThread() {
        thread = new Thread(this);
    }
    
    public void run() {}
    
    void start(Socket s) {
        socket = s;
        
        thread.start();
        
        try {
            if (socket != null && socket.isConnected()) {
                upstream = new PrintWriter(socket.getOutputStream());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
    
    void stop() {
        upstream = null;
        socket   = null;
        thread   = null;
    }
    
    void sendMessage(String message) {
        upstream.println(message);
        upstream.flush();
    }
}
