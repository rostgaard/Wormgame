package game;

public class Main {
    
    private ProgramWindow window;
    
    private String  host    = "localhost";
    private int     port    = 22144;
    private boolean debug   = true;
    
    public Main(String[] arguments) {
        window = new ProgramWindow(host,port,debug);
    }

    public static void main(String[] args) {
        new Main(args);
    }
}
