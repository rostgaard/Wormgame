package util;

import java.util.LinkedList;
import java.awt.Color;

public class Worm {
    private LinkedList<Position> worm = new LinkedList();
    private Color color;
    private boolean ownWorm = false;
    
    public Worm(Color c) {
        this.color = c;
    }
    
    public void setHead(Position head) {
        worm.add(head);
    }
    
    public Position getTail() {
        return worm.poll();
    }
    
    public Color getColor() {
        return color;
    }
    
    public void setColor(Color c) {
        color = c;
    }
    
    public int getLength() {
        return worm.size();
    }
    
    public LinkedList<Position> getPositions() {
        return worm;
    }
    
    public void claimWorm() {
        ownWorm = true;
    }
    
    public boolean isOwnWorm() {
        return ownWorm;
    }
}
