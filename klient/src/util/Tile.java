package util;

import java.awt.Color;

public class Tile {
    private TileType type;
    private int xPos;
    private int yPos;
    private Color col;
    
    public Tile(TileType t, int x, int y, Color c) {
        type = t;
        xPos = x;
        yPos = y;
        col  = c;
    }
    
    public Tile(TileType t, Color c) {
        type = t;
        col  = c;
    }
    
    public TileType getType() {
        return type;
    }
    
    public int getX() {
        return xPos;
    }
    
    public int getY() {
        return yPos;
    }
    
    public Color getColor() {
        return col;
    }
}
