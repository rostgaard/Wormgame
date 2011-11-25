package game;

import javax.swing.*;
import java.awt.*;
import java.awt.geom.*;
import java.awt.image.*;
import java.awt.event.*;
import java.util.*;

import net.Connection;
import util.Position;
import util.Direction;
import util.TileType;
import util.Tile;
import util.Worm;

public class GameCanvas extends JComponent {
    private Connection connection;
    private ProgramWindow window;
    
    private HashMap<String,Worm> worms = new HashMap();
    private Tile[][] map;

    private Position latestPosition;
    private Direction direction;
    
    private int fieldSize, mapSizeX, mapSizeY;
    
    /**
     * Constructor. Sets up the game and associates this object with relevant
     * parts of the application.
     * 
     * @param p the window that holds the game.
     * @param c the connection to the server.
     * @param x the number of fields on the map, horizontally.
     * @param y the number of fields on the map, vertically.
     */
    public GameCanvas(ProgramWindow p, Connection c, int x, int y) {
        window = p;
        connection = c;
        
        mapSizeX = x;
        mapSizeY = y;
        
        // Instantierer banens array
        map = new Tile[x+1][y+1];
        
        // Fylder arrayet med "tomme" felter
        for (int i = 1; i <= mapSizeX; i++) {
            for (int j = 1; j <= mapSizeY; j++) {
                map[i][j] = new Tile(TileType.AIR, Color.BLACK);
            }
        }

        // Sætter felt- (og dermed bane-)størrelsen til noget fornuftigt
        int largestAxis = 0;
        if (x >= y) largestAxis = x;
        else largestAxis = y;
        int tempFieldSize = 0;
        while ((tempFieldSize * largestAxis) <= 580) {
            tempFieldSize++;
        }
        fieldSize = tempFieldSize;

        // Sætter nogle foretrukne vinduesindstillinger
        this.setPreferredSize(new Dimension(fieldSize*mapSizeX, fieldSize*mapSizeY));
        this.repaint();

        
        // Sørger for at netværksforbindelsen kender til dette spil
        connection.tag(this);
    }
    
    /**
     * paints the latest snapshot of the game on the screen. Overrides paint()
     * method in {@link javax.swing.JComponent}.
     * @param g
     */
    @Override
    public void paint(Graphics g) {
        Graphics2D graphics = (Graphics2D)g;
        
        if (map != null) {
            for (int i = 1; i <= mapSizeX; i++) {
                for (int j = 1; j <= mapSizeY; j++) {
                    Tile tile = map[i][j];
                    
                    Position pos = parseCoordinates(i, j);
                    switch (tile.getType()) {
                        case WORM:
                            graphics.setColor(tile.getColor());
                            graphics.fill3DRect(pos.getX(), pos.getY(), fieldSize, fieldSize, true);
                            break;
                        case WALL:
                            graphics.setColor(tile.getColor());
                            graphics.fillRect(pos.getX(), pos.getY(), fieldSize, fieldSize);
                            break;
                        case CAKE:
                            graphics.setColor(Color.BLACK);
                            graphics.fillRect(pos.getX(), pos.getY(), fieldSize, fieldSize);
                            graphics.setColor(tile.getColor());
                            graphics.fillOval(pos.getX(), pos.getY(), fieldSize, fieldSize);
                            break;
                        case AIR:
                            graphics.setColor(tile.getColor());
                            graphics.fillRect(pos.getX(), pos.getY(), fieldSize, fieldSize);
                            break;
                    }
                }
            }
        }
    }
    
    /**
     * Marks a specified worm in the {@link #worms} array as the {@link Worm}
     * controlled by the player.
     * @param id the <code>id</code> of the worm.
     */
    public void tagOwnWorm(String id) {
        if (!worms.containsKey(id)) worms.put(id, new Worm(Color.GREEN));
        else worms.get(id).setColor(Color.GREEN);
        
        worms.get(id).claimWorm();
    }
    
    /**
     * Moves a {@link Worm} on the map, and, if necessary, removes its tail.
     * Usually called in response to a command from the server.
     * @param id the <code>id</code> of the {@link Worm}.
     * @param pos the new {@link Position} of the {@link Worm}s head.
     * @param length the length of the {@link Worm}.
     */
    public void updateWorm(String id, Position pos, int length) {
        if (!worms.containsKey(id)) {
            worms.put(id, new Worm(Color.RED));
        }
        
        Worm worm = worms.get(id);
        
        worm.setHead(pos);
        drawWormBit(pos, worm.getColor());
        if (worm.getLength() > length) {
            erase(worm.getTail());
        }
        
        if (worm.isOwnWorm()) {
            if (latestPosition != null) {
                if (pos.getX() > latestPosition.getX()) direction = Direction.EAST;
                if (pos.getX() < latestPosition.getX()) direction = Direction.WEST;
                if (pos.getY() > latestPosition.getY()) direction = Direction.NORTH;
                if (pos.getY() < latestPosition.getY()) direction = Direction.SOUTH;
            }
            latestPosition = pos;
        }
    }
    
    public void removeWorm(String id) {
        LinkedList<Position> worm = worms.get(id).getPositions();
        
        while (!worm.isEmpty()) {
            Position pos = worm.poll();
            this.erase(pos);
        }
        
        worms.remove(id);
    }
    
    public void drawCake(Position pos) {
        map[pos.getX()][pos.getY()] = new Tile(TileType.CAKE, Color.YELLOW);
        
        this.repaint();
    }
    
    public void drawWall(Position pos) {
        map[pos.getX()][pos.getY()] = new Tile(TileType.WALL, Color.DARK_GRAY);
        
        this.repaint();
    }
    
    public void erase(Position pos) {
        map[pos.getX()][pos.getY()] = new Tile(TileType.AIR, Color.BLACK);
        
        this.repaint();
    }
    
    void directionPressed(Direction d) {
        if (direction != null) {
            switch (direction) {
                case NORTH:
                    switch (d) {
                        case WEST:
                            connection.turnLeft();
                            break;
                        case EAST:
                            connection.turnRight();
                            break;
                    }
                    break;
                case EAST:
                    switch (d) {
                        case SOUTH:
                            connection.turnRight();
                            break;
                        case NORTH:
                            connection.turnLeft();
                            break;
                    }
                    break;
                case SOUTH:
                    switch (d) {
                        case WEST:
                            connection.turnRight();
                            break;
                        case EAST:
                            connection.turnLeft();
                            break;
                    }
                    break;
                case WEST:
                    switch (d) {
                        case SOUTH:
                            connection.turnLeft();
                            break;
                        case NORTH:
                            connection.turnRight();
                            break;
                    }
                    break;
            }
        }
    }
    
    private void drawWormBit(Position pos, Color c) {
        map[pos.getX()][pos.getY()] = new Tile(TileType.WORM, c);
        
        this.repaint();
    }
    
    private Position parseCoordinates(int x, int y) {
        return new Position((x-1)*fieldSize, mapSizeY*fieldSize-(y*fieldSize));
    }
}