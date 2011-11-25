package test;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

import net.Connection;

public class NetTester extends JFrame {
    private Connection connection;
    
    private JPanel panel;
    private JTextField textField;
    
    public NetTester() { }
    
    public NetTester(Connection c) {
        connection = c;
        
        textField = new JTextField();
        panel = new JPanel();
        
        textField.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                connection.sendFreestyleMessage(event.getActionCommand());
                textField.setText("");
            }
        });
        
        panel.setLayout(new BorderLayout());
        panel.add(textField, BorderLayout.SOUTH);
        
        this.add(panel);
        this.setTitle("NetTester");
        this.setSize(400, 42);
        this.setLocation(600, 600);
        
        this.setDefaultCloseOperation(EXIT_ON_CLOSE);
        this.setVisible(true);
    }
    
    public JTextField getAttachableNetTester(Connection c) {
        connection = c;
        textField = new JTextField();
        textField.setFont(new Font("Lucida", 0, 10));
        textField.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                connection.sendFreestyleMessage(event.getActionCommand());
                textField.setText("");
            }
        });
        
        return textField;
    }
}
