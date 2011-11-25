package game;

import javax.swing.*;
import javax.swing.event.*;
import java.awt.*;
import java.awt.event.*;

import util.LimitedDocument;
/**
 *
 * @author jesper
 */
public class LoginPanel extends JPanel {

    private JLabel          headline        = new JLabel("Opret konto");

    private JLabel          labelUsername   = new JLabel( "Brugernavn: " );
    private JTextField      fieldUsername   = new JTextField( "",15 );
    private JLabel          statusUsername  = new JLabel();

    private JLabel          labelPassword1  = new JLabel( "Kodeord: " );
    private JPasswordField  fieldPassword1  = new JPasswordField( "",15 );
    private JLabel          statusPassword1 = new JLabel();

    private JLabel          labelPassword2  = new JLabel( "Gentag kodeord: " );
    private JPasswordField  fieldPassword2  = new JPasswordField( "",15 );
    private JLabel          statusPassword2 = new JLabel();

    private JButton         buttonLogin     = new JButton("Opret");
    
    private JCheckBox       toggleCreate    = new JCheckBox("Opret konto");
    
    private boolean         usernameValid   = false;
    private boolean         password1Valid  = false;
    private boolean         password2Valid  = false;

    final private int usernameFieldMin = 2;
    final private int usernameFieldMax = 30;
    final private int passwordFieldMin = 4;
    final private ProgramWindow window;

    public LoginPanel(ProgramWindow p) {
        this.window = p;
        this.setLayout(null);
        this.setPreferredSize(new Dimension(450,250));

        prepHeadline();
        prepUsername();
        prepPassword1();
        prepPassword2();
        prepLoginButton();
        prepToggleCreate();
        
        setCreateMode(false);

        setUsernameStatusIcon(false);
        setPassword1StatusIcon(false);
        setPassword2StatusIcon(false);

        mountComponent(headline,182,30);
        mountComponent(labelUsername,80,80);
        mountComponent(fieldUsername,166,80);
        mountComponent(statusUsername,340,80);
        mountComponent(labelPassword1,98,105);
        mountComponent(fieldPassword1,166,105);
        mountComponent(statusPassword1,340,105);
        mountComponent(labelPassword2,51,130);
        mountComponent(fieldPassword2,166,130);
        mountComponent(statusPassword2,340,130);
        mountComponent(toggleCreate, 162,152);
        mountComponent(buttonLogin,200,185);
    }

    private void prepHeadline() {
        headline.setFont( new Font("sans-serif", Font.BOLD, 22) );
    }

    private void prepUsername(){

        //Limit input length via LimitedDocument
        fieldUsername.setDocument(new LimitedDocument(usernameFieldMax));

        //Keylistener
        fieldUsername.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    fieldPassword1.requestFocus();
                }
            }
        });

        //Documentlistener
        fieldUsername.getDocument().addDocumentListener(new DocumentListener() {
            public void changedUpdate(DocumentEvent d) {
                validateUsername();
            }
            public void removeUpdate(DocumentEvent d) {
                validateUsername();
            }
            public void insertUpdate(DocumentEvent d) {
                validateUsername();
            }
        });
    }

    private void prepPassword1() {

        fieldPassword1.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    if (toggleCreate.isSelected()) fieldPassword2.requestFocus();
                    else createOrLogin();
                }
            }
        });

        //Documentlistener
        fieldPassword1.getDocument().addDocumentListener(new DocumentListener() {
            public void changedUpdate(DocumentEvent d) {validatePassword1();}
            public void removeUpdate(DocumentEvent d) {validatePassword1();}
            public void insertUpdate(DocumentEvent d) {validatePassword1();}
        });

    }

    private void prepPassword2() {

        fieldPassword2.addKeyListener(new KeyAdapter() {
            @Override
            public void keyPressed(KeyEvent e) {
                if (e.getKeyCode() == KeyEvent.VK_ENTER) {
                    if (buttonLogin.isEnabled()) createOrLogin();
                }
            }
        });

        //Documentlistener
        fieldPassword2.getDocument().addDocumentListener(new DocumentListener() {
            public void changedUpdate(DocumentEvent d) {validatePassword2();}
            public void removeUpdate(DocumentEvent d) {validatePassword2();}
            public void insertUpdate(DocumentEvent d) {validatePassword2();}
        });
    }

    private void prepLoginButton() {
        buttonLogin.setActionCommand("do_create");
        buttonLogin.setEnabled(false);
        buttonLogin.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e){
                if (e.getActionCommand().equals("do_create")) {
                    createOrLogin();
                }
            }
        });
    }
    
    private void prepToggleCreate() {
        toggleCreate.addItemListener(new ItemListener() {
            public void itemStateChanged(ItemEvent evt) {
                setCreateMode(evt.getStateChange() == evt.SELECTED);
            }
        });
    }

    private void updateValidState() {
        setUsernameStatusIcon(usernameValid);
        setPassword1StatusIcon(password1Valid);
        setPassword2StatusIcon(password2Valid);
        
        if (toggleCreate.isSelected()) {
            if (usernameValid & password1Valid & password2Valid) buttonLogin.setEnabled(true);
            else buttonLogin.setEnabled(false);
        } else {
            if (usernameValid & password1Valid) buttonLogin.setEnabled(true);
            else buttonLogin.setEnabled(false);
        }
    }
    
    private void validateUsername() {
        if (fieldUsername.getText().length() < usernameFieldMin) usernameValid = false;
        else usernameValid = true;
        
        updateValidState();
    }

    private void validatePassword1() {
        String password1 = getPassword1();

        if ( password1.length() < passwordFieldMin) {
            password1Valid = false;
        } else {
            password1Valid = true;
        }
        
        validatePassword2();
        updateValidState();
    }

    private void validatePassword2() {
        String password1 = getPassword1();
        String password2 = getPassword2();

        if ( password1.length() < passwordFieldMin) {
            password2Valid = false;
        } else {
            if (password1.equals(password2)) {
                password2Valid = true;
            } else {
                password2Valid = false;
            }
        }
        
        updateValidState();
    }

    private void setUsernameStatusIcon(boolean y) {
        if (y) statusUsername.setIcon(window.fetchImage("images/ok.png"));
        else statusUsername.setIcon(window.fetchImage("images/error.png"));
    }

    private void setPassword1StatusIcon(boolean y) {
        if (y) statusPassword1.setIcon(window.fetchImage("images/ok.png"));
        else statusPassword1.setIcon(window.fetchImage("images/error.png"));
    }

    private void setPassword2StatusIcon(boolean y) {
        if (y) statusPassword2.setIcon(window.fetchImage("images/ok.png"));
        else statusPassword2.setIcon(window.fetchImage("images/error.png"));
    }

    private void mountComponent(JComponent j, int x, int y) {
        this.add(j);
        Insets insets = this.getInsets();
        Dimension size = j.getPreferredSize();
        j.setBounds(insets.left+x, insets.top+y, size.width, size.height);
    }

    private void createOrLogin() {
        this.window.createOrLogin(toggleCreate.isSelected(), fieldUsername.getText(), getPassword1());
    }

    private String getPassword1() {
        char[] passwordChars = fieldPassword1.getPassword();
        String password = "";
        for ( int i=0; i < passwordChars.length; i++ ){
            password += passwordChars[i];
        }
        return password;
    }

    private String getPassword2() {
        char[] passwordChars = fieldPassword2.getPassword();
       String password = "";
        for ( int i=0; i < passwordChars.length; i++ ){
            password += passwordChars[i];
        }
        return password;
    }
    
    private void setCreateMode(boolean mode) {
        if (mode) {
            labelPassword2.setEnabled(true);
            fieldPassword2.setEnabled(true);
            statusUsername.setVisible(true);
            statusPassword1.setVisible(true);
            statusPassword2.setVisible(true);
            buttonLogin.setText("Opret");
            headline.setText(" Opret");
        } else {
            labelPassword2.setEnabled(false);
            fieldPassword2.setEnabled(false);
            fieldPassword2.setText("");
            statusUsername.setVisible(false);
            statusPassword1.setVisible(false);
            statusPassword2.setVisible(false);
            buttonLogin.setText("Log ind");
            headline.setText("Log ind");
        }
    }
}
