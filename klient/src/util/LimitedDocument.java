package util;

import javax.swing.text.AttributeSet;
import javax.swing.text.PlainDocument;
import javax.swing.text.BadLocationException;
/**
 *
 * @author jesper
 */
public class LimitedDocument extends PlainDocument {
  int limit = 10;
  boolean useBeep = false;

  public LimitedDocument(int limit){
      this.limit = limit;
  }

  public LimitedDocument(int limit, boolean useBeep){
      this.limit = limit;
      this.useBeep = useBeep;
  }

  @Override
  public void insertString(int offs, String str, AttributeSet a) throws BadLocationException{
    if (str != null && this.getLength() + str.length() > limit)
    {
      if (useBeep) java.awt.Toolkit.getDefaultToolkit().beep();
      return;
    }
    super.insertString(offs, str, a);
  }
}
