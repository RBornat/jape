import java.awt.*;

public class testjape
{ 
  Frame       frame;
  ProofCanvas proof;
  
  public static void main(String[] args)
  { testjape jape = new testjape();
  }

  public testjape()
  { frame = new Frame("Jape");
    proof = new ProofCanvas();
    proof.setSize(200, 200);
    frame.setLayout(new BorderLayout());
    frame.add(proof, "Center");
    frame.pack();
    frame.show();
    testcanvas();
  }

  public void testcanvas()
  {
    TextItem t = new TextItem(proof, new Point(50, 50), "foobaz", 2);
    t.selected=true;
    proof.registerItem(t);
    t = new TextItem(proof, new Point(50, 100), "is best for you", 2);
    t.selected=true;
    t.greyed=true;
    proof.registerItem(t);
    t = new TextItem(proof, new Point(50, 150), "on \u22d6 April \u22d7 Thursdays", 2);
    proof.registerItem(t);
    proof.repaint();
  }
  
}


