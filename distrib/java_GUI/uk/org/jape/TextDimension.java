import java.awt.Dimension;
public class TextDimension extends Dimension
{ int    descent;

  public TextDimension(int width, int height, int descent)
  { super(width, height);
    this.descent = descent;
  }

  public String toString()
  { return "TextDimension("+width+","+height+","+descent+")";
  }
}

