import java.util.Properties;

public class PrintAllProperties
{
  public static void main(String[] args)
  {
    Properties props = System.getProperties();
    props.list(System.out);
  }
}
