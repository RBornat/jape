// A test class to see if the system environment is passed on at install-time
public class testclass
{ public testclass()
  { System.err.println("INSTALL DIRECTORY IS: "+System.getProperty("INSTALL"));
    System.getProperties().save(System.err, "Current Properties");
  }
}
