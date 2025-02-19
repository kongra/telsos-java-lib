package telsos.java.lib.logging;

public final class Logs {

  public static System.Logger get(Class<?> c) {
    return System.getLogger(c.getName());
  }

  private Logs() {}

}
