package telsos.java.lib.ex;

public final class Impossible extends RuntimeException {

  public Impossible() {}

  public Impossible(String message) {
    super(message);
  }

  public Impossible(Throwable cause) {
    super(cause);
  }

  public Impossible(String message, Throwable cause) {
    super(message, cause);
  }

}
