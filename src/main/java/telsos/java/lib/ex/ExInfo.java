package telsos.java.lib.ex;

public class ExInfo extends RuntimeException {

  public ExInfo() {}

  public ExInfo(String message, Throwable cause) {
    super(message, cause);
  }

  public ExInfo(String message) {
    super(message);
  }

  public ExInfo(Throwable cause) {
    super(cause);
  }

}
