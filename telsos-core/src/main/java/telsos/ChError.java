package telsos;

public class ChError extends AssertionError {

  public ChError() {
  }

  public ChError(Object detailMessage) {
    super(detailMessage);
  }

  public ChError(boolean detailMessage) {
    super(detailMessage);
  }

  public ChError(char detailMessage) {
    super(detailMessage);
  }

  public ChError(int detailMessage) {
    super(detailMessage);
  }

  public ChError(long detailMessage) {
    super(detailMessage);
  }

  public ChError(float detailMessage) {
    super(detailMessage);
  }

  public ChError(double detailMessage) {
    super(detailMessage);
  }

  public ChError(String message, Throwable cause) {
    super(message, cause);
  }

  private static final long serialVersionUID = 1L;

}
