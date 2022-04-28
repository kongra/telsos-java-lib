// © 2019 Konrad Grzanek <kongra@gmail.com>
package telsos;

public class ChError extends RuntimeException {

  public ChError() {}

  public ChError(long detailMessage) {
    super(String.valueOf(detailMessage));
  }

  public ChError(float detailMessage) {
    super(String.valueOf(detailMessage));
  }

  public ChError(double detailMessage) {
    super(String.valueOf(detailMessage));
  }

  public ChError(String detailMessage) {
    super(detailMessage);
  }

  public ChError(String message, Throwable cause) {
    super(message, cause);
  }

  private static final long serialVersionUID = 1L;

}
