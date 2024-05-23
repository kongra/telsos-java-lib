package telsos;

public class ImpossibleException extends RuntimeException {

  public ImpossibleException() {}

  public ImpossibleException(String message) {
    super(message);
  }

  public ImpossibleException(Throwable cause) {
    super(cause);
  }

  public ImpossibleException(String message, Throwable cause) {
    super(message, cause);
  }

  public ImpossibleException(String message, Throwable cause,
      boolean enableSuppression, boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }

}
