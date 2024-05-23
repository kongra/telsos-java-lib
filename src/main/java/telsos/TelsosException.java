// © 2019 Konrad Grzanek <kongra@gmail.com>
package telsos;

public class TelsosException extends RuntimeException {

  public TelsosException() {}

  public TelsosException(String message, Throwable cause,
      boolean enableSuppression, boolean writableStackTrace) {
    super(message, cause, enableSuppression, writableStackTrace);
  }

  public TelsosException(String message, Throwable cause) {
    super(message, cause);
  }

  public TelsosException(String message) {
    super(message);
  }

  public TelsosException(Throwable cause) {
    super(cause);
  }

}
