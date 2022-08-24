// © 2019 Konrad Grzanek <kongra@gmail.com>
package telsos;

public class ChError extends RuntimeException {

  private final transient Object subject;

  public ChError() {
    subject = null;
  }

  public ChError(long detailMessage) {
    super(String.valueOf(detailMessage));
    subject = detailMessage;
  }

  public ChError(float detailMessage) {
    super(String.valueOf(detailMessage));
    subject = detailMessage;
  }

  public ChError(double detailMessage) {
    super(String.valueOf(detailMessage));
    subject = detailMessage;
  }

  public ChError(Object subject) {
    this.subject = subject;
  }

  public ChError(Object subject, String detailMessage) {
    super(detailMessage);
    this.subject = subject;
  }

  public ChError(Object subject, String message, Throwable cause) {
    super(message, cause);
    this.subject = subject;
  }

  public final Object getSubject() {
    return subject;
  }

}
