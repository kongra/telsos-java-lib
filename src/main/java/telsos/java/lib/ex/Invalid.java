package telsos.java.lib.ex;

public final class Invalid extends RuntimeException {

  private final transient Object what;

  public Invalid(String message, Object what) {
    this(message, null, what);
  }

  public Invalid(Throwable cause, Object what) {
    this(null, cause, what);
  }

  public Invalid(String message, Throwable cause, Object what) {
    final var enableSuppression = false;
    final var writableStackTrace = false;
    super(message, cause, enableSuppression, writableStackTrace);
    this.what = what;
  }

  public Object getWhat() {
    return what;
  }

  @Override
  public String toString() {
    return "Invalid [getWhat()=%s, super.toString()=%s]".formatted(
        getWhat(),
        super.toString());
  }

}
