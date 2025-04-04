package telsos.java.lib.ex;

public final class ExInfo extends RuntimeException {

  private final transient Object data;

  public ExInfo() {
    data = null;
  }

  public ExInfo(String message) {
    this(message, (Object) null);
  }

  public ExInfo(Throwable cause) {
    super(cause);
    data = null;
  }

  public ExInfo(String message, Object data) {
    super(message);
    this.data = data;
  }

  public ExInfo(String message, Throwable cause) {
    this(message, cause, null);
  }

  public ExInfo(String message, Throwable cause, Object data) {
    super(message, cause);
    this.data = data;
  }

  public Object getData() {
    return data;
  }

  @Override
  public String toString() {
    return "ExInfo [getData()=%s, super.toString()=%s]".formatted(
        getData(),
        super.toString());
  }

}
