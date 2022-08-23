package telsos.string;

public class ValidatedString {

  private final String value;

  protected ValidatedString(String value) {
    this.value = value;
  }

  public String value() {
    return value;
  }

  @Override
  public String toString() {
    return value();
  }

  @Override
  public final int hashCode() {
    return 31 + value().hashCode();
  }

  @Override
  public final boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null)
      return false;
    if (this.getClass() == obj.getClass())
      return value().equals(((ValidatedString) obj).value());

    return false;
  }

}
