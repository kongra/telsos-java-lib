package telsos.string;

import java.util.Objects;

public class StringRef {

  private final String value;

  protected StringRef(String value) {
    Objects.requireNonNull(value);
    this.value = value;
  }

  public final String value() {
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
      return value().equals(((StringRef) obj).value());

    return false;
  }

}
