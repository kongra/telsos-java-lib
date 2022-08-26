package telsos.newtype;

import java.util.Objects;

public class Newtype<T> extends AbstractNewtype<Newtype<T>> {

  private final T value;

  public Newtype(T value) {
    this.value = Objects.requireNonNull(value);
  }

  @Override
  protected final int hash() {
    return value.hashCode();
  }

  @Override
  protected final boolean isEqualTo(Newtype<T> other) {
    return value.equals(other.value);
  }

  public T value() {
    return value;
  }

}
