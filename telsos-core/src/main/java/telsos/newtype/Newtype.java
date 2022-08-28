// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.newtype;

import java.util.Objects;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;

public class Newtype<T> extends AbstractNewtype<Newtype<T>> {

  protected static <T, S extends Newtype<T>> Optional<S> of(T t,
      Predicate<T> pred,
      Function<T, S> constr) {
    return pred.test(t)
        ? Optional.of(constr.apply(t))
        : Optional.empty();
  }

  private final T value;

  protected Newtype(T value) {
    this.value = Objects.requireNonNull(value);
  }

  public T value() {
    return value;
  }

  @Override
  protected final int hash() {
    return value.hashCode();
  }

  @Override
  protected final boolean isEqualTo(Newtype<T> other) {
    return value.equals(other.value);
  }

  @Override
  public String toString() {
    return value().toString();
  }

}
