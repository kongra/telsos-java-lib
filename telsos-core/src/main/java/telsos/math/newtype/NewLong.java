// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.math.newtype;

import java.util.Optional;
import java.util.function.LongFunction;
import java.util.function.LongPredicate;

import telsos.newtype.AbstractNewtype;

public class NewLong extends AbstractNewtype<NewLong> {

  protected static <S extends NewLong> Optional<S> of(long n,
      LongPredicate pred,
      LongFunction<S> constr) {
    return pred.test(n)
        ? Optional.of(constr.apply(n))
        : Optional.empty();
  }
  
  protected static <S extends NewLong> Optional<S> ofNullable(Long n,
      LongPredicate pred,
      LongFunction<S> constr) {
    return null == n ? Optional.empty() : of(n, pred, constr);
  }

  private final long value;

  protected NewLong(long value) {
    this.value = value;
  }

  public long value() {
    return value;
  }

  @Override
  protected final int hash() {
    return Long.hashCode(value());
  }

  @Override
  protected final boolean isEqualTo(NewLong other) {
    return value() == other.value();
  }

  @Override
  public String toString() {
    return String.valueOf(value());
  }

}
