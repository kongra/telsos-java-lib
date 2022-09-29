// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.math.newtype;

import java.util.Optional;
import java.util.function.IntFunction;
import java.util.function.IntPredicate;

import telsos.newtype.AbstractNewtype;

public class NewInt extends AbstractNewtype<NewInt> {

  protected static <S extends NewInt> Optional<S> of(int n,
      IntPredicate pred,
      IntFunction<S> constr) {
    return pred.test(n)
        ? Optional.of(constr.apply(n))
        : Optional.empty();
  }

  protected static <S extends NewInt> Optional<S> ofNullable(Integer n,
      IntPredicate pred,
      IntFunction<S> constr) {
    return null == n ? Optional.empty() : of(n, pred, constr);
  }

  private final int value;

  protected NewInt(int value) {
    this.value = value;
  }

  public int value() {
    return value;
  }

  @Override
  protected final int hash() {
    return value();
  }

  @Override
  protected final boolean isEqualTo(NewInt other) {
    return value() == other.value();
  }

  @Override
  public String toString() {
    return String.valueOf(value());
  }

}
