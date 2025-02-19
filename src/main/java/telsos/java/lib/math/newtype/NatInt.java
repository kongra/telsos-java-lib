package telsos.java.lib.math.newtype;

import java.util.Optional;

public final class NatInt extends NewInt {

  public static Optional<NatInt> of(int n) {
    return of(n, NatInt::isNat, NatInt::new);
  }

  public static Optional<NatInt> ofNullable(Integer n) {
    return ofNullable(n, NatInt::isNat, NatInt::new);
  }

  public static boolean isNat(int n) {
    return n >= 0;
  }

  private NatInt(int value) {
    super(value);
  }

}
