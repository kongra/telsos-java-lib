// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.newtype;

import java.util.Optional;

public final class NatInt extends NewInt {

  public static Optional<NatInt> of(int n) {
    return of(n, NatInt::isNat, NatInt::new);
  }

  public static boolean isNat(int n) {
    return 0 <= n;
  }

  private NatInt(int value) {
    super(value);
  }

}
