// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.math.newtype;

import java.util.Optional;

public class PosInt extends NewInt {

  public static Optional<PosInt> of(int n) {
    return of(n, PosInt::isPos, PosInt::new);
  }

  public static Optional<PosInt> ofNullable(Integer n) {
    return ofNullable(n, PosInt::isPos, PosInt::new);
  }

  public static boolean isPos(int n) {
    return 0 < n;
  }

  private PosInt(int value) {
    super(value);
  }

}
