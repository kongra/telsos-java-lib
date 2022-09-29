// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.math.newtype;

import java.util.Optional;

public final class PosLong extends NewLong {

  public static Optional<PosLong> of(long n) {
    return of(n, PosLong::isPos, PosLong::new);
  }

  public static Optional<PosLong> ofNullable(Long n) {
    return ofNullable(n, PosLong::isPos, PosLong::new);
  }

  public static boolean isPos(long n) {
    return 0 < n;
  }

  private PosLong(long value) {
    super(value);
  }

}