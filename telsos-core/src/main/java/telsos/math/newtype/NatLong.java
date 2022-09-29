// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.math.newtype;

import java.util.Optional;

public final class NatLong extends NewLong {

  public static Optional<NatLong> of(long n) {
    return of(n, NatLong::isNat, NatLong::new);
  }
  
  public static Optional<NatLong> ofNullable(Long n) {
    return ofNullable(n, NatLong::isNat, NatLong::new);
  }

  public static boolean isNat(long n) {
    return 0 <= n;
  }

  private NatLong(long value) {
    super(value);
  }

}
