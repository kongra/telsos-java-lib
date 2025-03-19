package telsos.java.lib.math.newtype;

import java.util.Optional;

public final class NatLong extends NewLong {

  public static Optional<NatLong> of(long n) {
    return of(n, NatLong::isNat, NatLong::new);
  }

  public static Optional<NatLong> ofNullable(Long n) {
    return ofNullable(n, NatLong::isNat, NatLong::new);
  }

  public static boolean isNat(long n) {
    return n >= 0;
  }

  private NatLong(long value) {
    super(value);
  }

}
