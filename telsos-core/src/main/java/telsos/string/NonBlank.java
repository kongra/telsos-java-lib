package telsos.string;

import java.util.Optional;

import telsos.newtype.Newtype;

public final class NonBlank extends Newtype<String> {

  public static Optional<NonBlank> of(String s) {
    return of(s, NonBlank::isNonBlank, NonBlank::new);
  }

  public static Optional<NonBlank> ofTrimmed(String s) {
    return Optional.ofNullable(s)
        .map(String::trim)
        .flatMap(NonBlank::of);
  }

  public static boolean isNonBlank(String s) {
    return s != null && !s.isBlank();
  }

  private NonBlank(String value) {
    super(value);
  }

}
