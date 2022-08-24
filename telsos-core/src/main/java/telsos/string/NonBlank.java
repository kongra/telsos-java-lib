package telsos.string;

import java.util.Optional;
import java.util.function.Predicate;

import telsos.Ch;

public final class NonBlank extends StringRef {

  public static NonBlank of(String s) {
    return Str.validOf(s, ch, NonBlank::new);
  }

  public static Optional<NonBlank> optionallyOf(String s) {
    return Str.validOf(s, pred, NonBlank::new);
  }

  public static NonBlank ofStripped(String s) {
    return Str.validOfStripped(s, ch, NonBlank::new);
  }

  public static Optional<NonBlank> optionallyOfStripped(String s) {
    return Str.validOfStripped(s, pred, NonBlank::new);
  }

  public static boolean isBlank(String s) {
    return s == null || s.isBlank();
  }

  public static final Predicate<String> pred = Predicate.not(NonBlank::isBlank);

  public static final Ch<String> ch = Ch.of(pred);

  private NonBlank(String value) {
    super(value);
  }

}
