package telsos.string;

import java.util.Optional;
import java.util.function.Predicate;

import telsos.Ch;

public final class NonBlank extends ValidatedString {

  public static NonBlank of(String s) {
    return Str.validOf(s, Ch.nonBlank, NonBlank::new);
  }

  public static Optional<NonBlank> optionally(String s) {
    return Str.validOf(s, predicate, NonBlank::new);
  }

  public static NonBlank ofStripped(String s) {
    return Str.validOfStripped(s, Ch.nonBlank, NonBlank::new);
  }

  public static final Predicate<String> predicate = s -> !s.isBlank();

  private NonBlank(String value) {
    super(value);
  }

}
