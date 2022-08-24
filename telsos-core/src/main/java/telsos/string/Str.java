package telsos.string;

import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;

import telsos.Ch;

public final class Str {

  public static <T extends StringRef> T validOf(String s,
      Ch<String> ch,
      Function<String, T> constr) {
    return constr.apply(ch.apply(s));
  }

  public static <T extends StringRef> Optional<T> validOf(String s,
      Predicate<String> pred,
      Function<String, T> constr) {
    return pred.test(s)
        ? Optional.of(constr.apply(s))
        : Optional.empty();
  }

  public static <T extends StringRef> T validOfStripped(String s,
      Ch<String> ch,
      Function<String, T> constr) {
    return validOf(strip(s), ch, constr);
  }

  public static <T extends StringRef> Optional<T> validOfStripped(String s,
      Predicate<String> pred,
      Function<String, T> constr) {
    return validOf(strip(s), pred, constr);
  }

  public static String strip(String s) {
    return s == null ? null : s.strip();
  }

  private Str() {}

}
