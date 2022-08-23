package telsos.string;

import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;

import telsos.Ch;

public final class Str {

  public static <T extends ValidatedString> T validOf(String s,
      Ch<String> ch,
      Function<String, T> constr) {
    return constr.apply(ch.apply(s));
  }

  public static <T extends ValidatedString> Optional<T> validOf(String s,
      Predicate<String> pred,
      Function<String, T> constr) {
    return s == null || !pred.test(s) ? Optional.empty()
        : Optional.of(constr.apply(s));
  }

  public static <T extends ValidatedString> T validOfStripped(String s,
      Ch<String> ch,
      Function<String, T> constructor) {
    return validOf(s.strip(), ch, constructor);
  }

  private Str() {}

}
