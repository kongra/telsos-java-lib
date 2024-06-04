// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos;

import java.util.Optional;
import java.util.function.Supplier;

public enum Kleene {

  TRUE, FALSE, UNKNOWN;

  public static Kleene fromBoolean(boolean b) {
    return b ? TRUE : FALSE;
  }

  public static Kleene fromBoolean(Boolean b) {
    return b == null ? UNKNOWN : fromBoolean(b.booleanValue());
  }

  public static Optional<Kleene> parseString(String s) {
    return switch (s) {
      case "true", "TRUE"       -> Optional.of(TRUE);
      case "false", "FALSE"     -> Optional.of(FALSE);
      case "unknown", "UNKNOWN" -> Optional.of(UNKNOWN);
      default                   -> Optional.empty();
    };
  }

  public static Kleene not(Kleene k) {
    return switch (k) {
      case TRUE    -> FALSE;
      case FALSE   -> TRUE;
      case UNKNOWN -> UNKNOWN;
    };
  }

  public static Kleene not(Supplier<Kleene> supplier) {
    return not(supplier.get());
  }

  public Kleene negate() {
    return not(this);
  }

  public static Kleene and(Kleene a, Supplier<Kleene> b) {
    return switch (a) {
      case TRUE    -> b.get();
      case FALSE   -> FALSE;
      case UNKNOWN -> b.get() == FALSE ? FALSE : UNKNOWN;
    };
  }

  public Kleene and(Supplier<Kleene> b) {
    return and(this, b);
  }

  public static Kleene or(Kleene a, Supplier<Kleene> b) {
    return switch (a) {
      case TRUE    -> TRUE;
      case FALSE   -> b.get();
      case UNKNOWN -> b.get() == TRUE ? TRUE : UNKNOWN;
    };
  }

  public Kleene or(Supplier<Kleene> b) {
    return or(this, b);
  }

  public static Kleene xor(Kleene a, Supplier<Kleene> b) {
    return switch (a) {
      case TRUE    -> not(b);
      case FALSE   -> b.get();
      case UNKNOWN -> UNKNOWN;
    };
  }

  public Kleene xor(Supplier<Kleene> b) {
    return xor(this, b);
  }

  @FunctionalInterface
  public interface Predicate<T> {

    Kleene test(T t);

    default Predicate<T> and(Predicate<T> other) {
      return t -> Kleene.and(test(t), () -> other.test(t));
    }

    default Predicate<T> or(Predicate<T> other) {
      return t -> Kleene.or(test(t), () -> other.test(t));
    }

    default Predicate<T> negate() {
      return t -> Kleene.not(test(t));
    }

    default Predicate<T> xor(Predicate<T> other) {
      return t -> Kleene.xor(test(t), () -> other.test(t));
    }
  }
}
