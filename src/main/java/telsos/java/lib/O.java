// © 2025 Konrad Grzanek <kongra@gmail.com>
package telsos.java.lib;

import java.util.Objects;
import java.util.function.Supplier;

public final class O {

  public static <T> T nn(T obj) {
    return Objects.requireNonNull(obj);
  }

  public static <T> T nn(T obj, String message) {
    return Objects.requireNonNull(obj, message);
  }

  public static boolean isNull(Object obj) {
    return obj == null;
  }

  public static boolean isSome(Object obj) {
    return !isNull(obj);
  }

  public static <T> T or(T obj, T other) {
    return isSome(obj) ? obj : other;
  }

  @SafeVarargs
  public static <T> T or(T obj, T other, T... others) {
    if (isSome(obj))
      return obj;

    if (isSome(other))
      return other;

    for (final T e : others) {
      if (isSome(e))
        return e;
    }

    return null;
  }

  public static <T> T or(T obj, Supplier<T> orElse) {
    O.nn(orElse);
    return isSome(obj) ? obj : orElse.get();
  }

  private O() {}
}
