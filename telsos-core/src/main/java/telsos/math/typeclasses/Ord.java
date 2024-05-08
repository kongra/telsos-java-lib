// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math.typeclasses;

import java.util.Comparator;

import telsos.ImpossibleException;

@FunctionalInterface
public interface Ord<T> {

  static <T> Ord<T> of(Comparator<T> comparator) {
    return (o1, o2) -> asOrdering(comparator.compare(o1, o2));
  }

  enum Ordering {
    LT, EQ, GT
  }

  Ordering compare(T o1, T o2);

  default boolean lt(T o1, T o2) {
    final var ordering = compare(o1, o2);
    return ordering == Ordering.LT;
  }

  default boolean lte(T o1, T o2) {
    final var ordering = compare(o1, o2);
    return ordering == Ordering.LT || ordering == Ordering.EQ;
  }

  default boolean gt(T o1, T o2) {
    final var ordering = compare(o1, o2);
    return ordering == Ordering.GT;
  }

  default boolean gte(T o1, T o2) {
    final var ordering = compare(o1, o2);
    return ordering == Ordering.GT || ordering == Ordering.EQ;
  }

  default T max(T o1, T o2) {
    return gt(o1, o2) ? o1 : o2;
  }

  default T min(T o1, T o2) {
    return lt(o1, o2) ? o1 : o2;
  }

  static Ordering asOrdering(int i) {
    return switch (Integer.signum(i)) {
      case -1 -> Ordering.LT;
      case 0  -> Ordering.EQ;
      case 1  -> Ordering.GT;
      default -> throw new ImpossibleException();
    };
  }

}
