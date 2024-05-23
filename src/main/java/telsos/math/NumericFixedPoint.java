// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math;

import java.util.function.UnaryOperator;

import telsos.typeclasses.Num;
import telsos.typeclasses.Ord;

public interface NumericFixedPoint<T> {

  Num<T> num();

  telsos.typeclasses.Enum<T> enm();

  Ord<T> ord();

  default T avg(T x, T y) {
    final var num = num();
    final var sum = num.add(x, y);
    final var two = enm().fromInt(2).orElseThrow();
    return num.divide(sum, two);
  }

  default T abs(T x) {
    final var num = num();
    final var ord = ord();
    return ord.lt(x, enm().fromInt(0).orElseThrow()) ? num.negate(x) : x;
  }

  default boolean areCloseEnough(T x, T y) {
    final var enm = enm();
    final var num = num();
    final var ord = ord();
    final var epsilon = num.divide(
        enm.fromInt(1).orElseThrow(),
        enm.fromInt(1_000_000).orElseThrow());
    return ord.lte(abs(num.subtract(x, y)), epsilon);
  }

  default T eval(UnaryOperator<T> f, T start, T fOfStart) {
    for (;;) {
      if (areCloseEnough(start, fOfStart))
        return fOfStart;

      start    = fOfStart;
      fOfStart = f.apply(fOfStart);
    }
  }

  default T eval(UnaryOperator<T> f, T start) {
    return eval(f, start, f.apply(start));
  }

  default UnaryOperator<T> averageDamp(UnaryOperator<T> f) {
    return x -> avg(x, f.apply(x));
  }

}
