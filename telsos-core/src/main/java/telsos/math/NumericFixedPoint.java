// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math;

import java.util.function.UnaryOperator;

import telsos.math.typeclasses.Num;
import telsos.math.typeclasses.Ord;

interface NumericFixedPoint<T> {

  Num<T> num();

  Ord<T> ord();

  default T avg(T x, T y) {
    final var num = num();
    final var sum = num.add(x, y);
    final var two = num.fromInt(2);
    return num.divide(sum, two);
  }

  default T abs(T x) {
    final var num = num();
    final var ord = ord();
    return ord.lt(x, num.fromInt(0)) ? num.negate(x) : x;
  }

  default boolean areCloseEnough(T x, T y) {
    final var num = num();
    final var ord = ord();
    final var epsilon = num.divide(num.fromInt(1), num.fromInt(1_000_000));
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
