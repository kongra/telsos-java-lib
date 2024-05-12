// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math;

import java.util.function.UnaryOperator;

@FunctionalInterface
public interface HeronSqrt<T> {

  NumericFixedPoint<T> fixedPoint();

  default T eval(T x) {
    final var fixedPoint = fixedPoint();
    final var num = fixedPoint.num();
    final UnaryOperator<T> f = fixedPoint.averageDamp(y -> num.divide(x, y));
    return fixedPoint.eval(f, num.fromInt(1));
  }
}
