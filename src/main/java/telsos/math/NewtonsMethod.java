// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math;

import java.util.function.UnaryOperator;

public interface NewtonsMethod<T> {

  NumericFixedPoint<T> fixedPoint();

  default UnaryOperator<T> deriv(UnaryOperator<T> g) {
    final var num = fixedPoint().num();
    final var enm = fixedPoint().enm();
    final var dx = num.divide(
        enm.fromInt(1).orElseThrow(),
        enm.fromInt(1_000_000).orElseThrow());

    return x -> num.divide(
        num.subtract(g.apply(num.add(x, dx)), g.apply(x)),
        dx);
  }

  default UnaryOperator<T> transform(UnaryOperator<T> g) {
    final var num = fixedPoint().num();
    final var gPrime = deriv(g);
    return x -> num.subtract(
        x, num.divide(g.apply(x), gPrime.apply(x)));
  }

  default T eval(UnaryOperator<T> g, T start) {
    return fixedPoint().eval(transform(g), start);
  }

}
