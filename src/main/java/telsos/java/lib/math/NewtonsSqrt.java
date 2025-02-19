package telsos.java.lib.math;

import java.util.function.UnaryOperator;

@FunctionalInterface
public interface NewtonsSqrt<T> {

  NewtonsMethod<T> newtonsMethod();

  default T square(T x) {
    return newtonsMethod()
        .fixedPoint()
        .num()
        .multiply(x, x);
  }

  default T eval(T x) {
    final var fixedPoint = newtonsMethod().fixedPoint();
    final var num = fixedPoint.num();
    final var enm = fixedPoint.enm();
    final UnaryOperator<T> g = y -> num.subtract(square(y), x);
    return newtonsMethod().eval(g, enm.fromInt(1).orElseThrow());
  }

}
