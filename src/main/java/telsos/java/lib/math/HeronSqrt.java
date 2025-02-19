package telsos.java.lib.math;

@FunctionalInterface
public interface HeronSqrt<T> {

  NumericFixedPoint<T> fixedPoint();

  default T eval(T x) {
    final var fixedPoint = fixedPoint();
    final var enm = fixedPoint.enm();
    final var num = fixedPoint.num();
    final var f = fixedPoint.averageDamp(y -> num.divide(x, y));
    return fixedPoint.eval(f, enm.fromInt(1).orElseThrow());
  }
}
