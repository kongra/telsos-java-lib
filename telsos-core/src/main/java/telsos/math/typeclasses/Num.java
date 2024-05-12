// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math.typeclasses;

public interface Num<T> {

  T add(T x, T y);

  T subtract(T x, T y);

  T multiply(T x, T y);

  T divide(T x, T y);

  default T abs(T x) {
    return signum(x) < 0 ? negate(x) : x;
  }

  T negate(T x);

  int signum(T x);

  T fromInt(int i);

}
