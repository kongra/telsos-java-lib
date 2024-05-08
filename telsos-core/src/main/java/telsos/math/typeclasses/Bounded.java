// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math.typeclasses;

@FunctionalInterface
public interface Bounded<T> {

  interface Bounds<T> {

    T minBound();

    T maxBound();
  }

  Bounds<T> getBounds();

  default T minBound() {
    return getBounds().minBound();
  }

  default T maxBound() {
    return getBounds().maxBound();
  }

}
