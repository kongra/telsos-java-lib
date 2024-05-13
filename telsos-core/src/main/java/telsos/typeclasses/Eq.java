// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.typeclasses;

@FunctionalInterface
public interface Eq<T> {

  boolean areEqual(T x, T y);

  default boolean areNotEqual(T x, T y) {
    return !areEqual(x, y);
  }

}
