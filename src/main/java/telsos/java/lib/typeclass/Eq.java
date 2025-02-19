package telsos.java.lib.typeclass;

@FunctionalInterface
public interface Eq<T> {

  boolean areEqual(T x, T y);

  default boolean areNotEqual(T x, T y) {
    return !areEqual(x, y);
  }

}
