package jesty.lib.typeclass;

@FunctionalInterface
public interface Semigroup<T> {

  T sconcat(T x, T y);

}
