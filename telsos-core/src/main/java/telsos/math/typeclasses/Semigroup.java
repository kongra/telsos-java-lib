// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math.typeclasses;

@FunctionalInterface
public interface Semigroup<T> {

  T sconcat(T x, T y);

}
