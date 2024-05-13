// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.typeclasses;

@FunctionalInterface
public interface Semigroup<T> {

  T sconcat(T x, T y);

}
