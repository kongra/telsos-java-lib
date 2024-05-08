// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.math.typeclasses;

public interface Bounded<T> {

  T minBound();

  T maxBound();

}
