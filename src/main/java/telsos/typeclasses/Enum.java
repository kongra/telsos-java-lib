// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.typeclasses;

import java.util.Optional;

public interface Enum<T> {

  Optional<T> fromInt(int i);

  int toInt(T e);

  Optional<T> pred(T e);

  Optional<T> succ(T e);

}
