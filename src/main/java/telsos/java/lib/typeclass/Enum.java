package telsos.java.lib.typeclass;

import java.util.Optional;

public interface Enum<T> {

  Optional<T> fromInt(int i);

  int toInt(T e);

  Optional<T> pred(T e);

  Optional<T> succ(T e);

}
