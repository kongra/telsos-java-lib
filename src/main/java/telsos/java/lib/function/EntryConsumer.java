package telsos.java.lib.function;

import telsos.java.lib.O;

@FunctionalInterface
public interface EntryConsumer<K, V> {

  void accept(K key, V value, boolean isLast);

  default EntryConsumer<K, V> andThen(EntryConsumer<K, V> after) {
    O.nn(after);
    return (key, value, isLast) -> {
      accept(key, value, isLast);
      after.accept(key, value, isLast);
    };
  }

}
