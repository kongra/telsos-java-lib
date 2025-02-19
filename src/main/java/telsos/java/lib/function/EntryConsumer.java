package telsos.java.lib.function;

import java.util.Objects;

@FunctionalInterface
public interface EntryConsumer<K, V> {

  void accept(K key, V value, boolean isLast);

  default EntryConsumer<K, V> andThen(EntryConsumer<K, V> after) {
    Objects.requireNonNull(after);
    return (key, value, isLast) -> {
      accept(key, value, isLast);
      after.accept(key, value, isLast);
    };
  }

}
