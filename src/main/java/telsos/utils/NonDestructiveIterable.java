// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.utils;

import java.util.Iterator;
import java.util.Objects;

public final class NonDestructiveIterable<T> implements Iterable<T> {

  public static <T> NonDestructiveIterable<T> of(Iterable<T> iterable) {
    if (iterable instanceof final NonDestructiveIterable<T> it)
      return it;

    Objects.requireNonNull(iterable);
    return new NonDestructiveIterable<>(iterable);
  }

  private final Iterable<T> iterable;

  private NonDestructiveIterable(Iterable<T> iterable) {
    this.iterable = iterable;
  }

  @Override
  public Iterator<T> iterator() {
    return NonDestructiveIterator.of(iterable.iterator());
  }

}
