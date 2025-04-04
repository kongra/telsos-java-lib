package telsos.java.lib.util;

import java.util.Iterator;

import telsos.java.lib.O;

public final class NonDestructiveIterator<E> implements Iterator<E> {

  public static <E> NonDestructiveIterator<E> of(Iterator<E> iterator) {
    if (iterator instanceof final NonDestructiveIterator<E> it)
      return it;

    O.nn(iterator);
    return new NonDestructiveIterator<>(iterator);
  }

  private final Iterator<E> iterator;

  private NonDestructiveIterator(Iterator<E> iterator) {
    this.iterator = iterator;
  }

  @Override
  public void remove() {
    throw new UnsupportedOperationException("remove");
  }

  @Override
  public boolean hasNext() {
    return iterator.hasNext();
  }

  @Override
  public E next() {
    return iterator.next();
  }

}
