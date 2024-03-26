// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.paip;

import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.function.Supplier;

public final class BreadthFirstSearch<T> {

  @FunctionalInterface
  public interface CarrierSupplier<T> extends Supplier<Deque<Iterable<T>>> {}

  public static <T> BreadthFirstSearch<T> of(Adjs<T> adjs, Predicate<T> goal) {
    return new BreadthFirstSearch<>(adjs, goal);
  }

  public Optional<T> search(T start) {
    return search(start, LinkedList::new);
  }

  public Optional<T> search(T start, CarrierSupplier<T> cs) {
    final var carrier = cs.get();
    carrier.addFirst(List.of(start));
    return searchImpl(carrier);
  }

  private Optional<T> searchImpl(Deque<Iterable<T>> carrier) {
    while (!carrier.isEmpty()) {
      for (final T e : carrier.getFirst()) {
        if (goal.test(e))
          return Optional.of(e);

        final var children = adjs.apply(e);
        if (areNonEmpty(children))
          carrier.addLast(children);
      }

      // No more elements in it, let's remove it
      carrier.removeFirst();
    }

    return Optional.empty();
  }

  private boolean areNonEmpty(Iterable<T> children) {
    // Returning null when there are no children is good for performance!
    return children != null && children.iterator().hasNext();
  }

  private final Adjs<T> adjs;

  private final Predicate<T> goal;

  private BreadthFirstSearch(Adjs<T> adjs, Predicate<T> goal) {
    this.goal = goal;
    this.adjs = adjs;
  }
}
