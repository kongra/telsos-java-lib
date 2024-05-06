// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.paip;

import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.Objects;
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
      final var firstIterableInCarrier = carrier.getFirst();
      for (final T element : firstIterableInCarrier) {
        if (goal.test(element))
          // We have a success.
          return Optional.of(element);

        final var iterableOverChildren = adjs.apply(element);
        if (isNotEmpty(iterableOverChildren)) {
          carrier.addLast(iterableOverChildren);
        }
      }

      // No more elements in the iterable, let's remove it from the carrier.
      carrier.removeFirst();
    }

    // No more iterables in the carrier - we didn't succeed.
    return Optional.empty();
  }

  private boolean isNotEmpty(Iterable<T> iterable) {
    return iterable != null && iterable.iterator().hasNext();
  }

  private final Adjs<T> adjs;

  private final Predicate<T> goal;

  private BreadthFirstSearch(Adjs<T> adjs, Predicate<T> goal) {
    this.adjs = Objects.requireNonNull(adjs);
    this.goal = Objects.requireNonNull(goal);
  }
}
