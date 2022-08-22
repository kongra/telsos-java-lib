// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.paip;

import java.util.Deque;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Predicate;
import java.util.function.Supplier;

public final class DepthFirstSearch<T> {

  @FunctionalInterface
  public interface CarrierSupplier<T> extends Supplier<Deque<Iterator<T>>> {}

  public static <T> DepthFirstSearch<T> of(Adjs<T> adjs, Predicate<T> goal) {
    return new DepthFirstSearch<>(adjs, goal);
  }

  public Optional<T> search(T start) {
    return search(start, LinkedList::new);
  }

  public Optional<T> search(T start, CarrierSupplier<T> cs) {
    final var carrier = cs.get();
    carrier.addFirst(List.of(start).iterator());
    return searchImpl(carrier);
  }

  private Optional<T> searchImpl(Deque<Iterator<T>> carrier) {
    while (!carrier.isEmpty()) {
      final var it = carrier.getFirst();
      if (!it.hasNext()) {
        carrier.removeFirst();
        continue;
      }

      final var e = it.next();
      if (goal.test(e))
        return Optional.of(e);

      final var children = adjs.apply(e);
      if (children != null) {
        final var childrenIt = children.iterator();
        if (childrenIt.hasNext()) {
          carrier.addFirst(childrenIt);
        }
      }
    }

    return Optional.empty();
  }

  private final Adjs<T> adjs;

  private final Predicate<T> goal;

  private DepthFirstSearch(Adjs<T> adjs, Predicate<T> goal) {
    this.goal = goal;
    this.adjs = adjs;
  }
}
