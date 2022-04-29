// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.paip;

import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.function.Predicate;
import java.util.function.Supplier;

import io.vavr.Function1;
import io.vavr.control.Option;

public final class BreadthFirstSearch<T> {

  @FunctionalInterface
  public interface Adjs<T> extends Function1<T, Iterable<T>> {}

  @FunctionalInterface
  public interface CarrierSupplier<T> extends Supplier<Deque<Iterable<T>>> {}

  public static <T> BreadthFirstSearch<T> of(Adjs<T> adjs, Predicate<T> goal) {
    return new BreadthFirstSearch<>(adjs, goal);
  }

  public Option<T> search(T start) {
    return search(start, LinkedList::new);
  }

  public Option<T> search(T start, CarrierSupplier<T> cs) {
    final var carrier = cs.get();
    carrier.addFirst(List.of(start));
    return searchImpl(carrier);
  }

  private Option<T> searchImpl(Deque<Iterable<T>> carrier) {
    while (!carrier.isEmpty()) {
      final var it = carrier.getFirst().iterator();
      while (it.hasNext()) {
        final var e = it.next();
        if (goal.test(e))
          return Option.of(e);

        var children = adjs.apply(e);
        if (areNonEmpty(children)) {
          carrier.addLast(children);
        }
      }

      // No more elements in it, let's remove it
      carrier.removeFirst();
    }

    return Option.none();
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
