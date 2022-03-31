// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.paip;

import static telsos.Ch.chSome;

import java.util.Deque;
import java.util.LinkedList;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.function.Supplier;

public class BreadthFirstSearch<T> {

  @FunctionalInterface
  public interface Adjs<T> extends Function<T, Iterable<T>> {}

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
      final var it = carrier.getFirst().iterator();
      while (it.hasNext()) {
        final var e = it.next();
        if (goal.test(e))
          return Optional.of(e);

        carrier.addLast(chSome(adjs.apply(e)));
      }

      // No more elements in it, let's remove it
      carrier.removeFirst();
    }

    return Optional.empty();
  }

  private final Adjs<T> adjs;

  private final Predicate<T> goal;

  private BreadthFirstSearch(Adjs<T> adjs, Predicate<T> goal) {
    this.goal = goal;
    this.adjs = adjs;
  }
}
