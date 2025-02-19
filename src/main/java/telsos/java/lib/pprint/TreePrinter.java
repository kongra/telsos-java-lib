package telsos.java.lib.pprint;

import java.util.LinkedList;
import java.util.function.Consumer;
import java.util.function.Function;

import telsos.java.lib.ex.Impossible;
import telsos.java.lib.math.newtype.NatInt;

public final class TreePrinter<T> {

  @FunctionalInterface
  public interface Adjs<T> extends Function<T, Iterable<T>> {}

  @FunctionalInterface
  public interface Repr<T> extends Function<T, String> {}

  @FunctionalInterface
  public interface Worker extends Consumer<String> {}

  public static <T> TreePrinter<T> of(Adjs<T> adjs, Repr<T> repr) {
    return new TreePrinter<>(adjs, repr);
  }

  public void print(T node, NatInt depth, Worker worker) {
    final var lastChildInfos = new LinkedList<Boolean>();
    lastChildInfos.add(true);
    impl(node, depth.value(), 0, true, lastChildInfos, worker);
  }

  public void print(T node, Worker worker) {
    final var depth = NatInt.of(Integer.MAX_VALUE)
        .orElseThrow(Impossible::new);
    print(node, depth, worker);
  }

  private void impl(T node, int depth, int level, boolean isFirst,
      LinkedList<Boolean> lastChildInfos, Worker worker) {

    final var s = repr.apply(node);
    final var pfx = isFirst ? EMPTY : EOL;
    final var r = level == 0 ? pfx + s : pfx + indent(lastChildInfos) + s;

    worker.accept(r);

    if (level != depth) {
      final var children = adjs.apply(node);
      final var it = children.iterator();

      while (it.hasNext()) {
        final var child = it.next();
        final var isLast = !it.hasNext();

        lastChildInfos.addFirst(isLast);
        impl(child, depth, level + 1, false, lastChildInfos, worker);
        lastChildInfos.removeFirst();
      }
    }
  }

  private static String indent(LinkedList<Boolean> lastChildInfos) {
    final var isLast = lastChildInfos.getFirst();
    // lastChildInfos is never empty!

    final var suffix = Boolean.TRUE.equals(isLast) ? FOR_LAST_CHILD : FOR_CHILD;
    final var prefix = new StringBuilder();

    var n = lastChildInfos.size();

    final var iter = lastChildInfos.listIterator(n);

    // We skip the last one
    iter.previous();
    n--;

    while (iter.hasPrevious() && n > 1) {
      final var info = iter.previous();
      prefix.append(Boolean.TRUE.equals(info) ? EMPTY_INDENT : INDENT);
      n--;
    }

    return prefix.append(suffix).toString();
  }

  private final Adjs<T> adjs;

  private final Repr<T> repr;

  private TreePrinter(Adjs<T> adjs, Repr<T> repr) {
    this.adjs = adjs;
    this.repr = repr;
  }

  private static final String INDENT = "│   ";
  private static final String EMPTY_INDENT = "    ";
  private static final String FOR_CHILD = "├── ";
  private static final String FOR_LAST_CHILD = "└── ";
  private static final String EOL = "\n";
  private static final String EMPTY = "";

}
