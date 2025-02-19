package telsos.java.lib.logging;

import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.function.Supplier;

import telsos.java.lib.Delay;
import telsos.java.lib.DynVar;
import telsos.java.lib.function.EntryConsumer;
import telsos.java.lib.string.NonBlank;
import telsos.java.lib.string.Str;

@FunctionalInterface
public interface MDC {

  DynVar<Delay<MDC>> delayed = DynVar.newInstance();

  static void exec(Supplier<MDC> supplier, Runnable body) {
    Objects.requireNonNull(supplier);
    Objects.requireNonNull(body);
    delayed.exec(Delay.of(supplier), body);
  }

  static <E> E eval(Supplier<MDC> supplier, Callable<E> body) {
    Objects.requireNonNull(supplier);
    Objects.requireNonNull(body);
    return delayed.eval(Delay.of(supplier), body);
  }

  void forEach(EntryConsumer<NonBlank, NonBlank> action);

  default String asString() {
    final var buf = new StringBuilder("{");
    forEach((key, val, isLast) -> {
      buf.append(Str.wrapInQuotes(key.value()));
      buf.append(':');
      buf.append(Str.wrapInQuotes(val.value()));
      if (!isLast) {
        buf.append(",");
      }
    });

    return buf.append('}').toString();
  }

}
