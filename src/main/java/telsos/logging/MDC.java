// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.logging;

import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.function.Supplier;

import telsos.Delay;
import telsos.DynVar;
import telsos.functions.EntryConsumer;
import telsos.strings.NonBlank;
import telsos.strings.Str;

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

  interface Builder {

    default Builder put(NonBlank key, NonBlank value) {
      putImpl(Objects.requireNonNull(key), Objects.requireNonNull(value));
      return this;
    }

    void putImpl(NonBlank key, NonBlank value);

    MDC build();
  }
}
