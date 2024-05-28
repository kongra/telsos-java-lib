// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.logging;

import telsos.functions.EntryConsumer;
import telsos.strings.NonBlank;

public interface MDC {

  void forEach(EntryConsumer<NonBlank, NonBlank> action);

  default String asString() {
    final var buf = new StringBuilder("{");
    forEach((key, value, isLast) -> {
      buf.append(key); buf.append('='); buf.append(value);
      if (!isLast) {
        buf.append(",");
      }
    });

    buf.append('}');
    return buf.toString();
  }

}
