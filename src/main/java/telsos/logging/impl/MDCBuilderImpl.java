// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.logging.impl;

import org.eclipse.collections.api.factory.Maps;
import org.eclipse.collections.api.map.MutableMap;

import telsos.AbstractSingleUseBuilder;
import telsos.functions.EntryConsumer;
import telsos.logging.MDC;
import telsos.strings.NonBlank;

public class MDCBuilderImpl extends AbstractSingleUseBuilder<MDC>
    implements MDC.Builder {

  private final MutableMap<NonBlank, NonBlank> entries = Maps.mutable.empty();

  @Override
  public void putImpl(NonBlank key, NonBlank value) {
    entries.put(key, value);
  }

  @Override
  protected MDC buildImpl() {
    if (entries.isEmpty())
      throw new IllegalArgumentException("MDC must have at least one entry");

    // Because the builder is single-use, we can safely use the entries map in
    // this::forEach.
    return this::forEach;
  }

  private void forEach(EntryConsumer<NonBlank, NonBlank> action) {
    final var it = entries.entrySet().iterator();
    while (it.hasNext()) {
      final var entry = it.next();
      final var isLast = !it.hasNext();
      action.accept(entry.getKey(), entry.getValue(), isLast);
    }
  }
}
