// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.logging;

import telsos.logging.impl.MDCBuilderImpl;

@FunctionalInterface
public interface LogFactory<T> {

  Log getLog(T arg);

  default MDC.Builder mdcBuilder() {
    return new MDCBuilderImpl();
  }
}
