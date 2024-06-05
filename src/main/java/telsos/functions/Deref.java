// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.functions;

import java.util.function.Supplier;

@FunctionalInterface
public interface Deref<T> extends Supplier<T> {

  T deref();

  @Override
  default T get() {
    return deref();
  }

}
