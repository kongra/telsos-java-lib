package telsos.java.lib.function;

import java.util.function.Supplier;

@FunctionalInterface
public interface Deref<T> extends Supplier<T> {

  T deref();

  @Override
  default T get() {
    return deref();
  }

}
