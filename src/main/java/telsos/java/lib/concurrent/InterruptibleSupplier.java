package telsos.java.lib.concurrent;

import java.util.function.Supplier;

@FunctionalInterface
public interface InterruptibleSupplier<T> extends Supplier<T> {

  T getInterruptible() throws InterruptedException;

  @Override
  default T get() {
    return Threads.eval(this);
  }

}
