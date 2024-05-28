// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos;

import java.util.Objects;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.function.Supplier;

public final class Delay<T> {

  public static <T> Delay<T> of(Supplier<T> supplier) {
    return new Delay<>(supplier);
  }

  public T get() {
    if (lock != null) {
      realize();
    }

    if (exception != null)
      return Exceptions.fail(exception);

    return value;
  }

  public boolean isRealized() {
    return lock == null;
  }

  @SuppressWarnings("squid:S3077")
  private volatile Lock lock;

  private Supplier<T> supplier;

  private T value;

  private Exception exception;

  private Delay(Supplier<T> supplier) {
    lock          = new ReentrantLock();
    this.supplier = Objects.requireNonNull(supplier);
  }

  private void realize() {
    final var l = lock;
    if (l != null) {
      l.lock();
      try {
        if (supplier != null) {
          try {
            value = supplier.get();
          } catch (final Exception e) {
            exception = e;
          }
          supplier = null;
          lock     = null;
        }
      } finally {
        l.unlock();
      }
    }
  }
}
