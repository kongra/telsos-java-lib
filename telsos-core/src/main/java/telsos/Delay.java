// Copyright (c) kongra
// Created 18.07.19
package telsos;

import java.util.function.Supplier;

public final class Delay<T> implements Supplier<T>, Deref<T>, Pending {

  private T value;

  private Exception exception;

  private Supplier<T> supplier;

  private Delay(Supplier<T> supplier) {
    this.supplier = supplier;
  }

  public static <T> Delay<T> delay(Supplier<T> supplier) {
    return new Delay<>(supplier);
  }

  @Override
  public T deref() {
    final Supplier<T> supplier1;
    synchronized (this) {
      if (!isPending()) {
        if (this.exception != null)
          throw Utils.sneakyThrow(this.exception);

        return this.value;
      }
      supplier1 = this.supplier;
    }

    T value1 = null;
    Exception exception1 = null;
    try {
      value1 = supplier1.get();
    } catch (Exception t) {
      exception1 = t;
    }

    synchronized (this) {
      this.supplier = null;
      this.value = value1;
      this.exception = exception1;
    }

    return value;
  }

  @Override
  public T get() {
    return deref();
  }

  @Override
  public synchronized boolean isPending() {
    return supplier != null;
  }

  @Override
  public synchronized String toString() {
    return "Delay{" + "pending=" + isPending() + ", value="
        + (isPending() ? "..." : String.valueOf(deref())) + '}';
  }

}
