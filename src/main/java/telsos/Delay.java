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
    final Supplier<T> supplier;
    synchronized (this) {
      if (!isPending()) {
        if (this.exception != null)
          throw Utils.sneakyThrow(this.exception);

        return this.value;
      } else {
        supplier = this.supplier;
      }
    }

    T value = null;
    Exception exception = null;
    try {
      value = supplier.get();
    } catch (Exception t) {
      exception = t;
    }

    synchronized (this) {
      this.supplier = null;
      this.value = value;
      this.exception = exception;
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

  // @Override
  // public boolean equals(Object o) {
  // if (this == o)
  // return true;
  // if (o == null || getClass() != o.getClass())
  // return false;
  // Delay<?> delay = (Delay<?>) o;
  // return Objects.equals(deref(), delay.deref());
  // }
  //
  // @Override
  // public int hashCode() {
  // return Objects.hash(deref());
  // }

}
