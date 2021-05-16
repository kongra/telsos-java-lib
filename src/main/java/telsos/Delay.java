// Copyright (c) kongra
// Created 18.07.19
package telsos;

import java.util.Objects;
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
  public synchronized T deref() {
    if (supplier != null) {
      try {
        value = supplier.get();
      } catch (Exception t) {
        exception = t;
      }
      supplier = null;
    }

    if (exception != null)
      throw Utils.sneakyThrow(exception);

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

  @Override
  public boolean equals(Object o) {
    if (this == o)
      return true;
    if (o == null || getClass() != o.getClass())
      return false;
    Delay<?> delay = (Delay<?>) o;
    return Objects.equals(deref(), delay.deref());
  }

  @Override
  public int hashCode() {
    return Objects.hash(deref());
  }

}
