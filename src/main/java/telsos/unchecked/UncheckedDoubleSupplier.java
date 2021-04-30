// Copyright (c) Konrad Grzanek
// Created 2019-07-22
package telsos.unchecked;

import java.util.function.DoubleSupplier;

@FunctionalInterface
public interface UncheckedDoubleSupplier extends DoubleSupplier {

  static double unchecked(UncheckedDoubleSupplier supplier) {
    return supplier.getAsDouble();
  }

  double getAsDoubleThrowing() throws Throwable;

  @Override
  default double getAsDouble() {
    try {
      return getAsDoubleThrowing();
    } catch (Throwable e) {
      throw new RuntimeException(e);
    }
  }
}
