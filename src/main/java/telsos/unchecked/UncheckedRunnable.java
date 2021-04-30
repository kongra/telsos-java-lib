// Copyright (c) Konrad Grzanek
// Created 19.07.19
package telsos.unchecked;

public interface UncheckedRunnable extends Runnable {

  static void unchecked(UncheckedRunnable body) {
    body.run();
  }

  void runThrowing() throws Throwable;

  @Override
  default void run() {
    try {
      runThrowing();
    } catch (Throwable e) {
      throw new RuntimeException(e);
    }
  }

}
