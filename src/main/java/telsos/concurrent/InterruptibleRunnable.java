// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.concurrent;

@FunctionalInterface
public interface InterruptibleRunnable extends Runnable {

  void runInterruptible() throws InterruptedException;

  @Override
  default void run() {
    Threads.run(this);
  }

}