// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.concurrent;

import java.time.Duration;
import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.Lock;

public final class Threads {

  @FunctionalInterface
  public interface RunnableThrowingInterrupted {

    void run() throws InterruptedException;

  }

  public static void run(RunnableThrowingInterrupted body) {
    try {
      body.run();
    } catch (final InterruptedException e) {
      e.printStackTrace();
      Thread.currentThread().interrupt();
    }
  }

  public static void acquiring(Semaphore s, Runnable body) {
    acquiring(s, 1, body);
  }

  public static void acquiring(Semaphore s, int permits,
      Runnable body) {
    run(() -> s.acquire(permits));
    try {
      body.run();
    } finally {
      s.release(permits);
    }
  }

  public static void locking(Lock lock, Runnable body) {
    lock.lock();
    try {
      body.run();
    } finally {
      lock.unlock();
    }
  }

  public static void locking(Lock lock, RunnableThrowingInterrupted body) {
    lock.lock();
    try {
      run(body);
    } finally {
      lock.unlock();
    }
  }

  public static void sleep(long millis) {
    run(() -> Thread.sleep(millis));
  }

  public static void sleep(Duration duration) {
    run(() -> Thread.sleep(duration));
  }

  public static void joinAll(Iterable<Thread> threads) {
    for (final var thread : threads) {
      run(thread::join);
    }
  }

  private Threads() {
    throw new UnsupportedOperationException();
  }
}