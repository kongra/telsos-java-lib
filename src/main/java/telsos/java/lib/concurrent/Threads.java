package telsos.java.lib.concurrent;

import java.time.Duration;
import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.Lock;

import telsos.java.lib.ex.Ex;

public final class Threads {

  public static void run(InterruptibleRunnable body) {
    try {
      body.runInterruptible();
    } catch (final InterruptedException e) {
      Thread.currentThread().interrupt();
      Ex.rethrow(e);
    }
  }

  public static <T> T eval(InterruptibleSupplier<T> supplier) {
    try {
      return supplier.getInterruptible();
    } catch (final InterruptedException e) {
      Thread.currentThread().interrupt();
      return Ex.rethrow(e);
    }
  }

  public static void runAcquiring(Semaphore s, InterruptibleRunnable body) {
    runAcquiring(s, 1, body);
  }

  public static void runAcquiring(Semaphore s, int permits,
      InterruptibleRunnable body) {
    run(() -> s.acquire(permits));
    try {
      body.run();
    } finally {
      s.release(permits);
    }
  }

  public static <T> T evalAcquiring(Semaphore s,
      InterruptibleSupplier<T> supplier) {
    return evalAcquiring(s, 1, supplier);
  }

  public static <T> T evalAcquiring(Semaphore s, int permits,
      InterruptibleSupplier<T> body) {
    run(() -> s.acquire(permits));
    try {
      return body.get();
    } finally {
      s.release(permits);
    }
  }

  public static void runLocking(Lock lock, InterruptibleRunnable body) {
    lock.lock();
    try {
      run(body);
    } finally {
      lock.unlock();
    }
  }

  public static <T> T evalLocking(Lock lock,
      InterruptibleSupplier<T> supplier) {
    lock.lock();
    try {
      return supplier.get();
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