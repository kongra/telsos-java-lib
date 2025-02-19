package telsos.java.lib;

public final class Stopwatch {

  public static Stopwatch start() {
    return new Stopwatch(System.nanoTime());
  }

  private final long start;

  public long elapsedNanosecs() {
    return System.nanoTime() - start;
  }

  public double elapsedMsecs() {
    return elapsedNanosecs() / 1e6;
  }

  public String elapstr() {
    final var digits = 2;
    return HumanReadable.nanosecs(elapsedNanosecs(), digits);
  }

  @Override
  public String toString() {
    return elapstr();
  }

  private Stopwatch(long start) {
    this.start = start;
  }
}
