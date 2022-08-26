// © 2019 Konrad Grzanek <kongra@gmail.com>
package telsos;

import java.util.Objects;

public final class Utils {

  /**
   * Throw even checked exceptions without being required to declare them or
   * catch them. Suggested idiom:
   * <p>
   * <code>throw sneakyThrow( some exception );</code>
   */
  public static TelsosException sneakyThrow(Throwable t) {
    // http://www.mail-archive.com/javaposse@googlegroups.com/msg05984.html
    Objects.requireNonNull(t);
    Utils.sneakyThrow0(t);
    return new TelsosException();
  }

  public static <T> T fail(Throwable t) {
    Utils.sneakyThrow(t);
    return null;
  }

  @SuppressWarnings("unchecked")
  private static <T extends Throwable> void sneakyThrow0(Throwable t) throws T {
    throw (T) t;
  }

  public static String room() {
    final var rt = Runtime.getRuntime();
    final var free = rt.freeMemory();
    final var total = rt.totalMemory();
    final var mx = rt.maxMemory();
    final var used = total - free;
    final var digits = 2;

    return "Used: %s | Free: %s | Total :%s | Max: %s".formatted(
        HumanReadable.bytes(used, digits), HumanReadable.bytes(free, digits),
        HumanReadable.bytes(total, digits), HumanReadable.bytes(mx, digits));
  }

  public static long stopwatch() {
    return System.nanoTime();
  }

  public static long elapsedNanosecs(long swatch) {
    return System.nanoTime() - swatch;
  }

  public static double elapsedMsecs(long swatch) {
    return elapsedNanosecs(swatch) / 1e6;
  }

  public static String elapstr(long swatch) {
    final var digits = 2;
    return HumanReadable.nanosecs(elapsedNanosecs(swatch), digits);
  }

  public static long objSize(Object obj) {
    return AgentProxy.instrumentation().getObjectSize(obj);
  }

  public static boolean areEqual(double x, double y, double epsilon) {
    return Math.abs(x - y) < epsilon;
  }

  public static final double EPSILON = 0.0000000000001D;

  public static boolean isEffectivelyInt(double d) {
    return areEqual(d, Math.rint(d), EPSILON);
  }

  private Utils() {}
}
