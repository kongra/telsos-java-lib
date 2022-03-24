// Copyright (c) kongra
// Created 18.07.19
package telsos;

public final class Utils {

  /**
   * Throw even checked exceptions without being required to declare them or
   * catch them. Suggested idiom:
   * <p>
   * <code>throw sneakyThrow( some exception );</code>
   */
  public static TelsosException sneakyThrow(Throwable t) {
    // http://www.mail-archive.com/javaposse@googlegroups.com/msg05984.html
    if (t == null)
      throw new NullPointerException();
    Utils.sneakyThrow0(t);
    return new TelsosException();
  }

  @SuppressWarnings("unchecked")
  private static <T extends Throwable> void sneakyThrow0(Throwable t) throws T {
    throw (T) t;
  }

  public static String room() {
    var rt = Runtime.getRuntime();
    var free = rt.freeMemory();
    var total = rt.totalMemory();
    var mx = rt.maxMemory();
    var used = total - free;
    var digits = 2;

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
    return HumanReadable.nanosecs(elapsedNanosecs(swatch), 2);
  }

  private Utils() {
  }
}
