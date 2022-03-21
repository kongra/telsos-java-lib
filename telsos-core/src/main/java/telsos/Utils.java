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

  private Utils() {
  }
}
