package telsos;

import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.function.Supplier;

public final class Exceptions {

  public static <T> T evalNothrowing(Callable<T> body) {
    try {
      return body.call();
    } catch (final Exception e) {
      throw new TelsosException(e);
    }
  }

  /**
   * Throw even checked exceptions without being required to declare them or
   * catch them. Suggested idiom:
   * <p>
   * <code>throw sneakyThrow( some exception );</code>
   */
  public static TelsosException sneakyThrow(Throwable t) {
    // http://www.mail-archive.com/javaposse@googlegroups.com/msg05984.html
    Objects.requireNonNull(t);
    sneakyThrow0(t);
    return new TelsosException();
  }

  public static <T> T fail(Throwable t) {
    sneakyThrow(t);
    return null;
  }

  @SuppressWarnings("unchecked")
  private static <T extends Throwable> void sneakyThrow0(Throwable t) throws T {
    throw (T) t;
  }

  public static final Supplier<RuntimeException> IMPOSSIBLE = ImpossibleException::new;

  private Exceptions() {}

}
