package telsos.java.lib.ex;

import java.util.Objects;
import java.util.concurrent.Callable;
import java.util.function.Supplier;

public final class Ex {

  public static <T> T evalUnchecked(Callable<T> body) {
    try {
      return body.call();
    } catch (final Exception e) {
      return rethrow(e);
    }
  }

  public static <T> T rethrow(Throwable t) {
    sneakyThrow0(t);
    return null;
  }

  public static Supplier<ExInfo> info(String message) {
    return () -> new ExInfo(message);
  }

  public static Supplier<ExInfo> info(
      Supplier<String> messageSupplier) {
    Objects.requireNonNull(messageSupplier);
    return () -> new ExInfo(messageSupplier.get());
  }

  @SuppressWarnings("unchecked")
  private static <X extends Throwable> void sneakyThrow0(Throwable t) throws X {
    throw (X) t;
  }

  private Ex() {}

}
