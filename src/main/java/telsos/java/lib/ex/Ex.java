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

  public static Supplier<ExInfo> info(String message, Object data) {
    return () -> new ExInfo(message, data);
  }

  public static Supplier<ExInfo> info(Supplier<String> messageSupplier) {
    Objects.requireNonNull(messageSupplier);
    return () -> new ExInfo(messageSupplier.get());
  }

  public static Supplier<ExInfo> info(Supplier<String> messageSupplier,
      Object data) {
    Objects.requireNonNull(messageSupplier);
    return () -> new ExInfo(messageSupplier.get(), data);
  }

  public static Supplier<ExInfo> info(String message,
      Supplier<Object> dataSupplier) {
    Objects.requireNonNull(dataSupplier);
    return () -> new ExInfo(message, dataSupplier.get());
  }

  public static Supplier<ExInfo> info(Supplier<String> messageSupplier,
      Supplier<Object> dataSupplier) {
    Objects.requireNonNull(messageSupplier);
    Objects.requireNonNull(dataSupplier);
    return () -> new ExInfo(messageSupplier.get(), dataSupplier.get());
  }

  public static Supplier<Invalid> invalid(String message, Object what) {
    return () -> new Invalid(message, what);
  }

  public static Supplier<Invalid> invalid(Supplier<String> messageSupplier,
      Object what) {
    Objects.requireNonNull(messageSupplier);
    return () -> new Invalid(messageSupplier.get(), what);
  }

  public static Supplier<Invalid> invalid(String message,
      Supplier<Object> whatSupplier) {
    Objects.requireNonNull(whatSupplier);
    return () -> new Invalid(message, whatSupplier.get());
  }

  public static Supplier<Invalid> invalid(Supplier<String> messageSupplier,
      Supplier<Object> whatSupplier) {
    Objects.requireNonNull(messageSupplier);
    Objects.requireNonNull(whatSupplier);
    return () -> new Invalid(messageSupplier.get(), whatSupplier.get());
  }

  @SuppressWarnings("unchecked")
  private static <X extends Throwable> void sneakyThrow0(Throwable t) throws X {
    throw (X) t;
  }

  private Ex() {}

}
