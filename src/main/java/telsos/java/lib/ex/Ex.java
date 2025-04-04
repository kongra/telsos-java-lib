package telsos.java.lib.ex;

import java.util.concurrent.Callable;
import java.util.function.Supplier;

import telsos.java.lib.O;

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
    O.nn(messageSupplier);
    return () -> new ExInfo(messageSupplier.get());
  }

  public static Supplier<ExInfo> info(Supplier<String> messageSupplier,
      Object data) {
    O.nn(messageSupplier);
    return () -> new ExInfo(messageSupplier.get(), data);
  }

  public static Supplier<ExInfo> info(String message,
      Supplier<Object> dataSupplier) {
    O.nn(dataSupplier);
    return () -> new ExInfo(message, dataSupplier.get());
  }

  public static Supplier<ExInfo> info(Supplier<String> messageSupplier,
      Supplier<Object> dataSupplier) {
    O.nn(messageSupplier);
    O.nn(dataSupplier);
    return () -> new ExInfo(messageSupplier.get(), dataSupplier.get());
  }

  public static Supplier<Invalid> invalid(String message, Object what) {
    return () -> new Invalid(message, what);
  }

  public static Supplier<Invalid> invalid(Supplier<String> messageSupplier,
      Object what) {
    O.nn(messageSupplier);
    return () -> new Invalid(messageSupplier.get(), what);
  }

  public static Supplier<Invalid> invalid(String message,
      Supplier<Object> whatSupplier) {
    O.nn(whatSupplier);
    return () -> new Invalid(message, whatSupplier.get());
  }

  public static Supplier<Invalid> invalid(Supplier<String> messageSupplier,
      Supplier<Object> whatSupplier) {
    O.nn(messageSupplier);
    O.nn(whatSupplier);
    return () -> new Invalid(messageSupplier.get(), whatSupplier.get());
  }

  public static Supplier<Impossible> impossible(String message) {
    return () -> new Impossible(message);
  }

  public static Supplier<Impossible> impossible(
      Supplier<String> messageSupplier) {
    O.nn(messageSupplier);
    return () -> new Impossible(messageSupplier.get());
  }

  @SuppressWarnings("unchecked")
  private static <X extends Throwable> void sneakyThrow0(Throwable t) throws X {
    throw (X) t;
  }

  private Ex() {}

}
