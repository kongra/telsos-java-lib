// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.logging;

public interface Log {

  void logImpl(Level level, String message);

  void logImpl(Level level, String message, Throwable throwable);

  default void log(Level level, String message) {
    logImpl(level, messageWithMDCPrefix(message));
  }

  default void log(Level level, String message, Throwable throwable) {
    logImpl(level, messageWithMDCPrefix(message), throwable);
  }

  default void log(Level level, String format, Object... arguments) {
    log(level, String.format(format, arguments));
  }

  default void trace(String message) {
    log(Level.TRACE, message);
  }

  default void trace(String message, Throwable throwable) {
    log(Level.TRACE, message, throwable);
  }

  default void trace(String format, Object... arguments) {
    log(Level.TRACE, format, arguments);
  }

  default void debug(String message) {
    log(Level.DEBUG, message);
  }

  default void debug(String message, Throwable throwable) {
    log(Level.DEBUG, message, throwable);
  }

  default void debug(String format, Object... arguments) {
    log(Level.DEBUG, format, arguments);
  }

  default void info(String message) {
    log(Level.INFO, message);
  }

  default void info(String message, Throwable throwable) {
    log(Level.INFO, message, throwable);
  }

  default void info(String format, Object... arguments) {
    log(Level.INFO, format, arguments);
  }

  default void warn(String message) {
    log(Level.WARN, message);
  }

  default void warn(String message, Throwable throwable) {
    log(Level.WARN, message, throwable);
  }

  default void warn(String format, Object... arguments) {
    log(Level.WARN, format, arguments);
  }

  default void error(String message) {
    log(Level.ERROR, message);
  }

  default void error(String message, Throwable throwable) {
    log(Level.ERROR, message, throwable);
  }

  default void error(String format, Object... arguments) {
    log(Level.ERROR, format, arguments);
  }

  default void fatal(String message) {
    log(Level.FATAL, message);
  }

  default void fatal(String message, Throwable throwable) {
    log(Level.FATAL, message, throwable);
  }

  default void fatal(String format, Object... arguments) {
    log(Level.FATAL, format, arguments);
  }

  private static String messageWithMDCPrefix(String message) {
    final var delayedMDC = MDC.delayed.get().orElse(null);
    if (delayedMDC == null)
      return message;

    return delayedMDC.deref().asString() + " " + message;
  }
}
