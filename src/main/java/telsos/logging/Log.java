// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.logging;

import telsos.processes.ProcessContextInService;

public interface Log {

  void log(ProcessContextInService processContextInService, Level level,
      String message);

  void log(ProcessContextInService processContextInService, Level level,
      String message,
      Throwable throwable);

  default void log(ProcessContextInService processContextInService, Level level,
      String format,
      Object... arguments) {
    log(processContextInService, level, String.format(format, arguments));
  }

  default void trace(ProcessContextInService processContextInService,
      String message) {
    log(processContextInService, Level.TRACE, message);
  }

  default void trace(ProcessContextInService processContextInService,
      String message,
      Throwable throwable) {
    log(processContextInService, Level.TRACE, message, throwable);
  }

  default void trace(ProcessContextInService processContextInService,
      String format,
      Object... arguments) {
    log(processContextInService, Level.TRACE, format, arguments);
  }

  default void debug(ProcessContextInService processContextInService,
      String message) {
    log(processContextInService, Level.DEBUG, message);
  }

  default void debug(ProcessContextInService processContextInService,
      String message,
      Throwable throwable) {
    log(processContextInService, Level.DEBUG, message, throwable);
  }

  default void debug(ProcessContextInService processContextInService,
      String format,
      Object... arguments) {
    log(processContextInService, Level.DEBUG, format, arguments);
  }

  default void info(ProcessContextInService processContextInService,
      String message) {
    log(processContextInService, Level.INFO, message);
  }

  default void info(ProcessContextInService processContextInService,
      String message,
      Throwable throwable) {
    log(processContextInService, Level.INFO, message, throwable);
  }

  default void info(ProcessContextInService processContextInService,
      String format,
      Object... arguments) {
    log(processContextInService, Level.INFO, format, arguments);
  }

  default void warn(ProcessContextInService processContextInService,
      String message) {
    log(processContextInService, Level.WARN, message);
  }

  default void warn(ProcessContextInService processContextInService,
      String message,
      Throwable throwable) {
    log(processContextInService, Level.WARN, message, throwable);
  }

  default void warn(ProcessContextInService processContextInService,
      String format,
      Object... arguments) {
    log(processContextInService, Level.WARN, format, arguments);
  }

  default void error(ProcessContextInService processContextInService,
      String message) {
    log(processContextInService, Level.ERROR, message);
  }

  default void error(ProcessContextInService processContextInService,
      String message,
      Throwable throwable) {
    log(processContextInService, Level.ERROR, message, throwable);
  }

  default void error(ProcessContextInService processContextInService,
      String format,
      Object... arguments) {
    log(processContextInService, Level.ERROR, format, arguments);
  }

  default void fatal(ProcessContextInService processContextInService,
      String message) {
    log(processContextInService, Level.FATAL, message);
  }

  default void fatal(ProcessContextInService processContextInService,
      String message,
      Throwable throwable) {
    log(processContextInService, Level.FATAL, message, throwable);
  }

  default void fatal(ProcessContextInService processContextInService,
      String format,
      Object... arguments) {
    log(processContextInService, Level.FATAL, format, arguments);
  }
}
