// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.logging;

import java.util.logging.Logger;

import telsos.processes.ProcessContextInService;

final class LogFactoryForLogger implements LogFactory<Logger> {

  @Override
  public Log create(Logger logger) {
    return new Log() {
      @Override
      public void log(ProcessContextInService serviceProcessCtx, Level level,
          String message) {
        logger.log(asTargetLevel(level), message);
      }

      @Override
      public void log(ProcessContextInService serviceProcessCtx, Level level,
          String message, Throwable throwable) {
        logger.log(asTargetLevel(level), message, throwable);
      }
    };
  }

  private static java.util.logging.Level asTargetLevel(Level level) {
    return switch (level) {
      case TRACE -> java.util.logging.Level.FINEST;
      case DEBUG -> java.util.logging.Level.FINE;
      case INFO  -> java.util.logging.Level.INFO;
      case WARN  -> java.util.logging.Level.WARNING;
      case ERROR -> java.util.logging.Level.SEVERE;
      case FATAL -> java.util.logging.Level.SEVERE;
    };
  }

}
