// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.logging;

import java.util.logging.Logger;

import telsos.logging.impl.LogFactoryForLogger;

public final class Logs {

  private static final LogFactory<Logger> forLogger = new LogFactoryForLogger();

  public static LogFactory<Logger> forLogger() {
    return forLogger;
  }

  public static LogFactory<String> forName() {
    return name -> forLogger().getLog(Logger.getLogger(name));
  }

  public static LogFactory<Class<?>> forClass() {
    return c -> forName().getLog(c.getName());
  }

  private Logs() {
    throw new UnsupportedOperationException(
        "This is a utility class and cannot be instantiated");
  }

}
