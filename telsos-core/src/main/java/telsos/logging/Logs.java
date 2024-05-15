// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.logging;

import java.util.logging.Logger;

public final class Logs {

  private static final LogFactory<Logger> forLogger = new LogFactoryForLogger();

  public static LogFactory<Logger> forLogger() {
    return forLogger;
  }

  public static LogFactory<String> forName() {
    return name -> forLogger().create(Logger.getLogger(name));
  }

  public static LogFactory<Class<?>> forClass() {
    return c -> forName().create(c.getName());
  }

  private Logs() {
    throw new UnsupportedOperationException(
        "This is a utility class and cannot be instantiated");
  }

}
