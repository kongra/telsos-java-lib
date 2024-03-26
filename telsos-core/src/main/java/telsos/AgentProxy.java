// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos;

import java.io.File;
import java.io.IOException;
import java.lang.System.Logger.Level;
import java.lang.instrument.Instrumentation;
import java.lang.reflect.InvocationTargetException;

import com.sun.tools.attach.AgentInitializationException;
import com.sun.tools.attach.AgentLoadException;
import com.sun.tools.attach.AttachNotSupportedException;
import com.sun.tools.attach.VirtualMachine;

public final class AgentProxy {

  public static Instrumentation instrumentation() {
    return Singleton.instance;
  }

  public static long objSize(Object obj) {
    return AgentProxy.instrumentation().getObjectSize(obj);
  }

  private static class Singleton {

    private static final Instrumentation instance;

    static {
      LOG.log(Level.INFO, "AgentProxy::initialize");
      loadAgent();
      instance = getInstrumentation();
    }

    private static Instrumentation getInstrumentation() {
      try {
        final var agentClass = Class.forName("telsos.Agent");
        final var method = agentClass.getMethod("instrumentation",
            (Class<?>[]) null);
        return (Instrumentation) method.invoke(null, (Object[]) null);
      } catch (ClassNotFoundException | NoSuchMethodException
          | SecurityException
          | IllegalAccessException | IllegalArgumentException
          | InvocationTargetException e) {
        throw new TelsosException(e);
      }
    }

    private static void loadAgent() {
      final var pid = ProcessHandle.current().pid();
      final var agentFile = new File(
          "../telsos-agent/target/telsos-agent-1.0.jar");
      final var agentPath = agentFile.getAbsolutePath();

      VirtualMachine jvm = null;
      try {
        jvm = VirtualMachine.attach(String.valueOf(pid));
        jvm.loadAgent(agentPath);
      } catch (IOException | AgentLoadException | AgentInitializationException
          | AttachNotSupportedException e) {
        throw new TelsosException(e);
      } finally {
        if (jvm != null)
          try {
            jvm.detach();
          } catch (final IOException e) {
            LOG.log(Level.ERROR, e);
          }
      }
    }
  }

  private AgentProxy() {}

  private static final System.Logger LOG = System
      .getLogger(AgentProxy.class.getName());
}
