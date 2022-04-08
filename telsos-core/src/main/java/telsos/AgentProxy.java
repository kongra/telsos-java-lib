// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos;

import java.io.File;
import java.io.IOException;
import java.lang.instrument.Instrumentation;
import java.lang.reflect.InvocationTargetException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.tools.attach.VirtualMachine;

public class AgentProxy {

  private static final Logger LOG = LoggerFactory.getLogger(AgentProxy.class);

  public static Instrumentation instrumentation() {
    return instr.deref();
  }

  private static final Delay<Instrumentation> instr = Delay
      .delay(AgentProxy::initialize);

  private static Instrumentation initialize() {
    LOG.info("AgentProxy::initialize");
    loadAgent();
    return getInstrumentation();
  }

  private static Instrumentation getInstrumentation() {
    try {
      var agentClass = Class.forName("telsos.Agent");
      var method = agentClass.getMethod("instrumentation", (Class<?>[]) null);
      return (Instrumentation) method.invoke(null, (Object[]) null);
    } catch (ClassNotFoundException | NoSuchMethodException | SecurityException
        | IllegalAccessException | IllegalArgumentException
        | InvocationTargetException e) {
      throw new TelsosException(e);
    }
  }

  private static void loadAgent() {
    var pid = ProcessHandle.current().pid();
    var agentFile = new File(
        "../telsos-agent/target/telsos-agent-1.0.jar");
    var agentPath = agentFile.getAbsolutePath();

    VirtualMachine jvm = null;
    try {
      jvm = VirtualMachine.attach(String.valueOf(pid));
      jvm.loadAgent(agentPath);
    } catch (Exception e) {
      throw new TelsosException(e);
    } finally {
      if (jvm != null) {
        try {
          jvm.detach();
        } catch (IOException e) {
          LOG.error(e.getMessage(), e);
        }
      }
    }
  }

  private AgentProxy() {}
}
