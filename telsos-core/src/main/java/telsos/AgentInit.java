// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos;

import static telsos.Ch.chSome;

import java.io.File;

import com.sun.tools.attach.VirtualMachine;

public class AgentInit {

  public static Object instrumentation() {
    return instr.deref();
  }

  private static final Delay<Object> instr = Delay.delay(AgentInit::loadAgent);

  private static record VMcloseable(VirtualMachine deref)
      implements AutoCloseable {

    public VMcloseable {
      chSome(deref);
    }

    @Override
    public void close() throws Exception {
      deref.detach();
    }
  }

  public static Object loadAgent() {
    var pid = ProcessHandle.current().pid();
    try (var vm = new VMcloseable(VirtualMachine.attach(String.valueOf(pid)))) {
      var agentFile = new File(
          "../telsos-agent/target/telsos-agent-1.0-SNAPSHOT.jar");
      var agentPath = agentFile.getAbsolutePath();
      vm.deref.loadAgent(agentPath);
      return "AgentInit::loadAgent vm pid: %d and path %s".formatted(pid,
          agentPath);
    } catch (Exception e) {
      throw new TelsosException(e);
    }
  }

  private AgentInit() {}
}
