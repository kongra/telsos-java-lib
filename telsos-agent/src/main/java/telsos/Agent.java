// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos;

import java.lang.instrument.Instrumentation;
import java.util.concurrent.atomic.AtomicReference;

public class Agent {

  public static Instrumentation instrumentation() {
    return premained.get().instr();
  }

  public static void premain(String agentArgs, Instrumentation instr) {
    init(agentArgs, instr);
  }

  public static void agentmain(String agentArgs, Instrumentation instr) {
    init(agentArgs, instr);
  }

  private static void init(String agentArgs, Instrumentation instr) {
    premained.set(new Premained(instr, agentArgs));
  }

  private static record Premained(Instrumentation instr, String agentArgs) {}

  private static final AtomicReference<Premained> premained = new AtomicReference<>();

  private Agent() {}

}
