package telsos;

import java.lang.instrument.Instrumentation;
import java.util.concurrent.atomic.AtomicReference;

public class InstrumentationAgent {

  private static record Premained(Instrumentation instr, String agentArgs) {
  }

  private static final AtomicReference<Premained> premained = new AtomicReference<>();

  public static void premain(final String agentArgs,
      final Instrumentation instr) {
    premained.set(new Premained(instr, agentArgs));
  }

  public static Instrumentation instrumentation() {
    return premained.get().instr();
  }

  private InstrumentationAgent() {
  }

}
