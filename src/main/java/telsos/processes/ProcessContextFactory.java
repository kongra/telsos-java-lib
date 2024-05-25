// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.processes;

import java.time.Instant;

public interface ProcessContextFactory {

  ProcessContext create(ProcessId processId, Instant startInstant);

  ProcessContextInService create(ProcessId processId, Instant startInstant,
      Service service, Instant instant);

  default ProcessContextInService create(ProcessContext processContext,
      Service service, Instant instant) {
    return create(processContext.processId(), processContext.startInstant(),
        service, instant);
  }

  default ProcessContextInService create(ProcessContext processContext,
      Service service) {
    return create(processContext.processId(), processContext.startInstant(),
        service, Instant.now());
  }

}
