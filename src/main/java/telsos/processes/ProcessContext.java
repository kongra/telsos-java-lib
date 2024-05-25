// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.processes;

import java.time.Instant;

public interface ProcessContext {

  ProcessId processId();

  Instant startInstant();

}
