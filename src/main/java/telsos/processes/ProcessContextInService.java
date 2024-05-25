// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.processes;

import java.time.Instant;

public interface ProcessContextInService extends ProcessContext {

  Service service();

  Instant instant();

}
