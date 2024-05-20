// © 2024 Konrad Grzanek <kongra@gmail.com>
package kongra.nc4.model.app;

import kongra.nc4.model.Value;
import telsos.strings.NonBlank;

record ValueImpl(NonBlank nonBlank) implements Value {

  @Override
  public String name() {
    return nonBlank.value();
  }
}