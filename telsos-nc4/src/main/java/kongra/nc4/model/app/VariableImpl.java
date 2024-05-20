// © 2024 Konrad Grzanek <kongra@gmail.com>
package kongra.nc4.model.app;

import kongra.nc4.model.Variable;
import telsos.strings.NonBlank;

record VariableImpl(NonBlank nonBlank) implements Variable {

  @Override
  public String name() {
    return nonBlank.value();
  }
}