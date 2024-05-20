// © 2024 Konrad Grzanek <kongra@gmail.com>
package kongra.nc4.model.app;

import java.util.Objects;

import kongra.nc4.model.ModelFactory;
import kongra.nc4.model.Value;
import kongra.nc4.model.Variable;
import telsos.strings.NonBlank;

final class ModelFactoryImpl implements ModelFactory {

  @Override
  public Variable createVariable(NonBlank name) {
    Objects.requireNonNull(name);
    return new VariableImpl(name);
  }

  @Override
  public Value createValue(NonBlank name) {
    Objects.requireNonNull(name);
    return new ValueImpl(name);
  }
}
