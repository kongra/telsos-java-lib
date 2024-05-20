// © 2024 Konrad Grzanek <kongra@gmail.com>
package kongra.nc4.model;

import java.util.Objects;
import java.util.Optional;

import telsos.strings.NonBlank;

public interface ModelParser {

  ModelFactory modelFactory();

  default Optional<Variable> parseVariable(String variableName) {
    Objects.requireNonNull(variableName);
    return NonBlank.of(variableName)
        .map(nonBlankName -> modelFactory().createVariable(nonBlankName));
  }

  default Optional<Value> parseValue(String valueName) {
    Objects.requireNonNull(valueName);
    return NonBlank.of(valueName)
        .map(nonBlankName -> modelFactory().createValue(nonBlankName));
  }

}
