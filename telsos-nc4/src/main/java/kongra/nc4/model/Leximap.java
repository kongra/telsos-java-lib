// © 2024 Konrad Grzanek <kongra@gmail.com>
package kongra.nc4.model;

import java.util.stream.Stream;

public interface Leximap {

  Stream<Variable> variables();

  Stream<Value> valuesOfAllVariables();

  Stream<Value> valuesOf(Variable variable);

}
