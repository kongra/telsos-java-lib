// © 2024 Konrad Grzanek <kongra@gmail.com>
package kongra.nc4.model;

import java.util.function.ToIntBiFunction;
import java.util.function.ToIntFunction;

public interface LeximapBuilder {

  LeximapBuilder add(Variable variable, Value value);

  LeximapBuilder add(String variable, String value);

  Leximap build();

  Leximap buildOrdered(
      ToIntFunction<Variable> variablesOrdering,
      ToIntBiFunction<Variable, Value> maybeVariable2DomainOrdering);

}
