// © 2024 Konrad Grzanek <kongra@gmail.com>
package kongra.nc4.model;

import telsos.strings.NonBlank;

public interface ModelFactory {

  Variable createVariable(NonBlank name);

  Value createValue(NonBlank name);

}
