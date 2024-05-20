// © 2024 Konrad Grzanek <kongra@gmail.com>
package kongra.nc4.model.app;

import java.util.Objects;

import kongra.nc4.model.ModelFactory;
import kongra.nc4.model.ModelParser;

class ModelParserImpl implements ModelParser {

  private final ModelFactory modelFactory;

  ModelParserImpl(ModelFactory modelFactory) {
    this.modelFactory = Objects.requireNonNull(modelFactory);
  }

  @Override
  public ModelFactory modelFactory() {
    return modelFactory;
  }

}
