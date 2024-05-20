// © 2024 Konrad Grzanek <kongra@gmail.com>
package kongra.nc4.model.app;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import kongra.nc4.model.LeximapBuilder;
import kongra.nc4.model.ModelFactory;
import kongra.nc4.model.ModelParser;

class LeximapBuilderImplTest {

  ModelFactory modelFactory;

  ModelParser modelParser;

  LeximapBuilder leximapBuilder;

  @BeforeEach
  void setUp() throws Exception {
    modelFactory   = new ModelFactoryImpl();
    modelParser    = new ModelParserImpl(modelFactory);
    leximapBuilder = new LeximapBuilderImpl(modelParser);
  }

  @Test
  void testAdd() {
    // TODO: implement
  }

  @Test
  void testBuild() {
    // TODO: implement
  }

  @Test
  void testBuildOrdered() {
    // TODO: implement
  }

}
