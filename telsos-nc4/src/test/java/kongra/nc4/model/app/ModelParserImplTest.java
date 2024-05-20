// © 2024 Konrad Grzanek <kongra@gmail.com>
package kongra.nc4.model.app;

import static org.assertj.core.api.Assertions.assertThat;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import kongra.nc4.model.ModelFactory;
import kongra.nc4.model.ModelParser;
import telsos.strings.NonBlank;

class ModelParserImplTest {

  ModelFactory modelFactory;

  ModelParser modelParser;

  @BeforeEach
  void setUp() throws Exception {
    modelFactory = new ModelFactoryImpl();
    modelParser  = new ModelParserImpl(modelFactory);
  }

  @Test
  void testCreateVariable() {
    final var variable1 = modelParser.parseVariable("variable1");
    final var variable2 = modelParser.parseVariable("         ");

    assertThat(variable1).isNotEmpty();
    assertThat(variable2).isEmpty();

    assertThat(variable1.get().name()).isEqualTo("variable1");

    assertThrows(NullPointerException.class,
        () -> modelParser.parseVariable((String) null));

    assertThrows(NullPointerException.class,
        () -> modelFactory.createVariable((NonBlank) null));
  }

  @Test
  void testCreateValue() {
    final var value1 = modelParser.parseValue("value1");
    final var value2 = modelParser.parseValue("      ");

    assertThat(value1).isNotEmpty();
    assertThat(value2).isEmpty();

    assertThat(value1.get().name()).isEqualTo("value1");

    assertThrows(NullPointerException.class,
        () -> modelParser.parseValue((String) null));

    assertThrows(NullPointerException.class,
        () -> modelFactory.createValue((NonBlank) null));
  }

}
