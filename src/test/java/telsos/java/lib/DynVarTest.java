package telsos.java.lib;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DynVarTest {

  static final ScopedValue<String> SCOPED_VALUE = ScopedValue
      .newInstance();

  DynVar<String> dynVar;

  @BeforeEach
  void setUp() {
    dynVar = DynVar.of(SCOPED_VALUE);
  }

  @Test
  void testExec() {
    assertThat(dynVar.get()).isEmpty();

    dynVar.exec("value-1", () -> {
      assertThat(dynVar.get().orElseThrow()).isEqualTo("value-1");

      dynVar.exec("value-2", () -> {
        assertThat(dynVar.get().orElseThrow()).isEqualTo("value-2");
      });

      assertThat(dynVar.get().orElseThrow()).isEqualTo("value-1");
    });
  }

  @Test
  void testEval() {
    assertThat(dynVar.get()).isEmpty();

    final var result = dynVar.eval("value-1", () -> {
      assertThat(dynVar.get().orElseThrow()).isEqualTo("value-1");

      return dynVar.eval("value-2", () -> {
        assertThat(dynVar.get().orElseThrow()).isEqualTo("value-2");
        return "result";
      });

    });

    assertThat(result).isEqualTo("result");
  }

  @Test
  void testGet() {
    assertThat(dynVar.get()).isEmpty();
    assertThat(dynVar.get("default-value")).isEqualTo("default-value");
    assertThat(dynVar.get(() -> "supplied-default-value"))
        .isEqualTo("supplied-default-value");
  }

}
