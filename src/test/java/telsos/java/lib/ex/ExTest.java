package telsos.java.lib.ex;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Optional;

import org.junit.jupiter.api.Test;

class ExTest {

  @Test
  void testFail() {
    final int value1 = Optional.ofNullable(256)
        .orElseThrow(Ex.info(() -> "Invalid Stuff"));
    assertThat(value1).isEqualTo(256);

    final int value2 = Optional.ofNullable(365)
        .orElseThrow(Ex.info("Invalid Stuff"));
    assertThat(value2).isEqualTo(365);
  }

}
