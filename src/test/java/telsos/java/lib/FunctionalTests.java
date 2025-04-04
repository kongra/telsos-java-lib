package telsos.java.lib;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.function.Function;

import org.junit.jupiter.api.Test;

class FunctionalTests {

  @Test
  void test() {
    final Function<Double, Double> square = x -> x * x;
    final double y = square.apply(5.0);
    assertThat(y).isEqualTo(25);
  }

}
