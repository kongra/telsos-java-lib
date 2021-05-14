package telsos;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

class TestArrays {

  @Test
  void test() {
    assertThat(2 <= 2).isTrue();
    assertThat(2 <= 20).isTrue();
    assertThat(2 <= 1).isFalse();
  }

}
