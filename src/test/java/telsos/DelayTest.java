// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class DelayTest {

  static final String TEST_VALUE = "Hello, World!";

  Delay<String> delay;

  Delay<String> delayThrowing;

  @BeforeEach
  void setUp() {
    delay         = Delay.of(() -> TEST_VALUE);
    delayThrowing = Delay.of(() -> {
                    throw new RuntimeException("Test exception");
                  });
  }

  @Test
  void testGet() {
    assertThat(delay.isRealized()).isFalse();
    final var s = delay.get();
    assertThat(s).isEqualTo(TEST_VALUE);
    assertThat(delay.isRealized()).isTrue();
  }

  @Test
  void testGetFailing() {
    assertThat(delayThrowing.isRealized()).isFalse();
    assertThatThrownBy(() -> delayThrowing.get())
        .isInstanceOf(RuntimeException.class)
        .hasMessage("Test exception");

    assertThat(delayThrowing.isRealized()).isTrue();
    assertThatThrownBy(() -> delayThrowing.get())
        .isInstanceOf(RuntimeException.class)
        .hasMessage("Test exception");
  }
}
