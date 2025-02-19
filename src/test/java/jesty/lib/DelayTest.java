package jesty.lib;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

import java.util.concurrent.atomic.AtomicInteger;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import telsos.java.lib.Delay;

class DelayTest {

  static final String TEST_VALUE = "Hello, World!";

  Delay<String> delay;

  Delay<String> delayThrowing;

  AtomicInteger supplierCallsCount;

  @BeforeEach
  void setUp() {
    supplierCallsCount = new AtomicInteger(0);
    delay = Delay.of(() -> {
      supplierCallsCount.incrementAndGet();
      return TEST_VALUE;
    });

    delayThrowing = Delay.of(() -> {
      throw new RuntimeException("Test exception");
    });
  }

  @Test
  void testGet() {
    assertThat(delay.isRealized()).isFalse();
    final var s1 = delay.deref();
    assertThat(s1).isEqualTo(TEST_VALUE);
    assertThat(delay.isRealized()).isTrue();

    final var s2 = delay.deref();
    assertThat(s2).isEqualTo(TEST_VALUE);

    assertThat(supplierCallsCount.get()).isEqualTo(1);
  }

  @Test
  void testGetFailing() {
    assertThat(delayThrowing.isRealized()).isFalse();
    assertThatThrownBy(() -> delayThrowing.deref())
        .isInstanceOf(RuntimeException.class)
        .hasMessage("Test exception");

    assertThat(delayThrowing.isRealized()).isTrue();
    assertThatThrownBy(() -> delayThrowing.deref())
        .isInstanceOf(RuntimeException.class)
        .hasMessage("Test exception");
  }
}
