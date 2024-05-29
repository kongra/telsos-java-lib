// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.logging;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import telsos.strings.NonBlank;

class MDCTest {

  MDC mdc;

  @BeforeEach
  void setUp() {
    final var userId = NonBlank.of("userId").orElseThrow();
    final var orderId = NonBlank.of("orderId").orElseThrow();

    final var userIdValue = NonBlank.of("123").orElseThrow();
    final var orderIdValue = NonBlank.of("456").orElseThrow();

    mdc = Logs.forClass().mdcBuilder()
        .put(userId, userIdValue)
        .put(orderId, orderIdValue)
        .build();
  }

  @Test
  void testAsString() {
    assertThat(mdc.asString())
        .isEqualTo("{\"userId\":\"123\",\"orderId\":\"456\"}");
  }

}
