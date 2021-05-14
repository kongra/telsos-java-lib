package telsos;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.Test;

import telsos.db.MAAS;

class TestArrays {

  @Test
  void test1() {
    assertThat(2 <= 2).isTrue();
    assertThat(2 <= 20).isTrue();
    assertThat(2 <= 1).isFalse();
  }

  @Test
  void test2() {
    MAAS.get().inReadCommitted(ctx -> {
      System.out.println("It works");
    });
  }

}
