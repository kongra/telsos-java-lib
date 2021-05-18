package telsos;

import static org.assertj.core.api.Assertions.assertThat;
import static telsos.db.maas.tables.Profiles.PROFILES;

import org.junit.jupiter.api.Test;

import telsos.db.Maas;

class TestArrays {

  @Test
  void test1() {
    assertThat(2 <= 2).isTrue();
    assertThat(2 <= 20).isTrue();
    assertThat(2 <= 1).isFalse();
  }

  @Test
  void test2() {
    Maas.get().inReadCommitted(ctx -> {
      System.out.println("It works");
    });

    var email = Maas.get().inSerializable1(ctx -> {
      var result = ctx.create().select(PROFILES.EMAIL).from(PROFILES)
          .where(PROFILES.ID.eq(1L)).fetch();

      System.out.println(result);
      return result.get(0).get(PROFILES.EMAIL);
    });

    assertThat(email).isEqualTo("kongra@gmail.com");
  }
}
