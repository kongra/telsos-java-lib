package telsos;

import static org.assertj.core.api.Assertions.assertThat;
import static telsos.db.Dbs.maas;
import static telsos.db.maas.tables.Profiles.PROFILES;

import org.junit.jupiter.api.Test;

class TestArrays {

  @Test
  void test1() {
    maas.inReadCommitted(ctx -> {
      System.out.println("It works");
    });

    var email = maas.inSerializable1(ctx -> {
      var result = ctx.create().select(PROFILES.EMAIL).from(PROFILES)
          .where(PROFILES.ID.eq(1L)).fetch();

      System.out.println(result);
      return result.get(0).get(PROFILES.EMAIL);
    });

    assertThat(email).isEqualTo("kongra@gmail.com");
  }
}
