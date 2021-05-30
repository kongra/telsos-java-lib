package telsos.profile;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;
import static org.assertj.core.api.Assertions.assertThatNullPointerException;

import org.junit.jupiter.api.Test;

import telsos.ChError;

class TestProfile {

  @Test
  void testProfileConstructor1() {
    var profile = new Profile();

    // Email
    assertThatNullPointerException().isThrownBy(() -> profile.getEmail());
    profile.setEmail("kongra@gmail.com");
    assertThat(profile.getEmail()).isEqualTo("kongra@gmail.com");

    // Name
    assertThatNullPointerException().isThrownBy(() -> profile.getName());
    assertThatExceptionOfType(ChError.class).isThrownBy(() -> {
      profile.setName("   ");
    });
    profile.setName("Konrad Grzanek");
    assertThat(profile.getName()).isEqualTo("Konrad Grzanek");

    // Id
    assertThatExceptionOfType(ChError.class).isThrownBy(() -> {
      profile.getId();
    });
    assertThatExceptionOfType(ChError.class).isThrownBy(() -> {
      profile.hashCode();
    });

    var otherProfile = new Profile();
    assertThatExceptionOfType(ChError.class).isThrownBy(() -> {
      profile.equals(otherProfile);
    });
    assertThatExceptionOfType(ChError.class).isThrownBy(() -> {
      profile.setId(-1);
    });

    profile.setId(0);
    profile.setId(1);
    assertThat(profile.getId()).isEqualTo(1);
  }

  @Test
  void testProfileConstructor3() {
    assertThatExceptionOfType(ChError.class).isThrownBy(() -> {
      @SuppressWarnings("unused")
      var profile = new Profile(-1, "kongra@gmail.com", "Konrad Grzanek");
    });
    assertThatExceptionOfType(ChError.class).isThrownBy(() -> {
      @SuppressWarnings("unused")
      var profile = new Profile(1, "kongra@gmail.com_", "Konrad Grzanek");
    });
    assertThatExceptionOfType(ChError.class).isThrownBy(() -> {
      @SuppressWarnings("unused")
      var profile = new Profile(1, "kongra@gmail.com", "   ");
    });

    var profile = new Profile(1, "kongra@gmail.com", "Konrad Grzanek");
    assertThat(profile.getId()).isEqualTo(1);
    assertThat(profile.getEmail()).isEqualTo("kongra@gmail.com");
    assertThat(profile.getName()).isEqualTo("Konrad Grzanek");
  }

}
