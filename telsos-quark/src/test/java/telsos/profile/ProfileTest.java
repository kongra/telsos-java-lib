package telsos.profile;

import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.time.LocalDate;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class ProfileTest {

  private final Profile profile = new Profile();

  @BeforeEach
  void setUp() throws Exception {
    clear();
  }

  private void clear() {
    profile.id = 0L;
    profile.email = "test1@junit.org";
    profile.registrationDate = LocalDate.now();
  }

  @Test
  void testDaysSinceRegistration2() {
    profile.registrationDate = null;
    assertThatExceptionOfType(NullPointerException.class).isThrownBy(() -> {
      profile.daysSinceRegistration();
    });
  }
}
