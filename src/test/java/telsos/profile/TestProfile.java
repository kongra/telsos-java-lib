package telsos.profile;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;

import telsos.ChError;
import telsos.JSON;

class TestProfile {

  @Test
  void testProfileConstructor() {
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

  final static ObjectMapper mapper = new ObjectMapper();

  @Test
  void testProfileJsonSerialization() throws JsonProcessingException {
    var profile = new Profile(1, "kongra@gmail.com", "Konrad Grzanek");
    var json = JSON.writeValueAsString(profile);
    var str = "{" + "\"id\":1," + "\"email\":\"kongra@gmail.com\""
        + ",\"name\":\"Konrad Grzanek\"" + "}";
    assertThat(json).isEqualTo(str);
  }

  @Test
  void testProfileJsonDeserialization() throws IOException {
    var str = "{" + "\"id\":1," + "\"email\":\"kongra@gmail.com\""
        + ",\"name\":\"Konrad Grzanek\"" + "}";
    var profile = JSON.readValue(Profile.class, str);
    var profile1 = new Profile(1, "kongra@gmail.com", "Konrad Grzanek");
    assertThat(profile).usingRecursiveComparison().isEqualTo(profile1);
  }

}
