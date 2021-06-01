package telsos.profile;

import static telsos.Ch.chEmail;
import static telsos.Ch.chNat;
import static telsos.Ch.chNonBlank;

import java.io.IOException;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.databind.ObjectWriter;

import lombok.Getter;
import telsos.JSON;

@Getter
public class Profile {

  private final long id;

  private final String email;

  private final String name;

  @JsonCreator
  public Profile(@JsonProperty("id") long id,
      @JsonProperty("email") String email, @JsonProperty("name") String name) {
    this.id = chNat(id);
    this.email = chEmail(email);
    this.name = chNonBlank(name);
  }

  @Override
  public final int hashCode() {
    return 31 + (int) (id ^ id >>> 32);
  }

  @Override
  public final boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (!(obj instanceof Profile))
      return false;
    var other = (Profile) obj;
    return id == other.id;
  }

  private static final ObjectReader PROFILE_JSON_READER = JSON.OBJECT_MAPPER
      .readerFor(Profile.class);

  private static final ObjectWriter PROFILE_JSON_WRITER = JSON.OBJECT_MAPPER
      .writerFor(Profile.class);

  public static Profile fromJSONString(String json) throws IOException {
    return PROFILE_JSON_READER.readValue(json, Profile.class);
  }

  public static String toJSONString(Profile profile)
      throws JsonProcessingException {
    return PROFILE_JSON_WRITER.writeValueAsString(profile);
  }

  public String toJSONString() throws JsonProcessingException {
    return toJSONString(this);
  }

}
