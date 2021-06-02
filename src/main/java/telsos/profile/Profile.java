package telsos.profile;

import static telsos.Ch.chEmail;
import static telsos.Ch.chNat;
import static telsos.Ch.chNonBlank;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.core.JsonProcessingException;

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

  public static Profile fromJSONString(String json)
      throws JsonProcessingException {
    return JSON.readValue(Profile.class, json);
  }

  public static String toJSONString(Profile profile)
      throws JsonProcessingException {
    return JSON.writeValue(profile);
  }

  public String toJSONString() throws JsonProcessingException {
    return toJSONString(this);
  }

}
