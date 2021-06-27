package telsos.profile;

import static telsos.Ch.chNonBlank;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Getter;

@Getter
public final class Passwd {

  private final String passwd;

  @JsonCreator
  public Passwd(@JsonProperty("passwd") String passwd) {
    this.passwd = chNonBlank(passwd);
  }

  @Override
  public int hashCode() {
    return 31 + passwd.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null || getClass() != obj.getClass())
      return false;
    var other = (Passwd) obj;
    return passwd.equals(other.passwd);
  }

}
