package telsos.profile;

import static telsos.Ch.chNonBlank;

import com.fasterxml.jackson.annotation.JsonCreator;
import com.fasterxml.jackson.annotation.JsonProperty;

import lombok.Getter;

@Getter
public final class Passwd {

  private final String value;

  @JsonCreator
  public Passwd(@JsonProperty("passwd") String value) {
    this.value = chNonBlank(value);
  }

  @Override
  public int hashCode() {
    return 31 + value.hashCode();
  }

  @Override
  public boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (obj == null || getClass() != obj.getClass())
      return false;
    var other = (Passwd) obj;
    return value.equals(other.value);
  }

}
