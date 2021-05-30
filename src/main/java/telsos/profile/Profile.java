package telsos.profile;

import static telsos.Ch.chEmail;
import static telsos.Ch.chNat;
import static telsos.Ch.chNonBlank;

import java.util.Objects;

public class Profile {

  private long id = -1;

  private String email;

  private String name;

  public Profile() {
  }

  public Profile(long id, String email, String name) {
    setId(id);
    setEmail(email);
    setName(name);
  }

  public final long getId() {
    return chNat(id);
  }

  public final void setId(long id) {
    this.id = chNat(id);
  }

  public final String getEmail() {
    return Objects.requireNonNull(email);
  }

  public final void setEmail(String email) {
    this.email = chEmail(email);
  }

  public final String getName() {
    return Objects.requireNonNull(name);
  }

  public final void setName(String name) {
    this.name = chNonBlank(name);
  }

  @Override
  public final int hashCode() {
    final var theId = getId();
    return 31 + (int) (theId ^ theId >>> 32);
  }

  @Override
  public final boolean equals(Object obj) {
    if (this == obj)
      return true;
    if (!(obj instanceof Profile))
      return false;
    var other = (Profile) obj;
    return getId() == other.getId();
  }

}
