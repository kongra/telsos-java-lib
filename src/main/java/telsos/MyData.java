package telsos;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class MyData {

  private final String email;

  private final String name;

  private final int age;

  private final String street;

  private final String number;

  @Override
  public final boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!(obj instanceof MyData)) {
      return false;
    }
    MyData other = (MyData) obj;
    if (email == null) {
      if (other.email != null) {
        return false;
      }
    } else if (!email.equals(other.email)) {
      return false;
    }
    return true;
  }

  @Override
  public final int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((email == null) ? 0 : email.hashCode());
    return result;
  }

}
