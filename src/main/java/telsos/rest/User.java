package telsos.rest;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Data;

@Data
public class User {

  @Size(min = 5, max = 10)
  @NotNull
  private String name;

  @Email
  @NotNull
  private String email;

  private int age;

  public User incrementAge() {
    age++;
    return this;
  }

}
