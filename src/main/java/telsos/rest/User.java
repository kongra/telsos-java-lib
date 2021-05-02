package telsos.rest;

import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Data;

@Data
public class User {

  @NotNull
  @Size(min = 5, max = 10)
  private String name;

}
