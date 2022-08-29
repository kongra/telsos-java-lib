// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.newtype;

public class NewInt extends AbstractNewtype<NewInt> {

  private final int value;

  protected NewInt(int value) {
    this.value = value;
  }

  public int value() {
    return value;
  }
  
  @Override
  protected final int hash() {
    return value();
  }

  @Override
  protected final boolean isEqualTo(NewInt other) {
    return value() == other.value();
  }
  
  @Override
  public String toString() {
    return String.valueOf(value());
  }

}
