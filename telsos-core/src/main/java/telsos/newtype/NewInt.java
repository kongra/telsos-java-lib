package telsos.newtype;

public class NewInt extends AbstractNewtype<NewInt> {

  private final int value;

  public NewInt(int value) {
    this.value = value;
  }

  @Override
  protected int hash() {
    return value();
  }

  @Override
  protected boolean isEqualTo(NewInt other) {
    return value() == other.value();
  }

  public int value() {
    return value;
  }

}
