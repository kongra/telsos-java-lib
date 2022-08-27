package telsos.newtype;

public final class NatInt extends NewInt {

  @Override
  public String toString() {
    return Integer.toString(value());
  }

  private NatInt(int value) {
    super(value);
  }

}
