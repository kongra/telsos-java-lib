// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.newtypes;

public abstract class AbstractNewtype<T extends AbstractNewtype<T>> {

  protected abstract int hash();

  protected abstract boolean isEqualTo(T other);

  @Override
  public final int hashCode() {
    return 31 + hash();
  }

  @SuppressWarnings("unchecked")
  @Override
  public final boolean equals(Object obj) {
    return this == obj || obj != null && this.getClass() == obj.getClass()
        && isEqualTo((T) obj);
  }

}
