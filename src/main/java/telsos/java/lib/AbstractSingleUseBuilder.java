package telsos.java.lib;

public abstract class AbstractSingleUseBuilder<T> implements Builder<T> {

  private boolean used;

  @Override
  public T build() {
    if (used)
      throw new IllegalStateException("This builder can be used only once.");

    used = true;
    return buildImpl();
  }

  protected abstract T buildImpl();
}
