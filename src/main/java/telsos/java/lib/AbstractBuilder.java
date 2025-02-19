package telsos.java.lib;

public abstract class AbstractBuilder<T> {

  private boolean used;

  public final T build() {
    if (used)
      throw new IllegalStateException("This builder can be used only once.");

    used = true;
    return buildImpl();
  }

  protected abstract T buildImpl();
}
