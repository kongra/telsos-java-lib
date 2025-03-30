package telsos.java.lib.refs;

public final class Ref<T> {

  @SuppressWarnings("java:S1104")
  public T value;

  public Ref() {}

  public Ref(T value) {
    this.value = value;
  }

}