package telsos.java.lib;

public interface Pending {

  boolean isPending();

  default boolean isRealized() {
    return !isPending();
  }

}
