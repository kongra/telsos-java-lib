// © 2019 Konrad Grzanek <kongra@gmail.com>
package telsos;

public interface Pending {

  boolean isPending();

  default boolean isRealized() {
    return !isPending();
  }

}
