// Copyright (c) Konrad Grzanek
// Created 20.07.19
package telsos;

public interface Pending {

  boolean isRealized();

  default boolean isPending() {
    return !isRealized();
  }

}
