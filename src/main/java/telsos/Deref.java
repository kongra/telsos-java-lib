// Copyright (c) Konrad Grzanek
// Created 20.07.19
package telsos;

@FunctionalInterface
public interface Deref<T> {

  T deref();

}
