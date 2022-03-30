// © 2019 Konrad Grzanek <kongra@gmail.com>
package telsos.function;

import telsos.TelsosException;

@FunctionalInterface
public interface ThrowingSupplier<T, E extends Exception> {

  T get() throws E, TelsosException;

}
