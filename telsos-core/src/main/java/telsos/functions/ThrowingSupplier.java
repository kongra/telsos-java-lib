// © 2019 Konrad Grzanek <kongra@gmail.com>
package telsos.functions;

import telsos.TelsosException;

@FunctionalInterface
public interface ThrowingSupplier<T, E extends Exception> {

  T get() throws E, TelsosException;

}
