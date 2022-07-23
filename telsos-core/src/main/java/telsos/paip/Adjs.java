// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.paip;

import io.vavr.Function1;

@FunctionalInterface
public interface Adjs<T> extends Function1<T, Iterable<T>> {}