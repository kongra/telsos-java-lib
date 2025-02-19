package telsos.java.lib.paip;

import java.util.function.Function;

@FunctionalInterface
public interface Adjs<T> extends Function<T, Iterable<T>> {}