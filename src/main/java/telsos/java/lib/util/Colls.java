package telsos.java.lib.util;

import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.IntFunction;

import telsos.java.lib.O;

public final class Colls {

  public static <T> T[] requireNotNulls(T[] objects) {
    for (final T obj : objects) {
      O.nn(obj);
    }
    return objects;
  }

  public static <T, I extends Iterable<T>> I requireNotNulls(
      I iterable) {
    iterable.forEach(O::nn);
    return iterable;
  }

  public static <T, R, S> S createNonEmpty(
      T obj, T[] objs,
      IntFunction<R> createWithCapacity,
      BiConsumer<R, T> addObj,
      BiConsumer<R, T[]> addObjs,
      Function<R, S> postProcessor) {

    final var areSomeObjs = objs != null && objs.length != 0;
    final var capacity = 1 + (areSomeObjs ? objs.length : 0);
    final var coll = createWithCapacity.apply(capacity);
    addObj.accept(coll, obj);
    if (areSomeObjs) {
      addObjs.accept(coll, objs);
    }
    return postProcessor.apply(coll);
  }

  private Colls() {}
}
