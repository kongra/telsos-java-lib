// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.util;

import java.util.Arrays;
import java.util.Objects;
import java.util.function.BiConsumer;
import java.util.function.Function;
import java.util.function.IntFunction;

import org.eclipse.collections.api.RichIterable;
import org.eclipse.collections.api.factory.Sets;
import org.eclipse.collections.api.set.ImmutableSet;
import org.eclipse.collections.api.set.MutableSet;

public final class Colls {

  @SafeVarargs
  public static <T> T[] requireNotNulls(T... objects) {
    for (final T obj : objects) {
      Objects.requireNonNull(obj);
    }
    return objects;
  }

  public static <T, I extends Iterable<T>> I requireNotNulls(
      I iterable) {
    for (final T obj : iterable) {
      Objects.requireNonNull(obj);
    }
    return iterable;
  }

  public static <T, R extends RichIterable<T>> R requireNotEmpty(
      R richIterable) {
    if (richIterable.isEmpty())
      throw new IllegalArgumentException();
    return richIterable;
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

  public static <T> ImmutableSet<T> nonEmptyImmutableSet(T obj, T[] objs) {
    return createNonEmpty(obj, objs,
        (IntFunction<MutableSet<T>>) Sets.mutable::ofInitialCapacity,
        MutableSet::add,
        (s, tab) -> s.addAll(Arrays.asList(tab)),
        MutableSet::toImmutable);
  }

  private Colls() {}
}
