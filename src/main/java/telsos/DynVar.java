// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos;

import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.function.Supplier;

import telsos.functions.Deref;

public final class DynVar<T> implements Deref<Optional<T>> {

  public static <T> DynVar<T> newInstance() {
    return of(ScopedValue.newInstance());
  }

  public static <T> DynVar<T> of(ScopedValue<T> scopedValue) {
    return new DynVar<>(scopedValue);
  }

  public void exec(T value, Runnable body) {
    ScopedValue.where(scopedValue, value).run(body);
  }

  public <E> E eval(T value, Callable<E> body) {
    return Exceptions
        .evalNothrowing(() -> ScopedValue.where(scopedValue, value).call(body));
  }

  public T get(T defaultValue) {
    return scopedValue.orElse(defaultValue);
  }

  public T get(Supplier<T> defaultValueSupplier) {
    return scopedValue.orElse(defaultValueSupplier.get());
  }

  @Override
  public Optional<T> deref() {
    return scopedValue.isBound() ? Optional.of(scopedValue.get())
        : Optional.empty();
  }

  private final ScopedValue<T> scopedValue;

  private DynVar(ScopedValue<T> scopedValue) {
    this.scopedValue = Objects.requireNonNull(scopedValue);
  }
}
