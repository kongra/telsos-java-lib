// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos;

import java.util.Objects;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.function.Supplier;

public final class DynVar<T> {

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

  public Optional<T> get() {
    return scopedValue.isBound() ? Optional.of(scopedValue.get())
        : Optional.empty();
  }

  public T get(T defaultValue) {
    return scopedValue.orElse(defaultValue);
  }

  public T get(Supplier<T> defaultValueSupplier) {
    return scopedValue.orElse(defaultValueSupplier.get());
  }

  private final ScopedValue<T> scopedValue;

  private DynVar(ScopedValue<T> scopedValue) {
    this.scopedValue = Objects.requireNonNull(scopedValue);
  }
}
