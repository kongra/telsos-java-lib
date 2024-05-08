// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.functions;

import java.util.Objects;

@FunctionalInterface
public interface ByteConsumer {

  void accept(byte value);

  default ByteConsumer andThen(ByteConsumer after) {
    Objects.requireNonNull(after);
    return t -> {
      accept(t);
      after.accept(t);
    };
  }

}
