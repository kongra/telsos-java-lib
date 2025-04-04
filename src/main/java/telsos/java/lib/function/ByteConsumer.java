package telsos.java.lib.function;

import telsos.java.lib.O;

@FunctionalInterface
public interface ByteConsumer {

  void accept(byte value);

  default ByteConsumer andThen(ByteConsumer after) {
    O.nn(after);
    return t -> {
      accept(t);
      after.accept(t);
    };
  }

}
