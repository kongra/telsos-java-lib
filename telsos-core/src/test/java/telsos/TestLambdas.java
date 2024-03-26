// © 2023 Konrad Grzanek <kongra@gmail.com>
package telsos;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.ToIntFunction;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class TestLambdas {

  static class SomeCodeToRun implements Runnable {
    @Override
    public void run() {
      System.out.println("Here I go!!!!");
    }
  }

  @Test
  void testLambdas() {
    final Runnable code = new SomeCodeToRun();
    code.run();

    final Runnable code2 = () -> System.out.println("Here I go again!!!!");
    code2.run();

    final Runnable code3 = () -> {
      System.out.println("Hehe!");
    };
    code3.run();

    final ToIntFunction<String> f1 = String::length;
    System.out.println(f1.applyAsInt("xyz"));

    final var coll1 = List.of(1, 2, 3, 4, 5, 6);
    doColl(coll1, e -> {
      System.out.println(e);
    });

    assertThat(coll1).isNotEmpty();

    doColl(coll1, System.out::println);
  }

  static <T> void doColl(Collection<T> coll, Consumer<T> body) {
    for (final T t : coll)
      body.accept(t);
  }

}
