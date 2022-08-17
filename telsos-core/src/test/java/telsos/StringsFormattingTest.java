// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.IOException;
import java.nio.file.Path;
import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

class StringsFormattingTest {

  private boolean isOdd(int n) {
    return n % 2 == 1;
  }

  @Test
  void testFormatting() throws IOException {
    final var list1 = List.of(1, 2, 3, 4, 5, 6, 7, 8);
    final var list2 = list1
        .stream()
        .filter(this::isOdd)
        .collect(Collectors.toList());

    final var s1 = """
        some value without a doubt some longer text  , so just go away
        some value without a doubt that contains just, so just go away
        some value without a doubt a few random lines, so just go away
        """;

    final var l1 = s1.lines()
        .map(l -> l + " ")
        .map(l -> l + "-")
        .map(l -> l + "/")
        .map(l -> l + "|")
        .toList();

    System.out.println(s1);
    System.out.println(l1);
    System.out.println(Path.of(".").toRealPath());

    l1.forEach(System.out::println);

    assertThat(list2).hasSize(4);

    record Range(int start, int end) {
      Range(int start) {
        this(start, start);
      }

      @Override
      public int hashCode() {
        return 31 + end;
      }

      @Override
      public boolean equals(Object obj) {
        return this == obj ||
            obj instanceof final Range other
                && other.end == end;
      }
    }

    final var r1 = new Range(1, 2);
    final var r2 = new Range(1);

    System.out.println(r1);
    System.out.println(r2);
  }

}
