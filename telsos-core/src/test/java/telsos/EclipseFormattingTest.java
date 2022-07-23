// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

class EclipseFormattingTest {

  private boolean isOdd(int n) {
    return n % 2 == 1;
  }

  @Test
  void testFormatting() {
    final var list1 = List.of(1, 2, 3, 4, 5, 6, 7, 8);
    final var list2 = list1
        .stream()
        .filter(this::isOdd)
        .collect(Collectors.toList());

    final var s1 = """
        some longer text
        that contains just
        a few random lines
        """;

    final var l1 = s1.lines()
        .map(l -> l + " ")
        .map(l -> l + "-")
        .map(l -> l + "/")
        .map(l -> l + "|" + '\n')
        .toList();

    System.out.println(s1);
    System.out.println(l1);

    assertThat(list2).hasSize(4);
  }

}
