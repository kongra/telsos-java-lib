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
    var list1 = List.of(1, 2, 3, 4, 5, 6, 7, 8);
    var list2 = list1.stream().filter(this::isOdd).collect(Collectors.toList());

    assertThat(list2).isNotEmpty();
  }

}
