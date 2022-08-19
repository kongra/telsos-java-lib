// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos;

import static org.assertj.core.api.Assertions.assertThat;

import org.eclipse.collections.api.factory.Lists;
import org.eclipse.collections.api.list.MutableList;
import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class MiscJavaAndLibsTest {

  @Test
  void testMutableListConsistency() {
    final MutableList<Integer> mutableList = Lists.mutable.of(1, 2, 3);
    assertThat(mutableList)
        .contains(1)
        .contains(2)
        .contains(3);
  }

  @Test
  void testNaN2IntConversion() {
    final var x = Math.sqrt(-5);
    assertThat(x).isNaN();

    final var n = (int) x;
    assertThat(n).isZero();
  }

}
