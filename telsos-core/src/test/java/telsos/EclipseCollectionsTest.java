// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos;

import static org.assertj.core.api.Assertions.assertThat;

import org.eclipse.collections.api.factory.Lists;
import org.eclipse.collections.api.list.MutableList;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

class EclipseCollectionsTest {

  MutableList<Integer> mutableList;

  @BeforeEach
  void beforeEach() {
    mutableList = Lists.mutable.of(1, 2, 3);
  }

  @Test
  void testMutableListConsistency() {
    assertThat(mutableList)
        .contains(1)
        .contains(2)
        .contains(3);
  }

}
