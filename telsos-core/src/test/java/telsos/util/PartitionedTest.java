// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.util;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import telsos.Control;
import telsos.math.newtype.NatLong;
import telsos.math.newtype.PosInt;

class PartitionedTest {

  List<Integer> originalList;

  @BeforeEach
  void setUp() {
    originalList = List.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  }

  static PosInt partitionSize(int n) {
    return PosInt.of(n).orElseThrow(IllegalArgumentException::new);
  }

  @Test
  void testSize() {
    assertThat(Partitioned.list(originalList, partitionSize(1))).hasSize(10);
    assertThat(Partitioned.list(originalList, partitionSize(2))).hasSize(5);
    assertThat(Partitioned.list(originalList, partitionSize(3))).hasSize(4);
    assertThat(Partitioned.list(originalList, partitionSize(4))).hasSize(3);
    assertThat(Partitioned.list(originalList, partitionSize(5))).hasSize(2);
    assertThat(Partitioned.list(originalList, partitionSize(6))).hasSize(2);
    assertThat(Partitioned.list(originalList, partitionSize(7))).hasSize(2);
    assertThat(Partitioned.list(originalList, partitionSize(8))).hasSize(2);
    assertThat(Partitioned.list(originalList, partitionSize(9))).hasSize(2);
    assertThat(Partitioned.list(originalList, partitionSize(10))).hasSize(1);
    assertThat(Partitioned.list(originalList, partitionSize(11))).hasSize(1);
    assertThat(Partitioned.list(originalList, partitionSize(100))).hasSize(1);
  }

  @Test
  void testGetInt() {
    Control.doRange(AscRange.of(1, 10).orElseThrow(), i -> {
      final var p = Partitioned.list(originalList, partitionSize((int) i));
      System.out.println(p);
    });

    Control.times(NatLong.of(10).orElseThrow(), System.out::println);
  }

}
