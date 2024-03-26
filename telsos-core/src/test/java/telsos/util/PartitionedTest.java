// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.util;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.List;
import java.util.stream.IntStream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import telsos.math.newtype.PosInt;

class PartitionedTest {

  List<Integer> list;

  @BeforeEach
  void setUp() {
    list = List.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  }

  static PosInt partitionSize(int n) {
    return PosInt.of(n).orElseThrow(IllegalArgumentException::new);
  }

  @Test
  void testSize() {
    assertThat(Partitioned.partition(list, partitionSize(1))).hasSize(10);
    assertThat(Partitioned.partition(list, partitionSize(2))).hasSize(5);
    assertThat(Partitioned.partition(list, partitionSize(3))).hasSize(4);
    assertThat(Partitioned.partition(list, partitionSize(4))).hasSize(3);
    assertThat(Partitioned.partition(list, partitionSize(5))).hasSize(2);
    assertThat(Partitioned.partition(list, partitionSize(6))).hasSize(2);
    assertThat(Partitioned.partition(list, partitionSize(7))).hasSize(2);
    assertThat(Partitioned.partition(list, partitionSize(8))).hasSize(2);
    assertThat(Partitioned.partition(list, partitionSize(9))).hasSize(2);
    assertThat(Partitioned.partition(list, partitionSize(10))).hasSize(1);
    assertThat(Partitioned.partition(list, partitionSize(11))).hasSize(1);
    assertThat(Partitioned.partition(list, partitionSize(100))).hasSize(1);
  }

  @Test
  void testGetInt() {
    IntStream.range(1, 12).forEach(i -> {
      final var p = Partitioned.partition(list, partitionSize(i));
      System.out.println(p);
    });
  }

}
