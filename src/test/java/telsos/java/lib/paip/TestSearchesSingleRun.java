package telsos.java.lib.paip;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Optional;

import org.junit.jupiter.api.Test;

class TestSearchesSingleRun {

  @Test
  void testBreadthFirstSearch() {
    assertThat(TestBreadthFirstSearch.search("a", "u"))
        .isEqualTo(Optional.of("u"));
  }

  @Test
  void testDepthFirstSearch() {
    assertThat(TestDepthFirstSearch.search("a", "u"))
        .isEqualTo(Optional.of("u"));
  }

}
