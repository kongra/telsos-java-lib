// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.paip;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Optional;

import org.junit.jupiter.api.Test;

class TestSearchesSingleRun {

//  private final TreePrinter<String> treePrinter = TreePrinter
//      .of(TestBreadthFirstSearch::children, Functions::identity);

  @Test
  void testBreadthFirstSearch() {
//    treePrinter.print("a", System.out::print);
//    System.out.println();

    assertThat(TestBreadthFirstSearch.search("a", "u"))
        .isEqualTo(Optional.of("u"));
  }

  @SuppressWarnings("static-method")
  @Test
  void testDepthFirstSearch() {
    assertThat(TestDepthFirstSearch.search("a", "u"))
        .isEqualTo(Optional.of("u"));
  }

}
