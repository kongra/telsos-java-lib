package telsos.paip;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.ArrayDeque;
import java.util.Optional;

import org.junit.jupiter.api.Test;

public class TestDepthFirstSearch {

  public static Optional<String> search(String start, String goal) {
    return DepthFirstSearch.of(TestBreadthFirstSearch::children, goal::equals)
        .search(start);
  }

  public static Optional<String> searchWithArray(String start, String goal) {
    return DepthFirstSearch.of(TestBreadthFirstSearch::children, goal::equals)
        .search(start, ArrayDeque::new);
  }

  public static Optional<String> search1(String start, String goal) {
    return DepthFirstSearch.of(TestBreadthFirstSearch::children1, goal::equals)
        .search(start);
  }

  public static Optional<String> searchWithArray1(String start, String goal) {
    return DepthFirstSearch.of(TestBreadthFirstSearch::children1, goal::equals)
        .search(start, ArrayDeque::new);
  }

  @SuppressWarnings("static-method")
  @Test
  final void testSearch() {
    assertThat(search("a", "a")).isEqualTo(Optional.of("a"));
    assertThat(search("a", "k")).isEqualTo(Optional.of("k"));
    assertThat(search("k", "k")).isEqualTo(Optional.of("k"));

    assertThat(search("d", "a")).isNotPresent();
    assertThat(search("www", "a")).isNotPresent();
    assertThat(search("a", "www")).isNotPresent();
  }

  @SuppressWarnings("static-method")
  @Test
  final void testSearch1() {
    assertThat(search1("a", "a")).isEqualTo(Optional.of("a"));
    assertThat(search1("a", "k")).isEqualTo(Optional.of("k"));
    assertThat(search1("k", "k")).isEqualTo(Optional.of("k"));

    assertThat(search1("d", "a")).isNotPresent();
    assertThat(search1("www", "a")).isNotPresent();
    assertThat(search1("a", "www")).isNotPresent();
  }

  @SuppressWarnings("static-method")
  @Test
  final void testSearchWithArray() {
    assertThat(searchWithArray("a", "a")).isEqualTo(Optional.of("a"));
    assertThat(searchWithArray("a", "k")).isEqualTo(Optional.of("k"));
    assertThat(searchWithArray("k", "k")).isEqualTo(Optional.of("k"));

    assertThat(searchWithArray("d", "a")).isNotPresent();
    assertThat(searchWithArray("www", "a")).isNotPresent();
    assertThat(searchWithArray("a", "www")).isNotPresent();
  }

  @SuppressWarnings("static-method")
  @Test
  final void testSearchWithArray1() {
    assertThat(searchWithArray1("a", "a")).isEqualTo(Optional.of("a"));
    assertThat(searchWithArray1("a", "k")).isEqualTo(Optional.of("k"));
    assertThat(searchWithArray1("k", "k")).isEqualTo(Optional.of("k"));

    assertThat(searchWithArray1("d", "a")).isNotPresent();
    assertThat(searchWithArray1("www", "a")).isNotPresent();
    assertThat(searchWithArray1("a", "www")).isNotPresent();
  }

}
