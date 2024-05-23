// © 2019 Konrad Grzanek <kongra@gmail.com>
package telsos.bench;

import static org.junit.jupiter.api.Assertions.assertFalse;

import java.util.List;

import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.OptionsBuilder;

@SuppressWarnings("static-method")
class TestJmhRunner {

  private static final List<Class<?>> BENCH_CLASSES = List.of(
      // BenchCh.class,
      // BenchStreams.class,
      // BenchBreadthFirstSearch.class
      BenchDepthFirstSearch.class);

  // Uncomment the annotation below to run the benchmark with mvm test
  // @Test
  void runJmhBenchmarks() throws RunnerException {
    final var opts = new OptionsBuilder().warmupIterations(3)
        .measurementIterations(3).forks(1)
        .jvmArgsAppend("-server", "-Xms256m", "-Xmx2G");

    BENCH_CLASSES.forEach(c -> opts.include(c.getSimpleName()));

    final var runResults = new Runner(opts.build()).run();
    assertFalse(runResults.isEmpty());
  }

}
