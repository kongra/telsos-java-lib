// © 2019 Konrad Grzanek <kongra@gmail.com>
package telsos.bench;

import java.util.concurrent.TimeUnit;
import java.util.stream.LongStream;
import java.util.stream.Stream;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.infra.Blackhole;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
public class BenchStreams {

  @Benchmark
  public static void iterBench(MyState state, Blackhole blackhole) {
    var sum = 0L;
    for (var i = state.start; i < state.n; i++) {
      sum += i;
    }
    state.result = sum;
    blackhole.consume(sum);
  }

  @Benchmark
  public static void streamBench(MyState state, Blackhole blackhole) {
    var sum = LongStream.iterate(state.start, i -> i + 1).limit(state.n)
        .reduce(0L, Long::sum);
    state.result = sum;
    blackhole.consume(sum);
  }

  @Benchmark
  public static void iterLongBench(MyState state, Blackhole blackhole) {
    Long sum = 0L;
    for (Long i = state.start; i < state.n; i++) {
      sum += i;
    }
    state.result = sum;
    blackhole.consume(sum);
  }

  @Benchmark
  public static void streamLongBench(MyState state, Blackhole blackhole) {
    long sum = Stream.iterate(state.start, i -> i + 1).limit(state.n).reduce(0L,
        Long::sum);
    state.result = sum;
    blackhole.consume(sum);
  }

  @State(Scope.Thread)
  public static class MyState {
    final long start = 200;
    final long n = 300;
    long result = 0;
  }

}
