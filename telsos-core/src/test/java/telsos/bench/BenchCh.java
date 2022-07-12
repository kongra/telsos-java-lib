// © 2019 Konrad Grzanek <kongra@gmail.com>
package telsos.bench;

import static telsos.Ch.chNonBlank;
import static telsos.Ch.chPos;

import java.util.concurrent.TimeUnit;

import org.openjdk.jmh.annotations.Benchmark;
import org.openjdk.jmh.annotations.BenchmarkMode;
import org.openjdk.jmh.annotations.Mode;
import org.openjdk.jmh.annotations.OutputTimeUnit;
import org.openjdk.jmh.annotations.Scope;
import org.openjdk.jmh.annotations.State;
import org.openjdk.jmh.infra.Blackhole;

@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.NANOSECONDS)
public class BenchCh {

  @Benchmark
  public static void chPosBench(MyState state, Blackhole blackhole) {
    final var l = chPos(state.l);
    blackhole.consume(l);
  }

  @Benchmark
  public static void chNonBlank1Bench(MyState state, Blackhole blackhole) {
    final var s = chNonBlank(state.s1);
    blackhole.consume(s);
  }

  @Benchmark
  public static void chNonBlank2Bench(MyState state, Blackhole blackhole) {
    final var s = chNonBlank(state.s2);
    blackhole.consume(s);
  }

  @Benchmark
  public static void chNonBlank3Bench(MyState state, Blackhole blackhole) {
    final var s = chNonBlank(state.s3);
    blackhole.consume(s);
  }

  @Benchmark
  public static void chNonBlank4Bench(MyState state, Blackhole blackhole) {
    final var s = chNonBlank(state.s4);
    blackhole.consume(s);
  }

  @Benchmark
  public static void chNonBlank5Bench(MyState state, Blackhole blackhole) {
    final var s = chNonBlank(state.s5);
    blackhole.consume(s);
  }

  @Benchmark
  public static void chNonBlank6Bench(MyState state, Blackhole blackhole) {
    final var s = chNonBlank(state.s6);
    blackhole.consume(s);
  }

  @State(Scope.Thread)
  public static class MyState {
    final String s1 = "a";
    final String s2 = " a";
    final String s3 = "a ";
    final String s4 = " a ";
    final String s5 = "A very long string with a lot of elements"
        + "inside of it usbdyfbusdybf yubsdfyu vyusvdf uyvsd uvy";

    final String s6 = " ".repeat(200) + s5;

    final long l = 1;
  }

}
