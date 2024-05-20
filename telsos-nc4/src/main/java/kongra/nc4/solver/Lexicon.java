// © 2024 Konrad Grzanek <kongra@gmail.com>
package kongra.nc4.solver;

import java.util.Arrays;
import java.util.BitSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.IntConsumer;
import java.util.stream.Collectors;

import org.eclipse.collections.api.block.function.primitive.BooleanFunction;

import kongra.nc4.model.Leximap;
import kongra.nc4.model.Value;
import kongra.nc4.model.Variable;
import telsos.functions.ByteConsumer;

final class Lexicon {

  static Lexicon of(Leximap leximap) {
    final var v2variable = leximap.variables().toArray(Variable[]::new);
    final var variablesCount = v2variable.length;
    final var v2domset = new BitSet[variablesCount];
    final var v2domsize = new byte[variablesCount];
    final var v2val2value = new Value[variablesCount][];

    final Map<Variable, Integer> variable2v = HashMap
        .newHashMap(variablesCount);

    @SuppressWarnings("unchecked")
    final Map<Value, Byte>[] v2value2val = new HashMap[variablesCount];

    forEveryV(variablesCount, v -> {
      final var variable = Objects.requireNonNull(v2variable[v]);
      final var val2value = leximap.valuesOf(variable).toArray(Value[]::new);

      final var domsize = val2value.length;
      if (domsize > Byte.MAX_VALUE)
        throw new IllegalArgumentException(
            "Illegal domsize %d".formatted(domsize));

      v2domsize[v]   = (byte) domsize;
      v2val2value[v] = val2value;
      variable2v.put(variable, v);

      v2domset[v] = new BitSet(domsize);
      v2domset[v].set(0, domsize);

      final Map<Value, Byte> value2val = HashMap.newHashMap(domsize);
      forEveryVal((byte) domsize, val -> {
        final var value = Objects.requireNonNull(val2value[val]);
        value2val.put(value, val);
      });

      v2value2val[v] = value2val;
    });

    final var valuesOfAllVariables = leximap
        .valuesOfAllVariables()
        .collect(Collectors.toUnmodifiableSet());

    return new Lexicon(
        variablesCount,
        v2domsize,
        v2domset,
        v2variable,
        v2val2value,
        variable2v::get,
        (v, value) -> v2value2val[v].get(value),
        leximap,
        valuesOfAllVariables::contains);
  }

  static LexiconMapping createLexiconMapping(
      Lexicon superLexicon,
      Lexicon subLexicon) {
    return new LexiconMapping(superLexicon, subLexicon);
  }

  int variablesCount() {
    return variablesCount;
  }

  BitSet[] v2domsetShallowCopy() {
    return Arrays.copyOf(v2domset, v2domset.length);
  }

  Variable variable(int v) {
    return v2variable[v];
  }

  Integer maybeV(Variable variable) {
    Objects.requireNonNull(variable);
    return variable2v.apply(variable);
  }

  int ensureV(Variable variable) {
    Objects.requireNonNull(variable);
    return maybeV(variable);
  }

  int domsize(int v) {
    return v2domsize[v];
  }

  Byte maybeVal(int v, Value value) {
    Objects.requireNonNull(value);
    return v2value2val.call(v, value);
  }

  byte ensureVal(int v, Value value) {
    Objects.requireNonNull(value);
    return maybeVal(v, value);
  }

  boolean isKnownValue(Value value) {
    Objects.requireNonNull(value);
    return isValue.booleanValueOf(value);
  }

  Value value(int v, byte val) {
    return v2val2value[v][val];
  }

  BitSet createDomset(int v) {
    final var n = domsize(v);
    final var domset = new BitSet(n);
    domset.set(0, n);
    return domset;
  }

  Leximap getLeximap() {
    return leximap;
  }

  // ASSIGNMENT CREATION
  static int[] createUnassignedVars(int length) {
    if (length > UNASSIGNED_VARS.length)
      throw new IllegalArgumentException();

    return Arrays.copyOf(UNASSIGNED_VARS, length);
  }

  static byte[] createUnassignedVals(int length) {
    if (length > UNASSIGNED_VALS.length)
      throw new IllegalArgumentException();

    return Arrays.copyOf(UNASSIGNED_VALS, length);
  }

  static final byte UNASSIGNED = -1;

  private static final int[] UNASSIGNED_VARS;

  private static final byte[] UNASSIGNED_VALS;

  static {
    UNASSIGNED_VARS = new int[100_000];
    Arrays.fill(UNASSIGNED_VARS, UNASSIGNED);
    UNASSIGNED_VALS = new byte[100_000];
    Arrays.fill(UNASSIGNED_VALS, UNASSIGNED);
  }

  // SUPER/SUB LEXICON MAPPING
  static void forEveryV(int variablesCount, IntConsumer body) {
    for (var v = 0; v < variablesCount; v++) {
      body.accept(v);
    }
  }

  static void forEveryV(Lexicon lexicon, IntConsumer body) {
    forEveryV(lexicon.variablesCount(), body);
  }

  void forEveryV(IntConsumer body) {
    forEveryV(this, body);
  }

  static void forEveryVal(byte domsize, ByteConsumer body) {
    for (var val = (byte) 0; val < domsize; val++) {
      body.accept(val);
    }
  }

  static class LexiconMapping {

    int superV(int v) {
      return superLexiconMapping[v];
    }

    Integer maybeSubV(int v) {
      final var subV = subLexiconMapping[v];
      return subV == UNASSIGNED ? null : subV;
    }

    private static int[] createSuperLexiconMapping(
        Lexicon superLexicon,
        Lexicon subLexicon) {

      final var mapping = new int[subLexicon.variablesCount()];

      subLexicon.forEveryV(subV -> {
        final var subVariable = subLexicon.variable(subV);
        final var superV = superLexicon.ensureV(subVariable);
        mapping[subV] = superV;
      });

      return mapping;
    }

    private static int[] createSubLexiconMapping(
        Lexicon superLexicon,
        Lexicon subLexicon) {
      /*
       * The UNASSIGNED values are needed here instead of just the default 0s,
       * because in sublex mapping the UNASSIGNED means no ability to perform
       * the mapping. In superlex case, the mapping is always possible.
       */
      final var mapping = createUnassignedVars(superLexicon.variablesCount());
      superLexicon.forEveryV(superV -> {
        final var superVariable = superLexicon.variable(superV);
        final var subV = subLexicon.maybeV(superVariable);
        if (subV != null) {
          mapping[superV] = subV;
        }
      });

      return mapping;
    }

    private final int[] superLexiconMapping;

    private final int[] subLexiconMapping;

    private LexiconMapping(Lexicon superLexicon, Lexicon subLexicon) {
      superLexiconMapping = createSuperLexiconMapping(superLexicon, subLexicon);
      subLexiconMapping   = createSubLexiconMapping(superLexicon, subLexicon);
    }
  }

  private final int variablesCount;

  private final byte[] v2domsize;

  private final BitSet[] v2domset;

  private final Variable[] v2variable;

  private final Value[][] v2val2value;

  /**
   * When the function returns null, it means there's no such Variable ->
   * v(ariable) mapping in the Lexicon.
   */
  private final Function<Variable, Integer> variable2v;

  private final V2Value2Val v2value2val;

  private final Leximap leximap;

  private final BooleanFunction<Value> isValue;

  @FunctionalInterface
  private interface V2Value2Val {

    /**
     * @param v
     * @param value
     * @return either a val(ue) or null that means there's no such v(ariable) ->
     *         Value -> val(ue) mapping in the Lexicon.
     */
    Byte call(int v, Value value);
  }

  @SuppressWarnings("java:S107") // SonarLint doesn't like the number or
                                 // parameters here.
  private Lexicon(
      int variablesCount,
      byte[] v2domsize,
      BitSet[] v2domset,
      Variable[] v2variable,
      Value[][] v2val2value,
      Function<Variable, Integer> variable2v,
      V2Value2Val v2value2val,
      Leximap leximap,
      BooleanFunction<Value> isValue) {

    this.variablesCount = variablesCount;
    this.v2domsize      = Objects.requireNonNull(v2domsize);
    this.v2domset       = Objects.requireNonNull(v2domset);
    this.v2variable     = Objects.requireNonNull(v2variable);
    this.v2val2value    = Objects.requireNonNull(v2val2value);
    this.variable2v     = Objects.requireNonNull(variable2v);
    this.v2value2val    = Objects.requireNonNull(v2value2val);
    this.leximap        = Objects.requireNonNull(leximap);
    this.isValue        = Objects.requireNonNull(isValue);
  }
}
