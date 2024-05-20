// © 2024 Konrad Grzanek <kongra@gmail.com>
package kongra.nc4.model.app;

import java.util.Comparator;
import java.util.Objects;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.ToIntBiFunction;
import java.util.function.ToIntFunction;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

import org.eclipse.collections.api.multimap.set.MutableSetMultimap;
import org.eclipse.collections.impl.factory.Multimaps;

import kongra.nc4.model.Leximap;
import kongra.nc4.model.LeximapBuilder;
import kongra.nc4.model.ModelParser;
import kongra.nc4.model.Value;
import kongra.nc4.model.Variable;

final class LeximapBuilderImpl implements LeximapBuilder {

  private final ModelParser modelParser;

  private MutableSetMultimap<Variable, Value> data = Multimaps.mutable.set
      .empty();

  LeximapBuilderImpl(ModelParser modelParser) {
    this.modelParser = Objects.requireNonNull(modelParser);
  }

  @Override
  public LeximapBuilderImpl add(Variable variable, Value value) {
    Objects.requireNonNull(data);
    Objects.requireNonNull(variable);
    Objects.requireNonNull(value);
    data.put(variable, value);
    return this;
  }

  @Override
  public LeximapBuilderImpl add(String variable, String value) {
    return add(
        modelParser.parseVariable(variable)
            .orElseThrow(IllegalArgumentException::new),
        modelParser.parseValue(value)
            .orElseThrow(IllegalArgumentException::new));
  }

  @Override
  public Leximap build() {
    final var closedData = Objects.requireNonNull(data);
    data = null;

    return new Leximap() {
      @Override
      public Stream<Variable> variables() {
        return StreamSupport.stream(closedData.keysView().spliterator(),
            false);
      }

      @Override
      public Stream<Value> valuesOfAllVariables() {
        return StreamSupport.stream(closedData.valuesView().spliterator(),
            false);
      }

      @Override
      public Stream<Value> valuesOf(Variable variable) {
        Objects.requireNonNull(variable);
        return closedData.get(variable).stream();
      }
    };
  }

  @Override
  public Leximap buildOrdered(
      ToIntFunction<Variable> variablesOrdering,
      ToIntBiFunction<Variable, Value> maybeVariable2DomainOrdering) {

    Objects.requireNonNull(variablesOrdering);

    final var closedData = Objects.requireNonNull(data);
    data = null;

    final Comparator<Variable> comparator = Comparator
        .comparingInt(variablesOrdering);
    final SortedMap<Variable, Set<Value>> sortedData = new TreeMap<>(
        comparator);

    closedData.forEachKeyMutableSet((variable, values) -> {
      Set<Value> valueSet = values;
      if (maybeVariable2DomainOrdering != null) {
        final Comparator<Value> domainComparator = Comparator.comparingInt(
            value -> maybeVariable2DomainOrdering.applyAsInt(variable, value));
        valueSet = new TreeSet<>(domainComparator);
      }
      sortedData.put(variable, valueSet);
    });

    return new Leximap() {
      @Override
      public Stream<Variable> variables() {
        return sortedData.keySet().stream();
      }

      @Override
      public Stream<Value> valuesOfAllVariables() {
        return sortedData.values().stream().flatMap(Set::stream);
      }

      @Override
      public Stream<Value> valuesOf(Variable variable) {
        Objects.requireNonNull(variable);
        return sortedData.get(variable).stream();
      }
    };
  }
}