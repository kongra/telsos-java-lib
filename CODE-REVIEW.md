# Code Review: telsos-java-lib

**Reviewed:** 2026-03-10
**Reviewer:** Claude Sonnet 4.6
**Java version:** 25
**Build tool:** Maven

---

## Executive Summary

A compact, well-structured utility library with a strong functional-programming orientation. The code is intentional: newtype safety, typeclass abstraction, lazy evaluation, scoped binding, and three-valued logic are all implemented coherently and with care. The library has zero runtime dependencies, which is a correct design choice.

Two confirmed bugs were found and fixed. Several moderate and minor issues were addressed. All findings are recorded below with their resolution status.

---

## Findings

### 1. `BigDecimalInstances.ENUM` — `pred` and `succ` swapped **[Fixed]**

`pred` was adding 1 and `succ` was subtracting 1 — completely inverted. Fixed by swapping the implementations.

---

### 2. `BigDecimalInstances.BOUNDED` — both bounds were zero **[Fixed]**

`new BigDecimal("-0")` equals `BigDecimal.ZERO`; both bounds resolved to 0. `BigDecimal` is mathematically unbounded so the `BOUNDED` instance was meaningless. Removed `BOUNDED`, `BIG_DECIMAL_BOUNDS`, and the unused `Bounded` import.

---

### 3. `DynVar.get(Supplier<T>)` — supplier evaluated eagerly **[Fixed]**

`scopedValue.orElse(defaultValueSupplier.get())` evaluated the supplier before `orElse()` was called, running it even when the scoped value was bound. Fixed with `scopedValue.isBound() ? scopedValue.get() : defaultValueSupplier.get()`. (`ScopedValue` has no `orElseGet(Supplier)` overload unlike `Optional`.)

---

### 4. `ExInfo` and `Invalid` — missing `serialVersionUID` **[Fixed]**

Both extend `RuntimeException` (which is `Serializable`) and use `transient` fields, but had no `serialVersionUID`. Added `@java.io.Serial private static final long serialVersionUID = 1L` to both.

---

### 5. `Threads.run()` — interrupt flag restored AND exception rethrown **[Retracted]**

Initially flagged as redundant. The original code is correct: `Ex.rethrow(e)` uses an unchecked generic cast and is opaque to SonarQube (`java:S2142`), which cannot see it as a rethrow. Restoring the interrupt flag satisfies the rule and is the right pattern — callers that check the flag rather than catching exceptions also receive the signal.

---

### 6. `AbstractNewtype.hashCode()` — weak hash formula **[Fixed]**

`31 + hash()` gives poor distribution for newtypes wrapping small consecutive integers. Changed to `31 * 17 + hash()`.

---

### 7. `Partitioned` — `hashCode()`/`equals()` overrides **[Retracted]**

Initially flagged as redundant no-ops. The overrides are load-bearing for SonarQube: rule `java:S2160` flags subclasses that add fields without overriding `equals`; rule `java:S1206` then requires `hashCode` whenever `equals` is overridden. The no-op overrides are the correct way to satisfy both rules while keeping `AbstractList` semantics.

---

### 8. BFS/DFS — inconsistent null-adjacency handling **[Fixed]**

The `Adjs` contract was ambiguous. Fixed by:
- Documenting in `Adjs` that implementations must never return `null` (use an empty `Iterable` for leaf nodes).
- Removing the `isNotEmpty()` null-guard helper from BFS.
- Removing the `if (childrenIt != null)` guard from DFS.
- Both algorithms now get the iterator directly and only enqueue/push if `hasNext()`.
- Updated `TestBreadthFirstSearch.children1` to honour the contract (`List.of()` instead of `null`).

---

### 9. `StrIts.java` — empty placeholder class **[Fixed]**

Deleted.

---

### 10. `NumericFixedPoint.areCloseEnough()` — epsilon recomputed on every call **[Open]**

`epsilon` (1/1,000,000) is reconstructed from scratch on every call to `areCloseEnough()`, which runs in a tight convergence loop. For `BigDecimal` this means fresh object allocation per iteration. No action taken; address if profiling shows it matters.

---

### 11. `--enable-preview` in a library **[Fixed]**

Removed `--enable-preview` and `-Xlint:-preview` from the compiler, surefire, and failsafe plugin configurations. The Java version was also updated to 25.

---

### 12. `Ord.asOrdering()` — unreachable `default` branch **[Fixed]**

`Integer.signum()` only returns `-1`, `0`, or `1`; the `default -> throw new Impossible()` branch was dead. Replaced `case 1` with `default -> Ordering.GT` (required by the compiler for int switch expressions) and removed the unused `Impossible` import.

---

## Design Notes (No Action Required)

**Sneaky throws (`Ex.rethrow`)**: The `sneakyThrow0` pattern bypasses Java's checked-exception system via a generic cast. Legitimate and widely used (Lombok, Kotlin, etc.); `@SuppressWarnings("unchecked")` is correctly applied.

**`Delay` thread-safety**: The `volatile Lock` idiom is a correct double-checked locking variant. Setting `lock = null` (a volatile write) creates the necessary happens-before relationship for the non-volatile `value` and `exception` fields. `@SuppressWarnings("java:S3077")` is appropriate.

**Public mutable fields in `Ref`**: `@SuppressWarnings("java:S1104")` on `Ref.value` is appropriate. The Ref classes are intentional mutable boxes; getters/setters would add friction without benefit.

**`Invalid` disables suppression and stack trace**: `writableStackTrace = false` is a valid performance optimisation for validation exceptions thrown frequently.

**`Control.STEP_1` using `orElseThrow(Impossible::new)`**: Correct — the empty branch is genuinely impossible for the literal `1`.

---

## Summary Table

| # | Severity | File | Issue | Status |
|---|----------|------|-------|--------|
| 1 | **Bug** | `BigDecimalInstances.java` | `pred`/`succ` implementations swapped | Fixed |
| 2 | **Bug** | `BigDecimalInstances.java` | `BOUNDED` both bounds zero | Fixed |
| 3 | **Significant** | `DynVar.java` | `get(Supplier)` evaluates supplier eagerly | Fixed |
| 4 | **Significant** | `ExInfo.java`, `Invalid.java` | Missing `serialVersionUID` | Fixed |
| 5 | Minor | `Threads.java` | Interrupt flag restored AND exception rethrown | Retracted |
| 6 | Minor | `AbstractNewtype.java` | Weak hash formula | Fixed |
| 7 | Minor | `Partitioned.java` | Redundant `hashCode()`/`equals()` overrides | Retracted |
| 8 | Minor | `BreadthFirstSearch.java` | Inconsistent null-adjacency handling vs DFS | Fixed |
| 9 | Minor | `StrIts.java` | Empty placeholder class | Fixed |
| 10 | Minor | `NumericFixedPoint.java` | Epsilon recomputed on every convergence check | Open |
| 11 | Minor | `pom.xml` | Preview features in library | Fixed |
| 12 | Cosmetic | `Ord.java` | Unreachable `default` in `signum` switch | Fixed |
