// © 2024 Konrad Grzanek <kongra@gmail.com>
package telsos.transactions;

import java.util.Optional;

public interface TransactionContextFactory {

  Optional<TransactionContext> createTransactionContext();

  Optional<NoTransactionContext> createNoTransactionContext();

}
