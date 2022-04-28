package telsos.quark;

import javax.enterprise.context.ApplicationScoped;
import javax.enterprise.event.Observes;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@ApplicationScoped
public class CounterObserver {

  public void onCounterInc(@Observes Long counterValue) {
    LOG.debug("There's new counter value %d".formatted(counterValue));
  }

  private static final Logger LOG = LoggerFactory
      .getLogger(CounterObserver.class);
}
