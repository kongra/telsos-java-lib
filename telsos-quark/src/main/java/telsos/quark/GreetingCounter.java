package telsos.quark;

import java.util.concurrent.atomic.AtomicLong;

import javax.enterprise.context.ApplicationScoped;
import javax.enterprise.event.Event;
import javax.inject.Inject;

import org.eclipse.microprofile.config.inject.ConfigProperty;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@ApplicationScoped
public class GreetingCounter {

  @Inject
  Event<Long> incEvent;

  @Inject
  @ConfigProperty()
  Long incDelta;

  private final AtomicLong count = new AtomicLong(0);

  public void inc() {
    final var value = count.addAndGet(incDelta);
    incEvent.fire(value);
  }

  public long value() {
    return count.longValue();
  }

  public GreetingCounter() {
    LOG.debug("GreetingCounter::constructor");
  }

  private static final Logger LOG = LoggerFactory
      .getLogger(GreetingCounter.class);

}
