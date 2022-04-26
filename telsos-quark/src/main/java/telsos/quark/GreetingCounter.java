package telsos.quark;

import java.util.concurrent.atomic.AtomicLong;

import javax.enterprise.context.SessionScoped;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@SessionScoped
public class GreetingCounter {

  private final AtomicLong count = new AtomicLong(0);

  public void inc() {
    count.incrementAndGet();
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
