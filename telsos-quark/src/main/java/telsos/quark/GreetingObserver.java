package telsos.quark;

import javax.enterprise.context.ApplicationScoped;
import javax.enterprise.event.Observes;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@ApplicationScoped
public class GreetingObserver {

  public void onIncEvent(@Observes Long n) {
    LOG.info("GreetingObserver::onIncEvent(" + n + ")");
  }

  private static final Logger LOG = LoggerFactory
      .getLogger(GreetingObserver.class);

}
