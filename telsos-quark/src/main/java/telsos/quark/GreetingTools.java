// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.quark;

import javax.enterprise.context.ApplicationScoped;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@ApplicationScoped
public class GreetingTools {

  public String sayHello(String to) {
    return "Saying HELLO to " + to;
  }

  public GreetingTools() {
    LOG.info("GreetingTools::constructor");
  }

  private static final Logger LOG = LoggerFactory
      .getLogger(GreetingTools.class);

}
