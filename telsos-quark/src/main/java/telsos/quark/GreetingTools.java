package telsos.quark;

import javax.enterprise.context.ApplicationScoped;

@ApplicationScoped
public class GreetingTools {

  public String sayHello(String to) {
    return "Saying HELLO to " + to;
  }

}
