package telsos.quark;

import javax.inject.Inject;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Path("/greeting")
public class GreetingResource {

  @Inject
  GreetingTools greetingTools;

  @GET
  @Path("/hello")
  @Produces(MediaType.TEXT_PLAIN)
  public String hello() {
    LOG.info("Is's ok to be nice");
    return greetingTools.sayHello("Test");
  }

  private static final Logger LOG = LoggerFactory
      .getLogger(GreetingResource.class);
}