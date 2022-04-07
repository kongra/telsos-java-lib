package telsos.quark;

import javax.inject.Inject;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Path("/greeting")
public class GreetingResource {

  @Inject
  GreetingTools greetingTools;

  @GET
  @Path("/hello/{id}")
  @Produces(MediaType.TEXT_PLAIN)
  public String hello(@PathParam("id") long id) {
    if (id > 100) {
      throw new IllegalArgumentException("The id must be <= 100");
    }

    LOG.debug("Is's ok to be nice");
    return greetingTools.sayHello("Test" + id);
  }

  private static final Logger LOG = LoggerFactory
      .getLogger(GreetingResource.class);
}