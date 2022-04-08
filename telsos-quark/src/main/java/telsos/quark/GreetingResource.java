package telsos.quark;

import java.lang.System.Logger;
import java.lang.System.Logger.Level;

import javax.inject.Inject;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

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

    LOG.log(Level.DEBUG, "Is's ok to be nice");
    return greetingTools.sayHello("Test" + id);
  }

//  private static final Logger LOG = LoggerFactory
//      .getLogger(GreetingResource.class);

  private static final Logger LOG = System
      .getLogger(GreetingResource.class.getName());

}