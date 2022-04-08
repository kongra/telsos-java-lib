// © 2022 Konrad Grzanek <kongra@gmail.com>
package telsos.quark;

import static telsos.Ch.chRange;

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
    chRange(1, 100, id);
    LOG.log(Level.DEBUG, "Is's ok to be nice");
    return greetingTools.sayHello("Test" + id);
  }

  private static final Logger LOG = System
      .getLogger(GreetingResource.class.getName());

}