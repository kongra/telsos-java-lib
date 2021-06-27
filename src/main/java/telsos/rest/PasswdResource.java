package telsos.rest;

import com.fasterxml.jackson.core.JsonProcessingException;

import jakarta.ws.rs.Consumes;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import jakarta.ws.rs.core.Response;
import me.gosimple.nbvcxz.Nbvcxz;
import me.gosimple.nbvcxz.resources.ConfigurationBuilder;
import me.gosimple.nbvcxz.scoring.TimeEstimate;
import telsos.JSON;
import telsos.Utils;
import telsos.profile.Passwd;
import telsos.profile.PasswdStrength;

@Path("/passwd")
public class PasswdResource {

  @GET
  @Path("/strength")
  @Consumes(MediaType.APPLICATION_JSON)
  @Produces(MediaType.APPLICATION_JSON)
  public Response strength(String passwdJSON) {
    return Utils.endPoint(() -> strengthImpl(passwdJSON));
  }

  private static Response strengthImpl(String passwdJSON)
      throws JsonProcessingException {
    var passwd = JSON.readValue(Passwd.class, passwdJSON);
    var result = NBVCXZ.estimate(passwd.getPasswd());

    var basicScore = result.getBasicScore();
    var entropy = result.getEntropy();
    var isMet = result.isMinimumEntropyMet();
    var isRandom = result.isRandom();
    var t2cOff = TimeEstimate.getTimeToCrackFormatted(result,
        "OFFLINE_BCRYPT_12");
    var t2cOn = TimeEstimate.getTimeToCrackFormatted(result,
        "ONLINE_THROTTLED");

    var response = new PasswdStrength(basicScore, entropy, isMet, isRandom,
        t2cOff, t2cOn);

    return Response.ok().entity(JSON.writeValueAsString(response)).build();
  }

  private static final Nbvcxz NBVCXZ = new Nbvcxz(
      new ConfigurationBuilder().setMinimumEntropy(20.0).createConfiguration());

}
