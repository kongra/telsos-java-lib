package telsos.profile;

import lombok.Data;

@Data
public class PasswdStrength {

  private final int basicScore;

  private final double entropy;

  private final boolean isEntropyMet;

  private final boolean isRandom;

  private final String t2cOff;

  private final String t2cOn;

}
