package telsos;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.ObjectReader;
import com.fasterxml.jackson.databind.ObjectWriter;

public class JSON {

  public static ObjectReader readerFor(Class<?> c) {
    synchronized (readers) {
      return readers.computeIfAbsent(c, mapper::readerFor);
    }
  }

  public static ObjectWriter writerFor(Class<?> c) {
    synchronized (writers) {
      return writers.computeIfAbsent(c, mapper::writerFor);
    }
  }

  public static <T> T readValue(Class<? extends T> c, String json)
      throws JsonProcessingException {
    return readerFor(c).readValue(json);
  }

  public static String writeValueAsString(Class<?> c, Object obj)
      throws JsonProcessingException {
    return writerFor(c).writeValueAsString(obj);
  }

  public static String writeValueAsString(Object obj)
      throws JsonProcessingException {
    return writeValueAsString(obj.getClass(), obj);
  }

  private static final ObjectMapper mapper = new ObjectMapper();
  private static final Map<Class<?>, ObjectReader> readers = new HashMap<>();
  private static final Map<Class<?>, ObjectWriter> writers = new HashMap<>();

  private JSON() {}

}
