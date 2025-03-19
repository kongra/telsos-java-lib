package telsos.java.lib.logging;

@FunctionalInterface
public interface LogFactory<T> {

  Log getLog(T arg);

}
