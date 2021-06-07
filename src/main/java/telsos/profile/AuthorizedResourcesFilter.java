package telsos.profile;

import java.io.IOException;

import jakarta.servlet.Filter;
import jakarta.servlet.FilterChain;
import jakarta.servlet.ServletException;
import jakarta.servlet.ServletRequest;
import jakarta.servlet.ServletResponse;
import jakarta.servlet.annotation.WebFilter;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

@WebFilter("/resources/img/restricted/*")
public class AuthorizedResourcesFilter implements Filter {

  @Override
  public void doFilter(ServletRequest request, ServletResponse response,
      FilterChain chain) throws IOException, ServletException {

    final var httpReq = (HttpServletRequest) request;
    final var httpRes = (HttpServletResponse) request;
    final var profile = (Profile) httpReq.getSession().getAttribute("profile");

    if (profile == null) {
      httpRes.sendError(HttpServletResponse.SC_FORBIDDEN);
    }

    chain.doFilter(request, response);
  }

  public AuthorizedResourcesFilter() {
  }

}
