<%@page contentType="text/html; charset=utf-8" pageEncoding="utf-8" %>
<%@ taglib prefix = "c" uri = "http://java.sun.com/jsp/jstl/core" %>
<html>
<head>
<meta charset="utf-8">
</head>
<body>
  <h2>Test polskich czcionek: ąęśćółżźń</h2>
  
  <c:forEach var="sample" items="some">
    <c:out value="sample"></c:out>
  </c:forEach>
  
</body>
</html>
