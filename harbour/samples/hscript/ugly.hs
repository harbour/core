<% FUNCTION Main()

/* Written by Felipe Coury <fcoury@flexsys-ci.com>
* www - http://www.harbour-project.org
*
*/

      LOCAL a := "Hello Mom!" %><HTML><BODY><%
      OutStd( a ) %><P>This is a <B>very ugly</B> script!!!<%
      OutStd( "Line 2" )
      %>
<P>
<%
OutStd( a, a, a )
%>
</HTML><%RETURN( NIL )%>
