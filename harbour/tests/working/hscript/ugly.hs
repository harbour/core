<% FUNCTION Main()

      LOCAL a := "Hello Mom!" %><HTML><BODY><%
      qOut( a ) %><P>This is a <B>very ugly</B> script!!!<%
      qOut( "Line 2" )
      %>
<P>
<%
qOut( a, a, a )
%>
</HTML><%RETURN( NIL )%>
