<%
/*
*
*  multiply.hs
*  HarbourScript Test
*
* Copyright (C) 1999  Felipe Coury <fcoury@flexsys-ci.com>
* www - http://www.harbour-project.org
*
*/

FUNCTION Main()

   LOCAL i, j

   IF !empty( GetEnv( "SERVER_NAME" ) )
      qqOut( "HTTP/1.0 200 OK" )
      qOut( "CONTENT-TYPE: TEXT/HTML" )
      qOut( "" )
   ENDIF
%>
<HTML>
<HEAD>
<TITLE>Testing script</TITLE>
</HEAD>

<H1><% qOut( "Teste de HarbourScript!" ) %></H1>

<BODY>
<%
   FOR i := 1 TO 10
%>
<TABLE border=1>
<TR>
<%
      FOR j := 1 TO 10
%>
<TR>
<TD><% qOut( str( i, 2 ) + " x " + str( j, 2 ) ) %></TD>
<TD><% qOut( i*j ) %></TD>
</TR>
<%
      NEXT
%>
</TABLE>
<BR>
<%
   NEXT
%>
</BODY>

</HTML>
