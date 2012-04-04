<%
/*
 * $Id$
 */

/*
*
*  HarbourScript Test
*
* Written by Felipe Coury <fcoury@flexsys-ci.com>
* www - http://harbour-project.org
*
*/

FUNCTION Start()

   LOCAL i, j

   IF !empty( GetEnv( "SERVER_NAME" ) )
      OutStd( "HTTP/1.0 200 OK" + chr(10) )
      OutStd( "CONTENT-TYPE: TEXT/HTML" + chr(10) + chr(10) )
   ENDIF
%>
<HTML>
<HEAD>
<TITLE>Testing script</TITLE>
</HEAD>

<H1><% OutStd( "HarbourScript Test!" ) %></H1>

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
<TD><% OutStd( str( i, 2 ) + " x " + str( j, 2 ) ) %></TD>
<TD><% OutStd( i*j ) %></TD>
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
