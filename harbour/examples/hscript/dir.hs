<%
/*
 * $Id$
 */

/*
* Written by Felipe Coury <fcoury@flexsys-ci.com>
* www - http://harbour-project.org
*
*/
FUNCTION Start()

   LOCAL aDir := Directory( "*.*" )
   LOCAL i

   IF !Empty( GetEnv( "SERVER_NAME" ) )
      OutStd( "HTTP/1.0 200 OK" + chr(10) )
      OutStd( "CONTENT-TYPE: TEXT/HTML" + chr(10) + chr(10) )
   ENDIF

   ASort( aDir,,, { |x, y| x[1] < y[1] } )
%>
<HTML>
<HEAD><TITLE>dir.hs - HarourScript demo of active context</TITLE></HEAD>

<BODY>
<TABLE border=1>
<TR>
<TD><B>File Name</B></TD>
<TD><B>Size</B></TD>
<TD><B>Date</B></TD>
<TD><B>Time</B></TD>
<TD><B>Attributes</B></TD>
</TR>
<%
   FOR i := 1 TO Len( aDir )
%>
<TR>
<TD><% OutStd( aDir[i,1] ) %></TD>
<TD><% OutStd( aDir[i,2] ) %></TD>
<TD><% OutStd( aDir[i,3] ) %></TD>
<TD><% OutStd( aDir[i,4] ) %></TD>
<TD><% OutStd( aDir[i,5] ) %></TD>
</TR>
<%
   NEXT
%>
</TABLE>
</BODY>
</HTML>
<%

   RETURN NIL
%>
