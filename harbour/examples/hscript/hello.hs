<%
/*
 * $Id$
 */

/*
*
*  Famous "Hello World"!
*
* Written by Felipe Coury <fcoury@flexsys-ci.com>
* www - http://harbour-project.org
*
*/

FUNCTION Start()

   // Add content-type parameter if using active page on a Web Server
   IF !empty( GetEnv( "SERVER_NAME" ) )
      OutStd( "HTTP/1.0 200 OK" + chr(10) )
      OutStd( "CONTENT-TYPE: TEXT/HTML" + chr(10) + chr(10) )
   ENDIF

%>
<HTML>
<HEAD>
<TITLE>Hello world!</TITLE>
</HEAD>

<BODY>
<%
   // Now saying hello to the world in 3 different ways:
   // 1. Pure harbour:
   OutStd( "<H1>Hello world!</H1></P>" )

   // 2. Hybrid harbour-html:
%>
<H1><% OutStd( "Hello world!" ) %></H1></P>
<%
   // 3. Pure html:
%>
<H1>Hello world!</H1></P>
</BODY>
</HTML>
