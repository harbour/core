<%
/*
*
*  hello.hs
*  Famous "Hello World"!
*
*/

FUNCTION Main()

   // Add content-type parameter if using active page on a Web Server
   IF !empty( GetEnv( "SERVER_NAME" ) )
      qqOut( "HTTP/1.0 200 OK" )
      qOut( "CONTENT-TYPE: TEXT/HTML" )
      qOut( "" )
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
   qOut( "<H1>Hello world!</H1></P>" )

   // 2. Hybrid harbour-html:
%>
<H1><% qOut( "Hello world!" ) %></H1></P>
<%
   // 3. Pure html:
%>
<H1>Hello world!</H1></P>
</BODY>
</HTML>
