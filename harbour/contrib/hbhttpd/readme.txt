/*
 * $Id$
 */

Date: Fri, 12 Jun 2009 19:47:37 +0300
From: Mindaugas Kavaliauskas <dbtopas@dbtopas.lt>
To: "Harbour Project Main Developer List." <harbour@harbour-project.org>
Subject: uhttpd v0.2


Hello,



I want to share some more ideas (and code) about uhttpd development.
All pro and cons, and any brainstorming is very welcome.

Sources can be obtained from:
http://www.dbtopas.lt/hrb/uhttpd-0.2.zip

You can test running demo application at (I'll try to keep it running
for some time): http://www.dbtopas.lt:8001/


I also want to add answer about one question. uhttpd support and
upload into Harbour SVN. I expected and wrote some time before:
------
I just have some ideas how to extend it, but I'm not sure if these
ideas will be similar to SVN changes by other people. It can happen,
that after some time I will propose something completely different
and incompatible from SVN.
------
I see many backward incompatible changes in my uhttpd, and I'm going
to do development in this incompatible way. I'm just experimenting
with my simple applications, and I want to find a best web application
architecture solution. I'm not interested in showcounter sample,
or uhttp_cookie object, so, I do not want to do any changes to SVN
uhttpd sample. Feel free to pick the features you like and put it
into SVN.



Regards,
Mindaugas



The main idea
=============
I've implemented sessions for uhttpd. This session model is different
from other WWW servers. In database oriented web applications, server
has to open database file read/write some data, generate html output,
close database, send response to client. This requires database
opening/closing for each request. The goal of this implementation was
to avoid this opening/closing and other per request initialization/
exit operations. This could be done by keeping a separate thread for
each session. Every request of some session is processed by the same
thread and this thread keeps databases open.
This approach makes web server  a little similar to terminal server:
each application has "its own" thread in web server (just like each
application has its own process in case remote terminal). Some remote
terminal protocol is used to send keyboard data to and receive screen
image from terminal server, here we use HTTP protocol for this
purpose.



Sessions
========
Main thread waits for connections. Accept connections are put into
common request queue. This queue is processed by some threads. These
threads reads http request from socket and analyzes session
information. If request corresponds to some session and session
information is required to handle particular request, the request is
redirected to sessioned request queue of corresponding session. These
sessioned requests are processed by sessioned threads. Each active
session has one sessioned thread to handle request. This thread keeps
databases open. Some request (for example, .css file request) does not
requires session data even if client has active session with server,
these request may be processed by threads of common request queue.

If keep-alive connections are used, after sessioned request is
processed, connection is put into common request queue. This helps to
move receiving of request and processing of static content responses
to common queue threads, thus leaving sessioned threads available for
generation of dynamic content for another keep-alive connection of the
same session.
                                               Common keep-alive conn.
                                              +----------------------+
                  Accepted                    |                      |
  +-------------+ connection                  V  Common queue        |
  | Main thread | -----------+------------->  ###################    |
  +-------------+            ^                                  V    |
                             |         +------+ +------+ +------+    |
                             |         |Thread| |Thread| |Thread| ---+
                             |         +------+ +------+ +------+    |
                             |                                       |
                   Keep-alive|                                       |
                  connections|     Sessioned request queues          |
                             |                                       |
                             |     ####  <---------------------------+
                             |     V                                 |
                             |     +------+                          |
                             +---- |Thread|                          |
                             |     +------+                          |
                             |                                       |
                             |                     Sessioned request |
                             |     ####  <---------------------------+
                             |     V
                             |     +------+
                             +---- |Thread|
                                   +------+



Thread circulation
==================
The figure above shows connection/request circulation. Thread
circulation is done in very similar way. All children threads are
created by main thread. These child threads wait for connections in
common request queue. If sessioned request is received, thread finds
the corresponding sessioned thread, passes request to it, and starts
to wait for new request. If sessioned thread is not found (the first
session request is received), this thread initializes session data
and becomes a sessioned thread. After processing of the first session
request, it does not return to common request queue, but waits for
requests of this session only. After session is destroyed, this
thread returns to common request queue.



Mounting table
==============
Traditional web servers exposes directory tree (DocumentRoot) to the
clients. Server side scripts are a regular files having executable
attribute or some kind of extension (ex., .php) inside directory
tree (or some aliased directory). uhttpd is oriented to be a single
compiled application and dynamic web pages are generated not by
external files (thought, we can add such possibility using .hrb or
.prg files), but generated by function linked into final executable.
Thus some "table" is needed to convert requested URL to server script.
This table is called mounting table in my uhttpd implementation. It
allows to mount a single URL or URL subtree to a particular handler
(function or codeblock).
Mounting table is hash, having this structure:
   oServer:aMount := { url => { handler, sessioned }, ... }
URL can a single URL path, or path containing '*' wildchar in the end.
Example:
    /app/login   - single URL match http://host/app/login
    /files/*     - the whole URL subtree from http://host/files/
    /*           - the whole URL tree http://host/

NOTE: '*' should be placed after '/' symbol to match URL subtree.
Usage of '/files*' is invalid and do not match '/files1', '/filesa'
or '/files/x'. The requested URL path is checked by deleting last
slashed part until URL is found in mounting table. If no URL found
in mounting table, 404 Not Found error is returned.
Example 1. If '/files/folder/aaa' is requested, '/files/folder/aaa',
'/files/folder/*', '/files/*', and '/*' will be checked before 404
error is returned.
Example 2. If '/files/folder/' is requested, '/files/folder/',
'/files/folder/*', '/files/*', and '/*' will be checked before 404
error is returned.

NOTE 2: if you want to use a slash-less URL address as a synonym for
the folder you may need an extra redirection rule. Ex.,
     "/files"   => {{|| URedirect("/files/")}, .F.}
     "/files/*" => {{|x| UProcFiles(DocumentRoot + x)}, .F.}


Widgets
=======
The implementation described above can be used to develop web
applications with a power comparable to plain php, but we will need
some framework/toolkit on top of basic uhttp server to allow a quick
application development. UWidgets is used for this purpose. It allows
to use some objects (browse, etc.) instead of plain:
    UWrite('<table>')
    DO WHILE ! EOF()
      UWrite('<tr><td>' + FIELD->NAME + '</td><td align="right">' +
             STR(FIELD->AGE) + '</td></tr>')
      DBSKIP()
    ENDDO
    UWrite('</table>')

To use UWigets under some URL subtree, you should add an entry to
server mounting table specifying standard widgets handler:
    "app/*" => {{|x| UProcWidgets(x, s_aMap)}, .T.}
You can see UWidgets handler requires requests to be sessioned.

s_aMap is one more table similar to server mounting table. Actually,
these two tables can be merged is widgets are implemented inside
server itself, but I want to keep widgets implementation separate,
thus, allowing an alternative implementations. s_aMap is hash
containing the mapping of URL subtree into handler functions. Ex.,
   s_aMap := { "login"        => @proc_login(), ;
               "main"         => @proc_main(), ;
               "account"      => @proc_account(), ;
               "items"        => @proc_items(), ;
               "items/edit"   => @proc_items_edit(), ;
               "logout"       => @proc_logout()}

Page handler functions receives a parameter indicating received
event/method. Handler has a structure:

STATIC FUNC proc_handler(cMethod)
   IF cMethod == "INIT"
     // This code is executed on entering URL (first call to this URL)
     // Here we open databases used to process queries
   ELSEIF cMethod == "POST"
     // Process HTTP POST request
   ELSEIF cMethod == "GET"
     // Process HTTP GET request
   ELSEIF cMethod == "EXIT"
     // This code is executed on leaving URL (before first call to
     // another URL)
     // Here we close databases opened in INIT method, etc.
   ENDIF
RETURN .T.

As you can see this handler reminds the structure of traditional GUI
based application message/event handler, for example in windows, we
have:

STATIC FUNC WndProc(hWnd, uMsg, wParam, lParam)
   IF uMsg == WM_CREATE
   ELSEIF uMsg == WM_PAINT
   ELSEIF uMsg == WM_DESTROY
   ENDIF
RETURN ...

I hope this similarity will help to develop (or convert) event based
GUI applications to web easier.

The widgets are created on INIT method. The main widget is UWMain
object. Creation of widgets is done using a function following
Clipper convention: <object_name>New(). So,
    oM := UWMainNew()
creates a main widget of web page. This main widget acts as a
layout/container in for example, GTK+ library. It has :Add() method
and other widgets can be included inside of it. Ex.,
     oM := UWMainNew()
     oM:Add( UWLabelNew("Hello, Widgets World!") )

UWidgets keeps main widget (and its children) inside session variable
and produces html output for it upon GET (or POST) requests. Main
widget "renders" all its child widgets, until the whole web page
content is generated. This html "rendering" is performed by
UWDefaultHandler().

POST method is usually used to perform some action on user data. I use
URedirect() function to do "redirect after post" and solve the problem
of from resubmitting, etc.



Modal page handlers
===================
Page handler has INIT, GET/POST, and EXIT messages. INIT and EXIT
methods are called only after you request of new page.

Ex.,

GET request "items" executes:
   page_items("INIT")
   page_items("GET")

Next GET request "items" executes:
   page_items("GET")

If you issue a GET "account" request, it will execute:
   page_items("EXIT")
   page_account("INIT")
   page_account("GET")

A tree structure of URL is transfered into page handles INIT, EXIT
logic. It helps make some feeling of modal structure of handler. I call
it "modal" because of idea how event are processed in event handlers of
modal dialogs. Let's have event handler function items_handler() for
items dialog, and items_edit_handler() function for item_edit dialog.

   PROC items_handler()
      ...

      IF event = "edit button pressed"
         dialog := create_new_modal_dialog() // create items_edit dialog
         dialog:handler := @items_edit_handler()
         process_event_loop() // until dialog is closed
         destroy_dialog(dialog)
      ENDIF
      ...
   RETURN

during process_event_loop() events are processed inside
items_edit_handler() function, and this event handler can access
workareas opened in a parent dialog (item dialog), private variables
of item_hadler(), etc.
The similar effect was tried to reach in page hadlers. Let's continue our
sample (last query was "account").

GET request for "items" executes:
   page_account("EXIT")
   page_items("INIT")
   page_items("GET")

GET request for "items/edit" executes:
   page_items_edit("INIT")  // no page_items("EXIT") !!!
   page_items_edit("GET")

GET request for "account" executes:
   page_items_edit("EXIT")
   page_items("EXIT")
   page_account("INIT")
   page_account("GET")

GET request for "account/edit" executes:
   page_account_edit("INIT")
   page_account_edit("GET")
etc...



Other major changes
===================
- dropped underscore and changed style to lower case for "global"
   memvars: server, get, post, cookie, session. These variables are
   accessed very often, and it is not some kind of internals marked by
   underscore. Code looked very UPPERCASE SCREAMING before;
- uhttpd rewritten to be an object. Server does not occupies main()
   function any more, and can be included into any application (or even
   a few servers in one application);
- implemented missing HTTP/1.1 headers;
- implemented keep-alive connections to reduce TCP handshake overhead;
- implemented HTTP/1.1 Last-Modified and co., to avoid resend of
   unmodified static content;
- session expiration;
- socket.c module supports linux (thanks Przemek for detailed
   instructions);
- socket.c is more multithread GC friendly (using hb_vm[Un]Lock());
- error handling. Runtime errors in "user" code does not cause server
   crash;



Pro and Cons
============

One thread per session
----------------------
Pro:
   * OK, if there is a limited number of clients actively using web
     application
Cons:
   * not scalable solution for sites with thousands low activity
     users (will keep a large number of inactive threads)

The implementation of sessioned threads was started from idea: it's
nice to have a prepared open aliases, positioned records, etc, on
request processing instead of opening, positioning and closing it
on every request. Some alias caching or another data model can solve
this problem.

Sessioned threads
-----------------
Pro:
   * Keeps alias state unchanged
   * Solves race condition problem for accessing session vartiables
Cons:
   * A little complicated and not standard architecture
   * Request should be divided into sessioned and non-sessioned

Modal page handlers
-------------------
Pro:
   * helps to have aliases opened in parent (as in modal application)
Cons:
   * disables to handle request for a few unrelated parts of web
     application. For example, if unrelated part having a different
     URL branch is opened in popup window. The old page handler
     receives EXIT message and state (aliases, etc) is lost.

Widgets and layouts
--------------------
Pro:
   * It's OK for compilated AJAX widgets, like browse
Cons:
   * For simple widgets (ex., UWHTML,. UWLabel, UWInput) is much easier
     to write a plain HTML code, than widget creation code



Roadmap
=======
I'm not sure if I will not delete the whole sessioned threads, modal
page handlers, and widgets in next version of uhttpd. But there are
a few things I know I will implement:
   * Templates
     Perhaps this can change widgets a lot. Only complicated widgets
     will remain. Simple widgets and layouts will move to templates.
