/*
 * $Id$
 */

WARNING
-------

This document has serious bugs related to English Language.
I take no responsabilities for any misinformation in any form.
<GG>


HarbourScript Alfa Edition
--------------------------

Inside this .zip file you will find HarbourScript package. This
should be unzipped to /tests/working/hscript directory in order
to work w/o any changes.

To make the HarbourScript Translator, hscript.exe, use makehs.bat.
If you are going to test offline, there's no need to make it,
it will be done automatically for you (Batch Power<g>). To test it,
you'll have the following options:


Testing HarbourScript using MS-Personal Web Server
--------------------------------------------------

(and maybe IIS 3.0+ too!)

1. Copy hscript.exe and harbour.exe to your cgi-bin directory
(or any other with Scripting allowed)

2. Copy all the scripts that you want to the same directory

3. You can now test Harbour Script using
http://localhost/cgi-bin/hscript?script=<script>.hs

Note: The hscript.exe program still locks the server sometimes.
This is due to external compilation of the resulting code. As
soon as we start to use macro substitution instead, this will
become more stable.


Testing HarbourScript without a Web Server
------------------------------------------

1. Make the sample scripts with makehtm.

2. Browse the resulting .htm files as reported on screen.


How do this thing work?
-----------------------

Well, the HS (HarbourScript) tecnology is based on the ASP
(Active Server Pages) concept and someway in new Oracle 8i's
too.

A .hs page is like any normal HTML page with Special Tags and
Embedded Code. Those tags are: <% (Start Scripting) and %>
(End Scripting). Once you run this script, the HS translator
translates the Embedded Code into true Harbour Code and executes
it. In other words, you write your PRGs INSIDE your web pages.
This concept is called active content.


Known bugs
----------

- Web Server hanging some times (see above);


Open questions
--------------

Things not working due to lack of knowledge (if you know how
to do any of this things, leave a message on Harbour List with
subject HS Open questions, thanks! ;) ):

- How to associate .hs with hscript.exe safely in PWS and IIS.
I associated it using the Registry Key HKEY_LOCAL_MACHINE\System\>
CurrentControlSet\Services\W3SVC\Parameters\Script Map but
whenever I call the script I get a Server Error 500 without any
further explanation.


Felipe G. Coury
fcoury@flexsys-ci.com
