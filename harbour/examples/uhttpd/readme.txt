/*
 * $Id$
 */

uHTTPD micro web server

Build it without GD: ..\..\bin\hbmk uhttpd.hbm
Build it with    GD: ..\..\bin\hbmk uhttpdgd.hbm
  [ This one needs bgd.dll. Please download it from:
  http://www.libgd.org/releases/gd-latest-win32.zip ]

Add -DUSE_HB_INET to command line if you want to use Harbour's
built-in socket functions.

To see accepted parameters run: uhttpd -?
Parameters can also be defined using uhttpd.ini file.

Before starting please build modules in modules folder using bldhrb.bat

Once started connect to uhttpd using:
http://localhost:8082
to see default index page.

Francesco
