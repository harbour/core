/*
 * $Id$
 */

HTTPSRV micro web server

Build it without GD: hbmk2 httpsrv.hbp
Build it with    GD: hbmk2 httpsrvd.hbp
  [ This one needs bgd.dll. Please download it from:
  http://www.libgd.org/releases/gd-latest-win32.zip ]

Add -DUSE_HB_INET to command line if you want to use Harbour's
built-in socket functions.

To see accepted parameters run: httpsrv -?
Parameters can also be defined using httpsrv.ini file.

Before starting please build modules using: hbmk2 modules.hbp

Once started connect to httpsrv using:
http://localhost:8082
to see default index page.

Francesco
