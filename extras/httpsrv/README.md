uHTTPD Micro Web Server
=======================

1. Build it

   * without GD:

   `hbmk2 uhttpd.hbp modules.hbp`

   * with GD (it requires hbgd library):

   `hbmk2 uhttpdgd.hbp modulesg.hbp`

2. To see accepted parameters, run:

   `uhttpd -?`

   Parameters can also be set in `uhttpd.ini` file.

3. Once started, the default index page will be available on URL:

   <http://localhost:8082>

Francesco
