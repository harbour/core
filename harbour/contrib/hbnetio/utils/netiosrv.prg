/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    demonstration/test code for alternative RDD IO API which uses own
 *    very simple TCP/IP file server.
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
 *
 */

/* netio_mtserver() needs MT HVM version */
request HB_MT

proc main( port, ifaddr )
   local pListenSocket

   pListenSocket := netio_mtserver( iif( port != NIL, val( port ), ), ifaddr )
   if empty( pListenSocket )
      ? "Cannot start server."
   else
      wait "Press any key to stop NETIO server."
      netio_serverstop( pListenSocket )
      pListenSocket := NIL
   endif
return
