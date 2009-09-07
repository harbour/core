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

proc main( cPort, cAddress )
   local pListenSocket

   pListenSocket := netio_mtserver( hb_ntos( cPort ), cAddress )
   if empty( pListenSocket )
      ? "Cannot start server."
   else
      wait "Press any key to stop NETIO server."
      netio_serverstop( pListenSocket )
      pListenSocket := NIL
   endif
return
