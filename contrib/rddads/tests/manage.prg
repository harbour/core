/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Advantage Database Server RDD Management Functions Test program
 *
 * Copyright 2001 Brian Hays <bhays@abacuslaw.com>
 * www - http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#require "rddads"

#include "ord.ch"

REQUEST ADS

#if defined( __HBDYNLOAD__RDDADS__ )
#include "rddads.hbx"
#endif

PROCEDURE Main()

   LOCAL i
   LOCAL aRay

#if defined( __HBDYNLOAD__RDDADS__ )
   LOCAL l := hb_libLoad( hb_libName( "rddads" + hb_libPostfix() ) )

   hb_rddADSRegister()

   HB_SYMBOL_UNUSED( l )
#elif defined( __HBSCRIPT__HBSHELL )
   hb_rddADSRegister()
#endif

   rddSetDefault( "ADS" )
   SET SERVER LOCAL    // REMOTE

   // use test   // make this available to get some stats on open tables below

   ? "Advantage Database Server Management Functions in Harbour"
   ?
   ? "Connect:", AdsMgConnect( "C:" )
   ?
   ? "AdsVersion( 0 ):", AdsVersion( 0 )
   ? "AdsVersion( 3 ):", AdsVersion( 3 )
   ?

   aRay := AdsMgGetInstallInfo()
   IF Len( aRay ) > 7
      ? "Install info:"
      ? aRay[ 1 ]
      ? aRay[ 2 ]
      ? aRay[ 3 ]
      ? aRay[ 4 ]
      ? aRay[ 5 ]
      ? aRay[ 6 ]
      ? aRay[ 7 ]
      ? aRay[ 8 ]
      ?
   ENDIF

   ? "Activity info:"
   ? AdsMgGetActivityInfo( 1 )
   ? AdsMgGetActivityInfo( 2 )

   aRay := AdsMgGetActivityInfo( 3 )
   IF Len( aRay ) > 3
      ? "Up Time:", aRay[ 1 ], aRay[ 2 ], aRay[ 3 ], aRay[ 4 ]
      ?
   ENDIF

   ?    "    Item          In Use     MaxUsed    Rejected"
   aRay := AdsMgGetActivityInfo( 4 )
   IF Len( aRay ) > 2
      ? "Users:         ", aRay[ 1 ], aRay[ 2 ], aRay[ 3 ]
   ENDIF

   aRay := AdsMgGetActivityInfo( 5 )
   IF Len( aRay ) > 2
      ? "Connections:   ", aRay[ 1 ], aRay[ 2 ], aRay[ 3 ]
   ENDIF

   aRay := AdsMgGetActivityInfo( 6 )
   IF Len( aRay ) > 2
      ? "WorkAreas:     ", aRay[ 1 ], aRay[ 2 ], aRay[ 3 ]
   ENDIF

   aRay := AdsMgGetActivityInfo( 7 )
   IF Len( aRay ) > 2
      ? "Tables:        ", aRay[ 1 ], aRay[ 2 ], aRay[ 3 ]
   ENDIF

   aRay := AdsMgGetActivityInfo( 8 )
   IF Len( aRay ) > 2
      ? "Indexes:       ", aRay[ 1 ], aRay[ 2 ], aRay[ 3 ]
   ENDIF

   aRay := AdsMgGetActivityInfo( 9 )
   IF Len( aRay ) > 2
      ? "Locks:         ", aRay[ 1 ], aRay[ 2 ], aRay[ 3 ]
   ENDIF

   aRay := AdsMgGetActivityInfo( 10 )
   IF Len( aRay ) > 2
      ? "TpsHeaderElems:", aRay[ 1 ], aRay[ 2 ], aRay[ 3 ]
   ENDIF

   aRay := AdsMgGetActivityInfo( 11 )
   IF Len( aRay ) > 2
      ? "TpsVisElems:   ", aRay[ 1 ], aRay[ 2 ], aRay[ 3 ]
   ENDIF

   aRay := AdsMgGetActivityInfo( 12 )
   IF Len( aRay ) > 2
      ? "TpsMemoElems:  ", aRay[ 1 ], aRay[ 2 ], aRay[ 3 ]
   ENDIF

   aRay := AdsMgGetActivityInfo( 13 )
   IF Len( aRay ) > 2
      ? "WorkerThreads: ", aRay[ 1 ], aRay[ 2 ], aRay[ 3 ]
   ENDIF

   WAIT
   ?

   aRay := AdsMgGetCommStats()
   IF Len( aRay ) > 10
      ? aRay[  1 ], "% of pkts with checksum failures "
      ? aRay[  2 ], "Total packets received           "
      ? aRay[  3 ], "Receive packets out of sequence  "
      ? aRay[  4 ], "Packet owner not logged in       "
      ? aRay[  5 ], "Receive requests out of sequence "
      ? aRay[  6 ], "Checksum failures                "
      ? aRay[  7 ], "Server initiated disconnects     "
      ? aRay[  8 ], "Removed partial connections      "
      ? aRay[  9 ], "Rcvd invalid packets (NT only)   "
      ? aRay[ 10 ], "RecvFrom failed (NT only)        "
      ? aRay[ 11 ], "SendTo failed (NT only)          "
   ENDIF

   WAIT
   ?

   aRay := AdsMgGetConfigInfo( 0 )
   IF Len( aRay ) > 24
      ? aRay[  1 ], " number connections            "
      ? aRay[  2 ], " number work areas             "
      ? aRay[  3 ], " number tables                 "
      ? aRay[  4 ], " number indexes                "
      ? aRay[  5 ], " number locks                  "
      ? aRay[  6 ], " user buffer                   "
      ? aRay[  7 ], " statistics dump interval      "
      ? aRay[  8 ], " max size of error log         "
      ? aRay[  9 ], " number TPS header elems       "
      ? aRay[ 10 ], " number TPS vis elems          "
      ? aRay[ 11 ], " number TPS memo elems         "
      ? aRay[ 12 ], " number rcv ECBs (NLM only)    "
      ? aRay[ 13 ], " number send ECBs (NLM only)   "
      ? aRay[ 14 ], " number packets per burst      "
      ? aRay[ 15 ], " number worker threads         "
      ? aRay[ 16 ], " index sort buffer size        "
      ? aRay[ 17 ], " reserved                      "
      ? aRay[ 18 ], " reserved                      "
      ? aRay[ 19 ], " error log path                "
      ? aRay[ 20 ], " semaphore file path           "
      ? aRay[ 21 ], " TPS log file path             "
      ? aRay[ 22 ], " reserved                      "
      ? aRay[ 23 ], " reserved                      "
      ? aRay[ 24 ], " NT Service IP send port #     "
      ? aRay[ 25 ], " NT Service IP rcv port #      "
   ENDIF

   WAIT
   ?

   aRay := AdsMgGetConfigInfo( 1 )
   IF Len( aRay ) > 12
      ? aRay[  1 ], " Total mem taken by cfg params "
      ? aRay[  2 ], " memory taken by connections   "
      ? aRay[  3 ], " memory taken by work areas    "
      ? aRay[  4 ], " memory taken by tables        "
      ? aRay[  5 ], " memory taken by indexes       "
      ? aRay[  6 ], " memory taken by locks         "
      ? aRay[  7 ], " memory taken by user buffer   "
      ? aRay[  8 ], " memory taken by TPS hdr elems "
      ? aRay[  9 ], " memory taken by TPS vis elems "
      ? aRay[ 10 ], " mem taken by TPS memo elems   "
      ? aRay[ 11 ], " mem taken by rcv ECBs (NLM)   "
      ? aRay[ 12 ], " mem taken by send ECBs (NLM)  "
      ? aRay[ 13 ], " mem taken by worker threads   "
   ENDIF

   ?

   // First arg:  pass in a file name for list of those with that file open
   // Second arg: Max # of users (required for memory allocation, default is 100)
   aRay := AdsMgGetUserNames()
   IF aRay != NIL
      ? "Number of connected users: ", Len( aRay )
      FOR i := 1 TO Len( aRay )
         ? aRay[ i ]
      NEXT
   ENDIF

   ?
   ? "Disconnect", AdsMgDisconnect()
   ?

   ? "end"
   ?

   RETURN
