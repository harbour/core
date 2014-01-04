/*
 * Harbour Project source code:
 * Harbour MT simple test
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

#include "rt_main.ch"

/* Don't change the position of this #include. */
#include "rt_vars.ch"

#define N_THREADS 5
#define N_JOBS    1000

PROCEDURE Main_MT()

#ifdef __HARBOUR__
   IF hb_mtvm()
      HBTEST DO_MTTES1()              IS "OK"
   ENDIF
#endif

   RETURN

#ifdef __HARBOUR__
FUNCTION DO_MTTES1()

   LOCAL aThreads, aResults, i, nDigit, nSum, nExpected
   LOCAL mtxJobs, mtxResults

   aThreads := {}
   aResults := {}
   mtxJobs := hb_mutexCreate()
   mtxResults := hb_mutexCreate()
   FOR i := 1 TO N_THREADS
      AAdd( aThreads, hb_threadStart( @thFunc(), mtxJobs, mtxResults ) )
   NEXT
   nDigit := 10
   FOR i := 1 TO N_JOBS
      hb_mutexNotify( mtxJobs, nDigit )
      nDigit++
   NEXT
   FOR i := 1 TO N_THREADS
      hb_mutexNotify( mtxJobs, NIL )
   NEXT
   FOR i := 1 TO N_JOBS
      hb_mutexSubscribe( mtxResults,, @nDigit )
      AAdd( aResults, nDigit )
   NEXT
   AEval( aThreads, {| x | hb_threadJoin( x ) } )
   nSum := 0
   FOR EACH nDigit IN aResults
      nSum += nDigit
   NEXT
   nSum := Round( nSum, 2 )
   nExpected := Round( ( 10 + 10 + N_JOBS - 1 ) / 2 / 3 * N_JOBS, 2 )
   IF Round( nSum - nExpected, 2 ) == 0
      RETURN "OK"
   ENDIF

   RETURN "ERROR, final sum: " + hb_ntos( nSum ) + ;
                 " expected: " + hb_ntos( nExpected )

PROCEDURE thFunc( mtxJobs, mtxResults )

   LOCAL xJob, xResult

   DO WHILE .T.
      hb_mutexSubscribe( mtxJobs,, @xJob )
      IF xJob == NIL
         EXIT
      ENDIF
      xResult := xJob / 3
      hb_mutexNotify( mtxResults, xResult )
   ENDDO

   RETURN

#endif /* __HARBOUR__ */

/* Don't change the position of this #include. */
#include "rt_init.ch"
