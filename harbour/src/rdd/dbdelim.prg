/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Copies the contents of a database to a delimited text file.
 * Appends the contents of a delimited text file to a database.
 *
 * Copyright 2001-2003 David G. Holm <dholm@jsd-llc.com>
 * www - http://harbour-project.org
 * APPEND FROM code submitted by Marco Braida <marcobra@elart.it>
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *    function __dbDelim() replaced by the new one which uses
 *    DELIM RDD I've just created
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

REQUEST Delim

FUNCTION __dbDelim( lExport, cFile, cDelimArg, aFields, bFor, bWhile, nNext, nRecord, lRest, cCodePage )

#ifdef HB_CLP_STRICT

   LOCAL nSrcArea
   LOCAL nDstArea
   LOCAL aStruct
   LOCAL cRDD := "DELIM"

   IF lExport
      nSrcArea := Select()
   ELSE
      nDstArea := Select()
   ENDIF

   IF Empty( aStruct := __dbStructFilter( dbStruct(), aFields ) )
      RETURN .F.
   ENDIF

   IF lExport
      dbCreate( cFile, aStruct, cRDD, .T., "", cDelimArg )
      nDstArea := Select()
      IF nDstArea == nSrcArea
         nDstArea := NIL
      ENDIF
      dbSelectArea( nSrcArea )
   ELSE
      IF ! __dbOpenSDF( cFile, aStruct, cRDD, .T., "", cDelimArg )
         RETURN .F.
      ENDIF
      nSrcArea := Select()
   ENDIF

   IF nDstArea != NIL
      __dbTrans( nDstArea, aStruct, bFor, bWhile, nNext, nRecord, lRest )
   ENDIF

   IF lExport
      IF nDstArea != NIL
         dbSelectArea( nDstArea )
         dbCloseArea()
      ENDIF
      dbSelectArea( nSrcArea )
   ELSE
      dbSelectArea( nSrcArea )
      dbCloseArea()
      dbSelectArea( nDstArea )
   ENDIF

   RETURN .T.

#else

   RETURN iif( lExport, ;
      __dbCopy( cFile, aFields, bFor, bWhile, nNext, nRecord, lRest, "DELIM",, cCodePage, cDelimArg ), ;
      __dbApp( cFile, aFields, bFor, bWhile, nNext, nRecord, lRest, "DELIM",, cCodePage, cDelimArg ) )

#endif
