/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * DataBase Function Mapped to 10 Characters length
 *
 * Copyright 2001 Luiz Rafael Culik <culik@sl.conex.net>
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
 * along with this software; see the file COPYING.  If not, write to
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

/* short (10 chars long) version of some functions for compatibility */

FUNCTION dbClearFil()
   RETURN dbClearFilter()

FUNCTION dbSetDrive( cRDD )
   RETURN dbSetDriver( cRDD )

FUNCTION dbSetRelat( xArea, bRelation, cRelation, lScoped )
   RETURN dbSetRelation( xArea, bRelation, cRelation, lScoped )

FUNCTION dbRLockLis()
    RETURN dbRLockList()

FUNCTION dbCloseAre()
   RETURN dbCloseArea()

FUNCTION dbSelectAr( xArea )
   RETURN dbSelectArea( xArea )

FUNCTION dbUnlockAl()
   RETURN dbUnlockAll()

FUNCTION dbClearRel()
   RETURN dbClearRelation()

FUNCTION dbSetFilte( bFilter, cFilter )
   RETURN dbSetFilter( bFilter, cFilter )

FUNCTION dbFieldInf( nType, nArea, xInfo )
   RETURN dbFieldInfo( nType, nArea, xInfo )

FUNCTION dbOrderInf( nInfo, cIndex, xOrder, xSet )
   RETURN dbOrderInfo( nInfo, cIndex, xOrder, xSet )

FUNCTION dbRecordIn( nInfo, nRecord, xSet )
   RETURN dbRecordInfo( nInfo, nRecord, xSet )

FUNCTION rddSetDefa( cRDD )
   RETURN rddSetDefault( cRDD )

FUNCTION __dbCopySt( cFileName, aFieldList )
   RETURN __dbCopyStruct( cFileName, aFieldList )

FUNCTION __dbCopyXS( cFileName )
   RETURN __dbCopyXStruct( cFileName )

/* ; NOTE: The created table will be kept open if lOpenMode parameter
           is of logical type. If .T. it will be opened in a new workarea,
           if .F. it will be opened in the current one. */
/* ; NOTE: Has an identical parameter list with dbCreate() */

FUNCTION __dbOpenSD( cFile, aStruct, cRDD, lOpenMode, cAlias, cDelimArg, cCodePage, nConnection )
   RETURN __dbOpenSDF( cFile, aStruct, cRDD, lOpenMode, cAlias, cDelimArg, cCodePage, nConnection )

FUNCTION __dbArrang( nToArea, aStruct, bFor, bWhile, nNext, nRecord, lRest, aFields )
   RETURN __dbArrange( nToArea, aStruct, bFor, bWhile, nNext, nRecord, lRest, aFields )

FUNCTION ordListCle()
   RETURN ordListClear()

FUNCTION ordListReb()
   RETURN ordListRebuild()

FUNCTION ordSetFocu( xOrder, cFile )
   RETURN ordSetFocus( xOrder, cFile )

FUNCTION ordSetRela( xArea, bRelation, cRelation )
   RETURN ordSetRelation( xArea, bRelation, cRelation )
