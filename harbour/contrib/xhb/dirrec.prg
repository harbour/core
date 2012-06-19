/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    xHarbour compatible function DirectoryRecurse()
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

/* This implementation uses different rules then xHarbour one.
 * It does not change current drive or current directory so
 * unlike the xHarbour version it's MT safe.
 * It also returns relative paths which are more similar to
 * DIRECTORY() function results so they can be easy used
 * directly in other code, f.e. to create archive without
 * absolute paths. Please note that user can easy convert
 * relative paths to absolte ones by simple adding curdir()
 * and/or cPath parameter passed to DirectoryRecurse() but
 * reverted conversion may not be possible in some cases.
 * The 3-rd xHarbour parameter <lCaseMach> is ignored because
 * harbour uses platform native rules to check filename mask
 * respecting SET FILECASE and SET DIRCASE settings.
 * xHarbour does not add "D" to attribute list used for directory
 * tree scanning so user always have to add it manually and later
 * it ignores it so it's not possible to extract file list with
 * directories entries. In Harbour it's fixed.
 * [druzus]
 */

#include "directry.ch"

FUNCTION DirectoryRecurse( cPath, cAttr )

   LOCAL aResult
   LOCAL cFilePath, cExt, cMask

   hb_FNameSplit( cPath, @cFilePath, @cMask, @cExt )
   cMask += cExt
   IF ! HB_ISSTRING( cAttr )
      cAttr := ""
   ENDIF
   /* The trick with StrTran() below if for strict xHarbour
    * compatibility though it should be reverted when it will
    * be fixed in xHarbour
    */
   aResult := HB_DirScan( cFilePath, cMask, ;
                          StrTran( Upper( cAttr ), "D" ) )

   AEval( aResult, {| x | x[ F_NAME ] := cFilePath + x[ F_NAME ] } )

   RETURN aResult
