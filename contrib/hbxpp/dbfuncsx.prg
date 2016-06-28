/*
 * Xbase++ compatible _dbExport() function
 *
 * Copyright 1999-2007 Viktor Szakats (vszakats.net/harbour)
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

FUNCTION _dbExport( cFile, aFields, bFor, bWhile, nNext, nRecord, lRest, cXPP_Driver, cDelimiter )

   SWITCH hb_defaultValue( cXPP_Driver, "" )
   CASE "SDFDBE"
      RETURN __dbCopy( cFile, aFields, bFor, bWhile, nNext, nRecord, lRest, "SDF" )
#if 0
      /* Alternate CA-Cl*pper compatible call: */
      RETURN __dbSDF( .T., cFile, aFields, bFor, bWhile, nNext, nRecord, lRest )
#endif
   CASE "DELDBE"
      RETURN __dbCopy( cFile, aFields, bFor, bWhile, nNext, nRecord, lRest, "DELIM", , , cDelimiter )
#if 0
      /* Alternate CA-Cl*pper compatible call: */
      RETURN __dbDelim( .T., cFile, cDelimiter, aFields, bFor, bWhile, nNext, nRecord, lRest )
#endif
   ENDSWITCH

   RETURN __dbCopy( cFile, aFields, bFor, bWhile, nNext, nRecord, lRest, cXPP_Driver )

FUNCTION xpp_dbUseArea( lNewArea, cDriver, cName, xcAlias, lShared, lReadonly )

   LOCAL nOldArea
   LOCAL nArea

   IF hb_defaultValue( lNewArea, .F. )

      hb_default( @xcAlias, "" )

      IF Empty( xcAlias )
         xcAlias := cName
      ENDIF

      IF HB_ISSTRING( xcAlias )
         nOldArea := Select()
         IF ( nArea := Select( xcAlias ) ) > 0
            xcAlias += "_" + hb_ntos( nArea )
         ENDIF
         dbSelectArea( nOldArea )
      ENDIF
   ENDIF

   RETURN dbUseArea( lNewArea, cDriver, cName, xcAlias, lShared, lReadonly )
