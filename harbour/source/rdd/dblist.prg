/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * __DBLIST() function
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 * www - http://www.harbour-project.org
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
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 RonPinkas <Ron@Profit-Master.com>
 * HB_aExpressions()
 *
*/

/* NOTE: lAll is basically a dummy parameter, nothing really depends on it.
         [vszakats] */

#include "set.ch"

FUNCTION __dbList( lOff, abEval, lAll, bFor, bWhile, nNext, nRecord, lRest, lToPrint, cToFileName )

   LOCAL lOldPrinter
   LOCAL lOldExtra
   LOCAL cOldExtraFile
   LOCAL cPath
   LOCAL cName
   LOCAL cExt

   LOCAL oError

   LOCAL bOutBlock

   LOCAL nLen := Len( abEval ), nIndex, asMacros, nMacros, nMacroIndex

   // Scan for strings instead of blocks - These are macros that need to be compiled into blocks.
   FOR nIndex := 1 TO nLen
     IF ValType( abEval[ nIndex ] ) == 'C'
        //? abEval[ nIndex ]
        // Macro may be a comma seperated list.
        asMacros := HB_aExpressions( abEval[ nIndex ] )
        nMacros  := Len( asMacros )

        // Array has to be sized to allow dor the extra blocks
        nLen += ( nMacros - 1 )
        aSize( abEval, nLen )

        // We will use the place holder of the string for the first new block.
        abEval[ nIndex ] := &( "{||" + asMacros[ 1 ] + "}" )

        // We will now push all subsequent blocks 1 at a time and insert the new block inplace.
        FOR nMacroIndex := 2 TO nMacros
           aIns( abEval, nIndex + nMacroIndex - 1 )
           abEval[ nIndex + nMacroIndex - 1 ] := &( "{||" + asMacros[ nMacroIndex ] + "}" )
        NEXT

        // The loop counter should skip the new elements.
        nIndex += ( nMacros - 1 )
     ENDIF
   NEXT

   /* Choose the output style */
   IF lOff
      bOutBlock := {|| QOut( iif( Deleted(), "*", " " ) ),;
                       aEval( abEval, {| bEval | QQOut( Eval( bEval ), "" ) } ) }
   ELSE
      bOutBlock := {|| QOut( Str( RecNo(), 7 ), iif( Deleted(), "*", " " ) ),;
                       aEval( abEval, {| bEval | QQOut( Eval( bEval ), "" ) } ) }
   ENDIF

   /* Save SETs */

   IF !Empty( lToPrint )
      lOldPrinter := Set(_SET_PRINTER, .T. )
   ENDIF
   IF !Empty( cToFileName )
      hb_FNameSplit( cToFileName, @cPath, @cName, @cExt )
      IF Empty( cExt )
         cExt := ".txt"
      ENDIF
      lOldExtra := Set( _SET_EXTRA, .T. )
      cOldExtraFile := Set( _SET_EXTRAFILE, hb_FNameMerge( @cPath, @cName, @cExt ) )
   ENDIF

   /* Do the job */

   BEGIN SEQUENCE

      IF Empty( lAll ) .AND. ;
         Empty( bFor ) .AND. ;
         Empty( bWhile ) .AND. ;
         Empty( nNext ) .AND. ;
         Empty( nRecord ) .AND. ;
         Empty( lRest )

         Eval( bOutBlock )
      ELSE
         dbEval( bOutBlock, bFor, bWhile, nNext, nRecord, lRest )
      ENDIF

   RECOVER USING oError
   END SEQUENCE

   /* Restor SETs */

   IF !Empty( lToPrint )
      Set( _SET_PRINTER, lOldPrinter )
   ENDIF
   IF !Empty( cToFileName )
      Set( _SET_EXTRAFILE, cOldExtraFile )
      Set( _SET_EXTRA, lOldExtra )
   ENDIF

   /* On error signal the error for the higher level error handler or quit */

   IF oError != NIL
      Break( oError )
   ENDIF

RETURN NIL
