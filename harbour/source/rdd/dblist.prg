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
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */

/* NOTE: lAll is basically a dummy parameter, nothing really depends on it. 
         [vszakats] */

FUNCTION __dbList( lOff, abEval, lAll, bFor, bWhile, nNext, nRecord, lRest, lToPrint, cToFileName )
   LOCAL lOldPrinter
   LOCAL lOldExtra
   LOCAL cOldExtraFile

   LOCAL oError

   LOCAL bOutBlock

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
      IF At( ".", cToFileName ) == 0
         cToFileName := cToFileName + ".txt"
      ENDIF
      lOldExtra := Set( _SET_EXTRA, .T. )
      cOldExtraFile := Set( _SET_EXTRAFILE, cToFileName )
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
