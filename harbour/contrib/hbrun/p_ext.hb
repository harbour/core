/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * extensions (dynamic manager plugin)
 *
 * Copyright 2012 Viktor Szakats (harbour syenar.net)
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

FUNCTION __hbrun_plugin()
   RETURN {;
      "id"   => "ext",;
      "init" => {| hConIO | __init( hConIO ) } ,;
      "exit" => {| context | HB_SYMBOL_UNUSED( context ) } ,;
      "cmd"  => {| context, cCommand | __command( context, cCommand ) } }

STATIC FUNCTION __init( hConIO )
   RETURN { hConIO, { ;
      "load"   => { "<name>" , "Load."   , {| context, cCommand | load( context, cCommand ) } },;
      "unload" => { "<name>" , "Unload." , {| context, cCommand | unload( context, cCommand ) } },;
      "list"   => { ""       , "List."   , {| context, cCommand | list( context ) } } } }

STATIC PROCEDURE __disp( context, cText )
   Eval( context[ 1 ][ "displine" ], cText )
   RETURN

STATIC FUNCTION __command( context, cCommand )
   LOCAL aCommand
   LOCAL nPos

   IF ! Empty( context )
      aCommand := hb_ATokens( cCommand, " " )
      IF ! Empty( aCommand ) .AND. ( nPos := hb_HPos( context[ 2 ], Lower( aCommand[ 1 ] ) ) ) > 0
         Eval( hb_HValueAt( context[ 2 ], nPos )[ 3 ], context, cCommand )
         RETURN .T.
      ENDIF
   ENDIF

   RETURN .F.

/* Commands */

STATIC PROCEDURE load( context, cCommand )
   LOCAL aToken := hb_ATokens( cCommand, " " )
   LOCAL tmp

   FOR tmp := 2 TO Len( aToken )
      __hbrun_extensions_dynamic_load( aToken[ tmp ] )
   NEXT

   RETURN

STATIC PROCEDURE unload( context, cCommand )
   LOCAL aToken := hb_ATokens( cCommand, " " )
   LOCAL tmp

   FOR tmp := 2 TO Len( aToken )
      __hbrun_extensions_dynamic_unload( aToken[ tmp ] )
   NEXT

   RETURN

STATIC PROCEDURE list( context )
   LOCAL cName

   FOR EACH cName IN __hbrun_extensions_get_list()
      __disp( context, cName )
   NEXT

   RETURN
