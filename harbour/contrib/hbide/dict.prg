/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                 Pritpal Bedi <bedipritpal@hotmail.com>
 *                               11Jun2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "common.ch"
#include "hbclass.ch"
#include "hbqtgui.ch"
#include "hbide.ch"
#include "xbp.ch"

/*----------------------------------------------------------------------*/

#define DIC_FILENAME                              1
#define DIC_CASESENSTITIVE                        2
#define DIC_CONVMODE                              3
#define DIC_AUTOCOMPLETE                          4
#define DIC_BGCOLOR                               5

#define DIC_NUM_VRBLS                             5

/*----------------------------------------------------------------------*/

FUNCTION hbide_loadUserDictionaries( oIde )
   //                File  , CaseSensitive , ConvMode=asis, upper, lower , include in autocomplete, bgColor
   #if 1
   LOCAL aDict := { "E:\harbour\contrib\hbide\projects\my.dic;NO;ASIS;YES;{122,133,233}" }
   #else
   LOCAL aDict := {}
   #endif
   LOCAL oDict, i

   FOR i := 1 TO Len( aDict )
      oDict := IdeDictionary():new( oIde ):create()
      oDict:load( aDict[ i ] )

      aadd( oIde:aUserDict, oDict )
   NEXT

   RETURN NIL

/*----------------------------------------------------------------------*/

CLASS IdeDictionary INHERIT IdeObject

   DATA   cDictInfo                               INIT ""
   DATA   cFilename                               INIT ""
   DATA   lActive                                 INIT .T.
   DATA   lToPrg                                  INIT .T.
   DATA   lToC                                    INIT .F.
   DATA   lToCPP                                  INIT .F.
   DATA   lToCH                                   INIT .F.
   DATA   lToH                                    INIT .F.
   DATA   lToIni                                  INIT .F.
   DATA   lToTxt                                  INIT .F.
   DATA   lToHbp                                  INIT .F.
   DATA   cConvMode                               INIT "ASIS"
   DATA   lCaseSensitive                          INIT .F.
   DATA   lBold                                   INIT .F.
   DATA   lItalic                                 INIT .F.
   DATA   lULine                                  INIT .F.
   DATA   lTxtColor                               INIT .F.
   DATA   lBgBColor                               INIT .F.
   DATA   cTxtColor                               INIT ""
   DATA   cBgColor                                INIT ""

   DATA   qBGColor
   DATA   lAutoComplete                           INIT .T.
   DATA   aItems                                  INIT {}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()                               VIRTUAL
   METHOD load( cDict )
   METHOD toString()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:new( oIde )
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:create( oIde )
   DEFAULT oIde TO ::oIde
   ::oIde := oIde
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:load( cDict )
   LOCAL a_:= hb_aTokens( cDict, ";" )
   LOCAL s, b_, n, n1, cKeyword, cSyntax, cDesc, c_

   IF !empty( a_ ) .AND. HB_ISARRAY( a_ )
      asize( a_, DIC_NUM_VRBLS )

      DEFAULT a_[ DIC_FILENAME       ] TO ""
      DEFAULT a_[ DIC_CASESENSTITIVE ] TO "NO"
      DEFAULT a_[ DIC_CONVMODE       ] TO "ASIS"
      DEFAULT a_[ DIC_AUTOCOMPLETE   ] TO "YES"
      DEFAULT a_[ DIC_BGCOLOR        ] TO "NONE"

      ::cDictInfo       := cDict
      ::cFilename       := a_[ 1 ]
      ::lCaseSensitive  := a_[ 2 ] == "YES"
      ::cConvMode       := a_[ 3 ]
      ::lAutoComplete   := iif( a_[ 4 ] == "NO", .f., .t. )
      ::cBgColor        := a_[ 5 ]

      IF !( ::cBgColor == "NONE" )
         c_:= hbide_evalAsIs( ::cBgColor )
         IF HB_ISARRAY( c_ ) .AND. Len( c_ ) == 3
            ::qBgColor := QColor( c_[ 1 ], c_[ 2 ], c_[ 3 ] )
         ENDIF
      ENDIF

      IF !empty( a_[ DIC_FILENAME ] ) .AND. hb_fileExists( a_[ DIC_FILENAME ] )
         b_:= hbide_readSource( a_[ DIC_FILENAME ] )

         FOR EACH s IN b_
            s := alltrim( s )
            IF empty( s )
               LOOP
            ENDIF
            cKeyword := ""
            cSyntax  := ""
            cDesc    := ""
            IF ( n := at( "(", s ) ) > 0
               IF ( n1 := at( ")", s ) ) > 0
                  cKeyword := alltrim( substr( s, 1, n - 1 ) )
                  cSyntax  := strtran( substr( s, 1, n1 ), " (", "(" )
                  cDesc    := alltrim( substr( s, n1 + 1 ) )
               ENDIF
            ELSE
               cKeyword := s
            ENDIF

            IF !empty( cKeyword )
               aadd( ::aItems, { cKeyword, cSyntax, cDesc } )
            ENDIF
         NEXT
      ENDIF

   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeDictionary:toString()
   LOCAL cDict := ""

   RETURN cDict

/*----------------------------------------------------------------------*/
