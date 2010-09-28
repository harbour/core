/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://harbour-project.org
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
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                Xbase++ Compatible xbpFontDialog Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              02Jul2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"

/*----------------------------------------------------------------------*/

CLASS XbpFontDialog INHERIT XbpWindow

   /* Appearance */
   DATA     modalState                            INIT   0

   DATA     title                                 INIT   ""
   DATA     buttonApply                           INIT   .F.
   DATA     buttonCancel                          INIT   .T.
   DATA     buttonHelp                            INIT   .F.
   DATA     buttonOk                              INIT   .T.
   DATA     buttonReset                           INIT   .F.
   DATA     strikeOut                             INIT   .T.
   DATA     underscore                            INIT   .T.

   DATA     name                                  INIT   .T.
   DATA     style                                 INIT   .T.
   DATA     size                                  INIT   .T.

   DATA     displayFilter                         INIT   .T.
   DATA     printerFilter                         INIT   .T.

   DATA     familyName                            INIT   " "
   DATA     nominalPointSize                      INIT   0

   DATA     bitmapOnly                            INIT   .F.
   DATA     fixedOnly                             INIT   .F.
   DATA     proportionalOnly                      INIT   .T.


   DATA     outLine                               INIT   .T.
   DATA     previewBGClr                          INIT   GraMakeRGBColor( {255,255,255} )
   DATA     previewFGClr                          INIT   GraMakeRGBColor( {0,0,0} )
   DATA     previewString                         INIT   " "
   DATA     printerPS                             INIT   NIL
   DATA     screenPS                              INIT   NIL

   DATA     synthesizeFonts                       INIT   .T.

   DATA     vectorOnly                            INIT   .F.
   DATA     vectorSizes                           INIT   {}

   DATA     viewPrinterFonts                      INIT   .F.
   DATA     viewScreenFonts                       INIT   .T.

   METHOD   new( oParent, oOwner, oScreenPS, oPrinterPS, aPos )
   METHOD   create( oParent, oOwner, oScreenPS, oPrinterPS, aPos )
   METHOD   execSlot( cSlot, p )
   METHOD   display( nMode )
   METHOD   destroy()
   METHOD   XbpFontObject()

   DATA     sl_activateApply
   DATA     sl_activateCancel
   DATA     sl_activateOk
   DATA     sl_activateReset

   METHOD   activateApply( ... )                  SETGET
   METHOD   activateCancel( ... )                 SETGET
   METHOD   activateOk( ... )                     SETGET
   METHOD   activateReset( ... )                  SETGET

   DATA     oScreenPS
   DATA     oPrinterPS
   DATA     aPos                                  INIT   { 0, 0 }
   DATA     ok                                    INIT   .f.
   DATA     oFont

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpFontDialog:new( oParent, oOwner, oScreenPS, oPrinterPS, aPos )

   DEFAULT oParent    TO ::oParent
   DEFAULT oOwner     TO ::oOwner
   DEFAULT oScreenPS  TO ::oScreenPS
   DEFAULT oPrinterPS TO ::oPrinterPS
   DEFAULT aPos       TO ::aPos

   ::oParent    := oParent
   ::oOwner     := oOwner
   ::oScreenPS  := oScreenPS
   ::oPrinterPS := oPrinterPS
   ::aPos       := aPos

   ::xbpWindow:init( oParent, oOwner )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpFontDialog:create( oParent, oOwner, oScreenPS, oPrinterPS, aPos )

   DEFAULT oParent    TO ::oParent
   DEFAULT oOwner     TO ::oOwner
   DEFAULT oScreenPS  TO ::oScreenPS
   DEFAULT oPrinterPS TO ::oPrinterPS
   DEFAULT aPos       TO ::aPos

   ::oParent    := oParent
   ::oOwner     := oOwner
   ::oScreenPS  := oScreenPS
   ::oPrinterPS := oPrinterPS
   ::aPos       := aPos

   IF ::viewPrinterFonts .and. ::oPrinterPS == NIL
      ::viewPrinterFonts := .f.
   ENDIF
   IF ( ! ::viewScreenFonts .and. ! ::viewPrinterFonts )
      ::viewScreenFonts := .t.
   ENDIF

   ::xbpWindow:create( oParent, oOwner )

   ::oWidget := QFontDialog()

   if !empty( ::title )
      ::oWidget:setWindowTitle( ::title )
   ENDIF

   ::oWidget:connect( "accepted()"               , {|p| ::execSlot( "accepted()"               , p ) } )
   ::oWidget:connect( "finished(int)"            , {|p| ::execSlot( "finished(int)"            , p ) } )
   ::oWidget:connect( "rejected()"               , {|p| ::execSlot( "rejected()"               , p ) } )
   ::oWidget:connect( "currentFontChanged(QFont)", {|p| ::execSlot( "currentFontChanged(QFont)", p ) } )
   ::oWidget:connect( "fontSelected(QFont)"      , {|p| ::execSlot( "fontSelected(QFont)"      , p ) } )

   ::postCreate()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpFontDialog:execSlot( cSlot, p )
   LOCAL nRet := XBP_ALLOW

   HB_SYMBOL_UNUSED( p )

   DO CASE
   CASE cSlot == "accepted()"
      ::activateOk( ::XbpFontObject() )

   CASE cSlot == "rejected()"
      IF hb_isBlock( ::sl_quit )
         nRet := eval( ::sl_quit, 0, 0, Self )
      ENDIF
      IF nRet == XBP_REJECT
         ::oWidget:reject()
      ELSE
         ::oWidget:accept()
      ENDIF

   CASE cSlot == "currentFontChanged(QFont)"    /* SIMULATE  sl_activateApply for timebeing */
      ::activateApply( ::XbpFontObject() )

   ENDCASE

   RETURN nRet

/*----------------------------------------------------------------------*/

METHOD XbpFontDialog:display( nMode )
   LOCAL nResult

   DEFAULT nMode TO 0

   ::modalState := nMode

   /* Before Display Initialize the Variables
    */
   ::oFont := QFont()

   ::oFont:setFamily( ::familyName )
   IF ::nominalPointSize > 0
      ::oFont:setPointSize( ::nominalPointSize )
   ENDIF

   ::oWidget:setCurrentFont( ::oFont )
   IF ::aPos[ 1 ] + ::aPos[ 2 ] != 0
      ::setPos()
   ENDIF

   IF nMode == 0                                   // Parent and Modal
      nResult := ::oWidget:exec()
   ELSE                                            // Non-modal
      ::oWidget:show()
   ENDIF

   RETURN nResult

/*----------------------------------------------------------------------*/

METHOD XbpFontDialog:destroy()

   ::xbpWindow:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpFontDialog:activateApply( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_activateApply := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. hb_isBlock( ::sl_activateApply )
      eval( ::sl_activateApply, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpFontDialog:activateCancel( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_activateCancel := a_[ 1 ]
   ELSEIF len( a_ ) >= 2 .AND. hb_isBlock( ::sl_activateCancel )
      eval( ::sl_activateCancel, a_[ 1 ], a_[ 2 ], Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpFontDialog:activateOk( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_activateOk := a_[ 1 ]
   ELSEIF len( a_ ) >= 1 .AND. hb_isBlock( ::sl_activateOk )
      eval( ::sl_activateOk, a_[ 1 ], NIL, Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpFontDialog:activateReset( ... )
   LOCAL a_:= hb_aParams()
   IF len( a_ ) == 1 .AND. hb_isBlock( a_[ 1 ] )
      ::sl_activateReset := a_[ 1 ]
   ELSEIF len( a_ ) >= 2 .AND. hb_isBlock( ::sl_activateReset )
      eval( ::sl_activateReset, a_[ 1 ], a_[ 2 ], Self )
   ENDIF
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpFontDialog:XbpFontObject()
   LOCAL oXbp

   ::oFont := QFont()
   ::oFont := ::oWidget:currentFont()

   oXbp := XbpFont():new()

   oXbp:bold             := ::oFont:bold()
   oXbp:italic           := ::oFont:italic()
   oXbp:underscore       := ::oFont:underline()
   oXbp:fixed            := ::oFont:fixedPitch()

   oXbp:familyName       := ::oFont:family()
   oXbp:nominalPointSize := ::oFont:pointSize()
   oXbp:weightClass      := ::oFont:weight()

   oXbp:setCompoundName( hb_ntos( oXbp:nominalPointSize ) + "." + oXbp:familyName )

   oXbp:create()

   RETURN oXbp

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
//
//                          Class XbpFont()
//
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/


CLASS XbpFont

   DATA     oWidget

   DATA     hFont
   DATA     oPS
   DATA     hdc
   DATA     oFontInfo

   DATA     familyName                            INIT   ""
   DATA     height                                INIT   0
   DATA     nominalPointSize                      INIT   0

   DATA     width                                 INIT   0
   DATA     widthClass                            INIT   .F.

   DATA     bold                                  INIT   .F.
   DATA     weightClass                           INIT   0 //FW_DONTCARE

   DATA     italic                                INIT   .F.
   DATA     strikeout                             INIT   .F.
   DATA     underscore                            INIT   .F.
   DATA     codePage                              INIT   0 //DEFAULT_CHARSET

   DATA     fixed                                 INIT   .F.
   DATA     antiAliased                           INIT   .F.

   DATA     compoundName                          INIT   ""               READONLY
   METHOD   setCompoundName( cName )              INLINE ::compoundName := cName

   DATA     generic                               INIT   .T.

   DATA     baseLine                              INIT   0                READONLY
   DATA     dbcs                                  INIT   .F.
   DATA     kerning                               INIT   .F.
   DATA     mbcs                                  INIT   .F.
   DATA     vector                                INIT   .F.
   DATA     outlined                              INIT   .F.

   DATA     aFontInfo                             INIT   {}

   METHOD   new( oPS )
   METHOD   create( cFontName )
   METHOD   configure( cFontName )
   METHOD   destroy()
   METHOD   list()
   METHOD   createFont()

   DESTRUCTOR _destroy()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpFont:new( oPS )

   DEFAULT oPS TO ::oPS

   ::oPS := oPS

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpFont:create( cFontName )
   LOCAL n, cFont, nPtSize

   IF !empty( cFontName )
      IF ( n := at( '.', cFontName ) ) > 0
         cFont   := '"' + substr( cFontName, n + 1 ) + '"'
         nPtSize := val( substr( cFontName, 1, n - 1 ) )
      ENDIF
   ENDIF
   IF empty( cFont )
      cFont   := ::familyName
      nPtSize := ::nominalPointSize
   ENDIF

   IF empty( nPtSize )
      nPtSize := 12
   ENDIF

   ::oWidget := QFont( cFont, nPtSize )

   ::oWidget:setBold( ::bold )
   ::oWidget:setItalic( ::italic )
   ::oWidget:setUnderline( ::underscore )
   ::oWidget:setStrikeOut( ::strikeout )
   ::oWidget:setFixedPitch( ::fixed )
   ::oWidget:setKerning( ::kerning )


   #if 0
   ::oWidget:setStretch( factor )
   //
   ::oWidget:setCapitalization( caps )
   ::oWidget:setFamily( family )
   ::oWidget:setLetterSpacing( type, spacing )
   ::oWidget:setOverline( enable )
   ::oWidget:setPixelSize( pixelSize )
   ::oWidget:setPointSize( pointSize )
   ::oWidget:setPointSizeF( pointSize )
   ::oWidget:setRawMode( enable )
   ::oWidget:setRawName( name )
   ::oWidget:setStyle( style )
   ::oWidget:setStyleHint( StyleHint hint, StyleStrategy strategy = PreferDefault )
   ::oWidget:setStyleStrategy( StyleStrategy )
   ::oWidget:setWeight( weight )
   ::oWidget:setWordSpacing( spacing )
   #endif

   /* Initializes the font structures internally */
   //::oWidget:initialize()

   /* Call the final step - beyond that any changes to properties above will have NO effect */
   ::oFontInfo := QFontInfo( ::oWidget )

   /* Reassign actual properties */
   ::bold        := ::oFontInfo:bold()
   ::italic      := ::oFontInfo:italic()
   ::fixed       := ::oFontInfo:fixedPitch()
   ::familyName  := ::oFontInfo:family()
   ::height      := ::oFontInfo:pointSize()
   ::weightClass := ::oFontInfo:weight()

   #if 0
   bool    exactMatch () const
   int     pixelSize () const
   qreal   pointSizeF () const
   bool    rawMode () const
   QFont::Style style () const
   QFont::StyleHint styleHint () const
   #endif
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpFont:configure( cFontName )

   HB_SYMBOL_UNUSED( cFontName )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpFont:destroy()

   ::oFontInfo := NIL
   ::oWidget   := NIL

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpFont:_destroy()

   ::oFontInfo := NIL
   ::oWidget   := NIL

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD XbpFont:list()
   LOCAL aList := {}

   RETURN aList

/*----------------------------------------------------------------------*/

METHOD XbpFont:createFont()
   LOCAL aFont := {}

   IF ::hFont <> NIL
      // Win_DeleteObject( ::hFont )
      ::hFont := NIL
   ENDIF

   IF ::oPS <> NIL
      //::height := xbp_PointSizeToHeight( ::oPS:hdc, ::nominalPointSize )
   ENDIF

   ::aFontInfo := array( 15 )

   ::aFontInfo[  1 ] := ::familyName
   ::aFontInfo[  2 ] := ::height
   ::aFontInfo[  3 ] := ::width
   ::aFontInfo[  4 ] := IF( ::bold, 75, -1 )
   ::aFontInfo[  5 ] := ::italic
   ::aFontInfo[  6 ] := ::underscore
   ::aFontInfo[  7 ] := ::strikeout
   ::aFontInfo[  8 ] := ::codePage
   ::aFontInfo[  9 ] := 0
   ::aFontInfo[ 10 ] := 0
   ::aFontInfo[ 11 ] := 0
   ::aFontInfo[ 12 ] := 0
   ::aFontInfo[ 13 ] := 0 //DEFAULT_QUALITY
   ::aFontInfo[ 14 ] := NIL

   //aFont := Xbp_FontCreate( ::aFontInfo )

   IF empty( aFont[ 1 ] )
      RETURN nil
   ENDIF

   ::hFont     := aFont[ 15 ]
   ::aFontInfo := aFont

   RETURN ::hFont

/*----------------------------------------------------------------------*/
