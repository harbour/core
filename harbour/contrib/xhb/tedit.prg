/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Editing and Forms Class for HTMLLIB
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
 * www - http://harbour-project.org
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "hbclass.ch"
#include "cgi.ch"

#define _OPTION_TEXT     1
#define _OPTION_VALUE    2
#define _OPTION_LABEL    3
#define _OPTION_SELECTED 4
#define _OPTION_DISABLED 5

STATIC s_oForm

/****
*
*     Class THtmlControl()
*
*/

CREATE CLASS THtmlControl

   VAR nH
   VAR Document
   VAR FORM
   VAR oHtm

   VAR Caption
   VAR lBreak INIT .F.

   VAR TYPE INIT "TEXT"
   VAR Name
   VAR Value
   VAR Size
   VAR ROWS, COLS

   VAR cOutPut INIT ""

   VAR PICTURE INIT "@X"

   VAR aOptions INIT {} // SELECT options

   VAR TabIndex INIT 0
   VAR disabled INIT .F.
   VAR READONLY INIT .F.
   VAR Multiple INIT .F.
   VAR style
   VAR ID

   VAR MaxLength // length in chars
   VAR MaxChars // length on screen

   VAR Checked INIT .F. // checkboxes...
   VAR lLabel INIT .F.

   VAR Source, Align // images...
   VAR WRAP // textarea

   VAR onBlur
   VAR onChange
   VAR onFocus
   VAR onSelect
   VAR onClick
   VAR onMouseOver
   VAR onMouseOut
   VAR onMouseDown
   VAR onMouseup
   VAR onKeyPress
   VAR onKeyDown
   VAR onKeyUp

   METHOD SetName( c ) INLINE ::Name := c

   METHOD SetValue( c ) INLINE ::Value := c

   METHOD SetStyle( c ) INLINE ::Style := c

   METHOD SetId( c ) INLINE ::id := c

   METHOD SetRows( c ) INLINE ::Rows := c

   METHOD SetCols( c ) INLINE ::Cols := c

   METHOD SetCaption( c ) INLINE ::Caption := c

   METHOD SetPicture( c ) INLINE ::picture := c

   METHOD SetOnBlur( c ) INLINE ::onBlur := c

   METHOD SetOnChange( c ) INLINE ::onChange := c

   METHOD SetOnFocus( c ) INLINE ::onFocus := c

   METHOD SetOnSelect( c ) INLINE ::onSelect := c

   METHOD SetOnClick( c ) INLINE ::onClick := c

   METHOD SetOnMsOver( c ) INLINE ::onMouseOver := c

   METHOD SetOnMsOut( c ) INLINE ::onMouseOut := c

   METHOD SetSize( n ) INLINE ::Size := n

   METHOD SetMaxChars( n ) INLINE ::MaxChars := n

   METHOD SetChecked( l ) INLINE ::Checked := l

   METHOD SetAlign( c ) INLINE ::Align := c

   METHOD SetWrap( c ) INLINE ::wrap := c

   METHOD SetSource( c ) INLINE ::Source := c

   METHOD SetReadOnly( l ) INLINE ::readOnly := l

   METHOD SetDisabled( l ) INLINE ::disabled := l

   METHOD SetMultiple( l ) INLINE ::multiple := l

   METHOD SetOnMsDown( c ) INLINE ::onMouseDown := c

   METHOD SetOnMsUp( c ) INLINE ::onMouseup := c

   METHOD SetOnKPress( c ) INLINE ::onKeyPress := c

   METHOD SetOnKDown( c ) INLINE ::onKeyDown := c

   METHOD SetOnKUp( c ) INLINE ::onKeyUp := c

   METHOD SetLabel( l ) INLINE ::lLabel := l

   METHOD Put()

   METHOD AddOption( cOption, cValue, cLabel, lSelected, lDisabled )

   METHOD SetControl( name, rows, cols, size, maxchars, value, onfocus, ;
      onblur, onchange, onselect, onclick, onmsover, onmsout, ;
      onmsdown, onmsup, onkdown, onkup, onkprs, ;
      pic, cap, dis, ro, lMulti, checked, ;
      align, wrap, type, Style, ID, lLabel )

ENDCLASS

/****
*
*     THtmlControl():Put()
*
*/

METHOD Put() CLASS THtmlControl

   LOCAL i
   LOCAL cStr := ""

   ::nH := HtmlPageHandle()
   ::form := HtmlFormName()
   ::oHtm := HtmlPageObject()

   ::cOutput += iif( ::lBreak, CRLF() + "<BR>", CRLF() )
   IF ::lLabel
      ::cOutPut += CRLF() + "<LABEL>" + CRLF()
   ENDIF

   IF ::Caption != NIL
      ::cOutput += ::Caption + HTMLSPACE( 2 ) + " <!-- " + ::Type + " Control Caption -->" + CRLF()
   ENDIF

   IF ::Type == "SELECT"
      ::cOutPut += CRLF() + ' <SELECT ' + CRLF()
   ELSE
      ::cOutPut += CRLF() + ' <INPUT TYPE="'
      ::cOutput += ::Type + '"' + CRLF()
   ENDIF

   IF ::Name != NIL
      ::cOutput += Space( 2 ) + '     NAME="' + ::Name + '"' + CRLF()
   ENDIF

   IF ::Type == "TEXTAREA"

      IF ::Rows != NIL
         ::cOutput += Space( 2 ) + '     ROWS="' + hb_ntos( ::Rows ) + '"' + CRLF()
      ENDIF

      IF ::Cols != NIL
         ::cOutput += Space( 2 ) + '     COLS="' + hb_ntos( ::Cols ) + '"' + CRLF()
      ENDIF

      IF ::Wrap != NIL
         ::cOutput += Space( 2 ) + '     WRAP="' + ::Wrap + '"' + CRLF()
      ENDIF

   ENDIF

   IF ::value != NIL

      IF ::Picture == NIL
         ::Picture := "@X"
      ENDIF

      ::cOutput += Space( 2 ) + '    VALUE="' + Transform( ::Value, ::Picture ) + '"' + CRLF()
   ENDIF

   IF ::maxChars != NIL
      ::cOutput += Space( 2 ) + 'MAXLENGTH="' + hb_ntos( ::maxChars ) + '"' + CRLF()
   ENDIF

   IF ::Size != NIL
      ::cOutput += Space( 2 ) + '     SIZE="' + hb_ntos( ::Size ) + '"' + CRLF()
   ENDIF

   IF ::Id != NIL
      ::cOutput += Space( 2 ) + '       ID="' + ::Id + '"' + CRLF()
   ENDIF

   IF ::Style != NIL
      ::cOutput += Space( 2 ) + '    STYLE="' + ::Style + '"' + CRLF()
   ENDIF

   IF ::type == "IMAGE"

      IF ::Source != NIL
         ::cOutput += Space( 2 ) + '      SRC="' + ::Source + '"' + CRLF()
      ENDIF

   ENDIF

   IF ::Align != NIL
      ::cOutput += Space( 2 ) + '    ALIGN="' + ::Align + '"' + CRLF()
   ENDIF

   IF ::type == "RADIO" .OR. ::type == "CHECKBOX"

      IF ::Checked
         ::cOutput += Space( 2 ) + '   CHECKED' + CRLF()
      ENDIF

   ENDIF

   IF ::Disabled
      ::cOutput += Space( 2 ) + '  DISABLED' + CRLF()
   ENDIF

   IF ::Readonly
      ::cOutput += Space( 2 ) + '  READONLY' + CRLF()
   ENDIF

   IF ::onChange != NIL
      ::cOutput += Space( 2 ) + ' onChange="' + ::onChange + '"' + CRLF()
   ENDIF

   IF ::onFocus != NIL
      ::cOutput += Space( 2 ) + '  onFocus="' + ::onFocus + '"' + CRLF()
   ENDIF

   IF ::onBlur != NIL
      ::cOutput += Space( 2 ) + '   onBlur="' + ::onBlur + '"' + CRLF()
   ENDIF

   IF ::onSelect != NIL
      ::cOutput += Space( 2 ) + ' onSelect="' + ::onSelect + '"' + CRLF()
   ENDIF

   IF ::onClick != NIL
      ::cOutput += Space( 2 ) + '  onClick="' + ::onClick + '"' + CRLF()
   ENDIF

   IF ::onMouseOver != NIL
      ::cOutput += 'onMouseOver="' + ::onMouseOver + '"' + CRLF()
   ENDIF

   IF ::onMouseOut != NIL
      ::cOutput += Space( 1 ) + 'onMouseOut="' + ::onMouseOut + '"' + CRLF()
   ENDIF

   IF ::onMouseDown != NIL
      ::cOutput += 'onMouseDown="' + ::onMouseDown + '"' + CRLF()
   ENDIF

   IF ::onMouseUp != NIL
      ::cOutput += Space( 1 ) + 'onMouseUp="' + ::onMouseUp + '"' + CRLF()
   ENDIF

   IF ::onKeyDown != NIL
      ::cOutput += 'onKeyDown="' + ::onKeyDown + '"' + CRLF()
   ENDIF

   IF ::onKeyUp != NIL
      ::cOutput += Space( 1 ) + 'onKeyUp="' + ::onKeyUp + '"' + CRLF()
   ENDIF

   IF ::onKeyPress != NIL
      ::cOutput += Space( 1 ) + 'onKeyPress="' + ::onKeyPress + '"' + CRLF()
   ENDIF

   ::cOutput += " >" + CRLF()

   IF ::lLabel
      ::cOutPut += CRLF() + "</LABEL>" + CRLF()
   ENDIF

// FWrite( ::nH, ::cOutput )
   ::oHtm:cStr += ::cOutput

   IF ::Type == "SELECT"

      FOR i := 1 TO Len( ::aOptions )
         cStr := "<OPTION"
         cStr += iif( ::aOptions[ i, _OPTION_VALUE ] != NIL, ;
            " value=" + ::aOptions[ i, _OPTION_VALUE ], "" )
         cStr += iif( ::aOptions[ i, _OPTION_LABEL ] != NIL, ;
            " label=" + ::aOptions[ i, _OPTION_LABEL ], "" )
         cStr += iif( ::aOptions[ i, _OPTION_SELECTED ] == .T., ;
            " SELECTED ", "" )
         cStr += iif( ::aOptions[ i, _OPTION_DISABLED ] == .T., ;
            " DISABLED ", "" )
         cStr += ">" + ::aOptions[ i, _OPTION_TEXT ] + "</OPTION>" + CRLF()
//       FWrite( ::nH, cStr )
         ::oHtm:cStr += cStr
      NEXT

//    FWrite( ::nH, "</SELECT>" )
      ::oHtm:cStr += "</SELECT>"
   ENDIF

   RETURN Self

/****
*
*     THtmlControl():AddOption()
*
*/

METHOD AddOption( cOption, cValue, cLabel, lSelected, lDisabled ) CLASS THtmlControl

   AAdd( ::aOptions, { cOption, cValue, cLabel, lSelected, lDisabled } )

   RETURN Self

/****
*
*     THtmlControl():setControl()
*
*     Batch set control properties/methods
*
*/

METHOD SetControl( name, rows, cols, size, maxchars, value, onfocus, ;
      onblur, onchange, onselect, onclick, onmsover, onmsout, ;
      onmsdown, onmsup, onkdown, onkup, onkprs, ;
      pic, cap, dis, ro, lMulti, checked, ;
      align, wrap, type, Style, ID, lLabel ) CLASS THtmlControl

   ::name := name
   ::rows := ROWS
   ::cols := COLS
   ::size := size
   ::maxChars := maxchars
   ::value := value
   ::onFocus := onfocus
   ::onBlur := onblur
   ::onChange := onchange
   ::onSelect := onselect
   ::onClick := onclick
   ::onMouseOver := onmsover
   ::onMouseOut := onmsout
   ::onMouseDown := onmsdown
   ::onMouseUp := onmsup
   ::onKeyDown := onkdown
   ::onKeyUp := onkup
   ::onKeyPress := onkprs
   ::picture := pic
   ::caption := cap
   ::disabled := dis
   ::readonly := ro
   ::Multiple := lMulti
   ::Checked := checked
   ::Align := align
   ::Wrap := wrap
   ::type := type
   ::Style := Style
   ::Id := ID
   ::lLabel := lLabel

   RETURN Self

/****
*
*     Class THtmlForm()
*
*/

CREATE CLASS THtmlForm

   VAR nH
   VAR oHtm

   VAR aControls INIT {}
   VAR Name INIT ""
   VAR Action
   VAR METHOD
   VAR EncType INIT "multipart/form-data"
   VAR onSubmit
   VAR onReset
   VAR Target
   VAR Frame INIT .F.
   VAR Caption INIT ""
   VAR CaptionColor INIT "black"
   VAR CapFontColor INIT "white"
   VAR CaptionImage
   VAR BGIMAGE
   VAR fontColor INIT "black"
   VAR COLOR INIT "#9196A0"
   VAR WIDTH INIT 90

   VAR cOutput INIT ""

   METHOD setHandle( h ) INLINE ::nH := h

   METHOD setName( c ) INLINE ::Name := c

   METHOD setAction( c ) INLINE ::Action := c

   METHOD setMethod( c ) INLINE ::Method := c

   METHOD setEncType( c ) INLINE ::encType := c

   METHOD setOnSubmit( c ) INLINE ::onSubmit := c

   METHOD setOnReset( c ) INLINE ::onReset := c

   METHOD setTarget( c ) INLINE ::Target := c

   METHOD setCapClr( c ) INLINE ::CaptionColor := c

   METHOD setCapFntClr( c ) INLINE ::CapFontColor := c

   METHOD setCapImage( c ) INLINE ::CaptionImage := c

   METHOD setBgImage( c ) INLINE ::bgImage := c

   METHOD setFontColor( c ) INLINE ::FontColor := c

   METHOD setFrmColor( c ) INLINE ::Color := c

   METHOD setwidth( c ) INLINE ::width := c

   METHOD AddControl( o ) INLINE iif( HB_ISOBJECT( o ), ( o:nH := ::nH, o:Form := Self ), ), ;
      AAdd( ::aControls, o )

   METHOD PutControls() INLINE AEval( ::aControls, {| e | e:Put() } )

   METHOD New( cName, cAction, cMethod, lFrame, cCaption, nWidth )

   METHOD Put( lPutControls )

   METHOD END ()

   METHOD GetControl( cName )

ENDCLASS

/****
*
*     THtmlForm():New()
*
*/

METHOD New( cName, cAction, cMethod, lFrame, cCaption, nWidth ) CLASS THtmlForm

   HB_SYMBOL_UNUSED( cAction )

   __defaultNIL( @cName, "Form1" )
   __defaultNIL( @cMethod, "POST" )
   __defaultNIL( @lFrame, .F. )
   __defaultNIL( @cCaption, "" )
   __defaultNIL( @nWidth, 90 )

   ::oHtm := HtmlPageObject()
   ::Name := cName
   ::Method := cMethod

   ::nH := HtmlPageHandle()

   ::Frame := lFrame
   ::Caption := cCaption
   ::width := nWidth

   ::aControls := {}

   s_oForm := Self

   RETURN Self

/****
*
*     THtmlForm():Put()
*
*/

METHOD Put( lPutControls ) CLASS THtmlForm

   __defaultNIL( @lPutControls, .F. )

   IF ! HB_ISNUMERIC( ::width )
      ::width := 90
   ENDIF

   IF ! HB_ISSTRING( ::color )
      ::Color := "#9196A0"
   ENDIF

   IF ! HB_ISSTRING( ::fontColor )
      ::fontColor := "black"
   ENDIF

   IF ! HB_ISSTRING( ::CaptionColor )
      ::CaptionColor := "black"
   ENDIF

   IF ::Frame
      ::cOutPut := CRLF() + CRLF() + CRLF() + "<!-------  Start of Form ------->" + CRLF() + CRLF()
//    FWrite( ::nH, ::cOutput )
      ::oHtm:cStr += ::cOutput
      ::cOutPut := '<TABLE BGCOLOR="#9196A0" ' + CRLF() + ;
         "       COLS=1 " + CRLF() + ;
         "       ROWS=1 " + CRLF() + ;
         "       CELLPADDING=3 " + CRLF() + ;
         "       CELLSPACING=3 " + CRLF() + ;
         "       WIDTH=" + hb_ntos( ::width ) + "% " + CRLF() + ;
         '       BORDERCOLORLIGHT="#000000" ' + CRLF() + ;
         '       BORDERCOLORDARK="#FFFFFF" ' + CRLF() + ;
         "       BORDER " + CRLF() + ;
         "       >" + CRLF()
//    FWrite( ::nH, ::cOutput )
      ::oHtm:cStr += ::cOutput
      ::cOutPut := '<TR BGCOLOR="' + ::captionColor + '">' + CRLF()

      IF ::Caption != NIL
         ::cOutPut := "<TD"
//       FWrite( ::nH, ::cOutput )
         ::oHtm:cStr += ::cOutput

         IF ::captionImage != NIL
            ::cOutPut := ' BACKGROUND="' + ::captionImage + '"' + CRLF()
//          FWrite( ::nH, ::cOutput )
            ::oHtm:cStr += ::cOutput
         ENDIF

         ::cOutPut := '>'
//       FWrite( ::nH, ::cOutput )
         ::oHtm:cStr += ::cOutput

         IF ::capFontColor != NIL
            ::cOutPut := '<FONT COLOR="' + ::capFontColor + '">' + CRLF()
//          FWrite( ::nH, ::cOutput )
            ::oHtm:cStr += ::cOutput
         ENDIF

         ::cOutPut := "<B>" + ::Caption + "</B>" + CRLF()
//       FWrite( ::nH, ::cOutput )
         ::oHtm:cStr += ::cOutput
         ::cOutPut := "</TD></TR>"
//       FWrite( ::nH, ::cOutput )
         ::oHtm:cStr += ::cOutput
      ENDIF

      ::cOutPut := '<TR BGCOLOR="' + ::color + '">' + CRLF()
//    FWrite( ::nH, ::cOutput )
      ::oHtm:cStr += ::cOutput
      ::cOutPut := '<TD'
//    FWrite( ::nH, ::cOutput )
      ::oHtm:cStr += ::cOutput
      IF ::bgImage != NIL
         ::cOutPut := ' BACKGROUND="' + ::bgImage + '"' + CRLF()
//       FWrite( ::nH, ::cOutput )
         ::oHtm:cStr += ::cOutput
      ENDIF

      ::cOutPut := '>'
//    FWrite( ::nH, ::cOutput )
      ::oHtm:cStr += ::cOutput
      IF ::fontColor != NIL
         ::cOutPut := '<FONT COLOR="' + ::FontColor + '">' + CRLF()
//       FWrite( ::nH, ::cOutput )
         ::oHtm:cStr += ::cOutput
      ENDIF

   ENDIF

   ::cOutput += CRLF() + "<FORM " + CRLF()
// FWrite( ::nH, ::cOutput )
   ::oHtm:cStr += ::cOutput

   IF ::name != NIL
      ::cOutPut := Space( 5 ) + '    NAME="' + ::Name + '"' + CRLF()
//    FWrite( ::nH, ::cOutput )
      ::oHtm:cStr += ::cOutput
   ENDIF

   IF ::method != NIL
      ::cOutPut := Space( 5 ) + '  METHOD="' + ::Method + '"' + CRLF()
//    FWrite( ::nH, ::cOutput )
      ::oHtm:cStr += ::cOutput
   ENDIF

   IF ::Action != NIL
      ::cOutPut := Space( 5 ) + '  ACTION=' + ::Action + '' + CRLF()
//    FWrite( ::nH, ::cOutput )
      ::oHtm:cStr += ::cOutput
   ENDIF

   IF ::Target != NIL
      ::cOutPut := Space( 5 ) + '  TARGET=' + ::Target + '' + CRLF()
//    FWrite( ::nH, ::cOutput )
      ::oHtm:cStr += ::cOutput
   ENDIF

   IF ::Enctype != NIL
      ::cOutPut := Space( 5 ) + ' ENCTYPE="' + ::encType + '"' + CRLF()
//    FWrite( ::nH, ::cOutput )
      ::oHtm:cStr += ::cOutput
   ENDIF

   IF ::onSubmit != NIL
      ::cOutPut := Space( 5 ) + 'onSubmit="' + ::onSubmit + '"' + CRLF()
//    FWrite( ::nH, ::cOutput )
      ::oHtm:cStr += ::cOutput
   ENDIF

   IF ::onReset != NIL
      ::cOutPut := Space( 5 ) + ' onReset="' + ::onReset + '"' + CRLF()
//    FWrite( ::nH, ::cOutput )
      ::oHtm:cStr += ::cOutput
   ENDIF

   ::cOutPut := ">" + CRLF()

// FWrite( ::nH, ::cOutput )
   ::oHtm:cStr += ::cOutput
   IF lPutControls
      AEval( ::aControls, {| e | iif( HB_ISOBJECT( e ), ;
         e:Put(), ::oHtm:cStr += e ) } )
   ENDIF

   RETURN Self

/****
*
*     THtmlForm():End()
*
*/

METHOD End() CLASS THtmlForm

   ::ohtm:cStr += "</FORM>" + CRLF()

   IF ::Frame
      ::ohtm:cStr += "</TD>" + CRLF()
      ::ohtm:cStr += "</TR>" + CRLF()
      ::ohtm:cStr += "</TABLE>" + CRLF()
   ENDIF

   ::ohtm:cStr += CRLF() + CRLF() + "<!---  End of Form --->" + CRLF() + CRLF() + CRLF()

   RETURN Self

/****
*
*     THtmlForm():GetControl()
*
*     Retrieve a form control object by name
*
*/

METHOD GetControl( cName ) CLASS THtmlForm

   LOCAL oRet
   LOCAL nPos := AScan( ::aControls, {| e | e:name == cName } )

   IF nPos > 0
      oRet := ::aControls[ nPos ]
   ENDIF

   RETURN oRet

/****
*
*     HtmlFormObject()
*
*     Return current form object
*
*/

FUNCTION HtmlFormObject()

   RETURN s_oForm
