/*
 * Harbour Project source code:
 * Editing and Forms Class for HTMLLIB
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See COPYING.txt for licensing terms.
 *
 */

#include "hbclass.ch"
#include "cgi.ch"

#define _OPTION_TEXT      1
#define _OPTION_VALUE     2
#define _OPTION_LABEL     3
#define _OPTION_SELECTED  4
#define _OPTION_DISABLED  5

STATIC s_oForm

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

METHOD Put() CLASS THtmlControl

   LOCAL i
   LOCAL cStr

   ::nH := HtmlPageHandle()
   ::form := HtmlFormName()
   ::oHtm := HtmlPageObject()

   ::cOutput += iif( ::lBreak, CRLF() + "<br />", CRLF() )
   IF ::lLabel
      ::cOutPut += CRLF() + "<label>" + CRLF()
   ENDIF

   IF HB_ISSTRING( ::Caption )
      ::cOutput += ::Caption + HTMLSPACE( 2 ) + " <!-- " + ::Type + " Control Caption -->" + CRLF()
   ENDIF

   IF ::Type == "SELECT"
      ::cOutPut += CRLF() + ' <select ' + CRLF()
   ELSE
      ::cOutPut += CRLF() + ' <input type="'
      ::cOutput += ::Type + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::Name )
      ::cOutput += Space( 2 ) + '     name="' + ::Name + '"' + CRLF()
   ENDIF

   IF ::Type == "TEXTAREA"
      IF HB_ISNUMERIC( ::Rows )
         ::cOutput += Space( 2 ) + '     rows="' + hb_ntos( ::Rows ) + '"' + CRLF()
      ENDIF
      IF HB_ISNUMERIC( ::Cols )
         ::cOutput += Space( 2 ) + '     cols="' + hb_ntos( ::Cols ) + '"' + CRLF()
      ENDIF
      IF HB_ISSTRING( ::Wrap )
         ::cOutput += Space( 2 ) + '     wrap="' + ::Wrap + '"' + CRLF()
      ENDIF
   ENDIF

   IF ::value != NIL

      IF ! HB_ISSTRING( ::Picture )
         ::Picture := "@X"
      ENDIF

      ::cOutput += Space( 2 ) + '    value="' + Transform( ::Value, ::Picture ) + '"' + CRLF()
   ENDIF

   IF HB_ISNUMERIC( ::maxChars )
      ::cOutput += Space( 2 ) + 'maxlength="' + hb_ntos( ::maxChars ) + '"' + CRLF()
   ENDIF

   IF HB_ISNUMERIC( ::Size )
      ::cOutput += Space( 2 ) + '     size="' + hb_ntos( ::Size ) + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::Id )
      ::cOutput += Space( 2 ) + '       id="' + ::Id + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::Style )
      ::cOutput += Space( 2 ) + '    style="' + ::Style + '"' + CRLF()
   ENDIF

   IF ::type == "IMAGE" .AND. HB_ISSTRING( ::Source )
      ::cOutput += Space( 2 ) + '      src="' + ::Source + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::Align )
      ::cOutput += Space( 2 ) + '    align="' + ::Align + '"' + CRLF()
   ENDIF

   IF ::type == "RADIO" .OR. ::type == "CHECKBOX" .AND. ::Checked
      ::cOutput += Space( 2 ) + '   CHECKED' + CRLF()
   ENDIF

   IF ::Disabled
      ::cOutput += Space( 2 ) + '  DISABLED' + CRLF()
   ENDIF

   IF ::Readonly
      ::cOutput += Space( 2 ) + '  READONLY' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::onChange )
      ::cOutput += Space( 2 ) + ' onChange="' + ::onChange + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::onFocus )
      ::cOutput += Space( 2 ) + '  onFocus="' + ::onFocus + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::onBlur )
      ::cOutput += Space( 2 ) + '   onBlur="' + ::onBlur + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::onSelect )
      ::cOutput += Space( 2 ) + ' onSelect="' + ::onSelect + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::onClick )
      ::cOutput += Space( 2 ) + '  onClick="' + ::onClick + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::onMouseOver )
      ::cOutput += 'onMouseOver="' + ::onMouseOver + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::onMouseOut )
      ::cOutput += Space( 1 ) + 'onMouseOut="' + ::onMouseOut + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::onMouseDown )
      ::cOutput += 'onMouseDown="' + ::onMouseDown + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::onMouseUp )
      ::cOutput += Space( 1 ) + 'onMouseUp="' + ::onMouseUp + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::onKeyDown )
      ::cOutput += 'onKeyDown="' + ::onKeyDown + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::onKeyUp )
      ::cOutput += Space( 1 ) + 'onKeyUp="' + ::onKeyUp + '"' + CRLF()
   ENDIF

   IF HB_ISSTRING( ::onKeyPress )
      ::cOutput += Space( 1 ) + 'onKeyPress="' + ::onKeyPress + '"' + CRLF()
   ENDIF

   ::cOutput += " >" + CRLF()

   IF ::lLabel
      ::cOutPut += CRLF() + "</label>" + CRLF()
   ENDIF

   ::oHtm:cStr += ::cOutput

   IF ::Type == "SELECT"

      FOR EACH i IN ::aOptions
         cStr := ;
            "<option" + ;
            iif( HB_ISSTRING( i[ _OPTION_VALUE ] ), " value=" + i[ _OPTION_VALUE ], "" ) + ;
            iif( HB_ISSTRING( i[ _OPTION_LABEL ] ), " label=" + i[ _OPTION_LABEL ], "" ) + ;
            iif( HB_ISLOGICAL( i[ _OPTION_SELECTED ] ) .AND. i[ _OPTION_SELECTED ], " SELECTED ", "" ) + ;
            iif( HB_ISLOGICAL( i[ _OPTION_DISABLED ] ) .AND. i[ _OPTION_DISABLED ], " DISABLED ", "" ) + ;
            ">" + i[ _OPTION_TEXT ] + "</option>" + CRLF()
         ::oHtm:cStr += cStr
      NEXT

      ::oHtm:cStr += "</select>"
   ENDIF

   RETURN Self

METHOD AddOption( cOption, cValue, cLabel, lSelected, lDisabled ) CLASS THtmlControl

   AAdd( ::aOptions, { cOption, cValue, cLabel, lSelected, lDisabled } )

   RETURN Self

/* Batch set control properties/methods
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
   METHOD End()
   METHOD GetControl( cName )

ENDCLASS

METHOD New( cName, cAction, cMethod, lFrame, cCaption, nWidth ) CLASS THtmlForm

   HB_SYMBOL_UNUSED( cAction )

   ::oHtm := HtmlPageObject()
   ::Name := hb_defaultValue( cName, "Form1" )
   ::Method := hb_defaultValue( cMethod, "POST" )

   ::nH := HtmlPageHandle()

   ::Frame := hb_defaultValue( lFrame, .F. )
   ::Caption := hb_defaultValue( cCaption, "" )
   ::width := hb_defaultValue( nWidth, 90 )

   ::aControls := {}

   s_oForm := Self

   RETURN Self

METHOD Put( lPutControls ) CLASS THtmlForm

   hb_default( @::width, 90 )
   hb_default( @::Color, "#9196A0" )
   hb_default( @::fontColor, "black" )
   hb_default( @::CaptionColor, "black" )

   IF ::Frame
      ::cOutPut := CRLF() + CRLF() + CRLF() + "<!-------  Start of Form ------->" + CRLF() + CRLF()
      ::oHtm:cStr += ::cOutput
      ::cOutPut := '<table bgcolor="#9196A0" ' + CRLF() + ;
         "       cols=1 " + CRLF() + ;
         "       rows=1 " + CRLF() + ;
         "       cellpadding=3 " + CRLF() + ;
         "       cellspacing=3 " + CRLF() + ;
         "       width=" + hb_ntos( ::width ) + "% " + CRLF() + ;
         '       bordercolorlight="#000000" ' + CRLF() + ;
         '       bordercolordark="#FFFFFF" ' + CRLF() + ;
         "       border " + CRLF() + ;
         "       >" + CRLF()
      ::oHtm:cStr += ::cOutput
      ::cOutPut := '<tr bgcolor="' + ::captionColor + '">' + CRLF()

      IF HB_ISSTRING( ::Caption )
         ::cOutPut := "<td"
         ::oHtm:cStr += ::cOutput

         IF HB_ISSTRING( ::captionImage )
            ::cOutPut := ' background="' + ::captionImage + '"' + CRLF()
            ::oHtm:cStr += ::cOutput
         ENDIF

         ::cOutPut := '>'
         ::oHtm:cStr += ::cOutput

         IF HB_ISSTRING( ::capFontColor )
            ::cOutPut := '<font color="' + ::capFontColor + '">' + CRLF()
            ::oHtm:cStr += ::cOutput
         ENDIF

         ::cOutPut := "<b>" + ::Caption + "</b>" + CRLF()
         ::oHtm:cStr += ::cOutput
         ::cOutPut := "</td></tr>"
         ::oHtm:cStr += ::cOutput
      ENDIF

      ::cOutPut := '<tr bgcolor="' + ::color + '">' + CRLF()
      ::oHtm:cStr += ::cOutput
      ::cOutPut := '<td'
      ::oHtm:cStr += ::cOutput
      IF HB_ISSTRING( ::bgImage )
         ::cOutPut := ' background="' + ::bgImage + '"' + CRLF()
         ::oHtm:cStr += ::cOutput
      ENDIF

      ::cOutPut := '>'
      ::oHtm:cStr += ::cOutput
      IF HB_ISSTRING( ::fontColor )
         ::cOutPut := '<font color="' + ::FontColor + '">' + CRLF()
         ::oHtm:cStr += ::cOutput
      ENDIF

   ENDIF

   ::cOutput += CRLF() + "<form " + CRLF()
   ::oHtm:cStr += ::cOutput

   IF HB_ISSTRING( ::name )
      ::cOutPut := Space( 5 ) + '    name="' + ::Name + '"' + CRLF()
      ::oHtm:cStr += ::cOutput
   ENDIF

   IF HB_ISSTRING( ::method )
      ::cOutPut := Space( 5 ) + '  method="' + ::Method + '"' + CRLF()
      ::oHtm:cStr += ::cOutput
   ENDIF

   IF HB_ISSTRING( ::Action )
      ::cOutPut := Space( 5 ) + '  action=' + ::Action + '' + CRLF()
      ::oHtm:cStr += ::cOutput
   ENDIF

   IF HB_ISSTRING( ::Target )
      ::cOutPut := Space( 5 ) + '  target=' + ::Target + '' + CRLF()
      ::oHtm:cStr += ::cOutput
   ENDIF

   IF HB_ISSTRING( ::Enctype )
      ::cOutPut := Space( 5 ) + ' enctype="' + ::encType + '"' + CRLF()
      ::oHtm:cStr += ::cOutput
   ENDIF

   IF HB_ISSTRING( ::onSubmit )
      ::cOutPut := Space( 5 ) + 'onSubmit="' + ::onSubmit + '"' + CRLF()
      ::oHtm:cStr += ::cOutput
   ENDIF

   IF HB_ISSTRING( ::onReset )
      ::cOutPut := Space( 5 ) + ' onReset="' + ::onReset + '"' + CRLF()
      ::oHtm:cStr += ::cOutput
   ENDIF

   ::cOutPut := ">" + CRLF()

   ::oHtm:cStr += ::cOutput
   IF hb_defaultValue( lPutControls, .F. )
      AEval( ::aControls, {| e | iif( HB_ISOBJECT( e ), e:Put(), ::oHtm:cStr += e ) } )
   ENDIF

   RETURN Self

METHOD End() CLASS THtmlForm

   ::ohtm:cStr += "</form>" + CRLF()

   IF ::Frame
      ::ohtm:cStr += "</td>" + CRLF()
      ::ohtm:cStr += "</tr>" + CRLF()
      ::ohtm:cStr += "</table>" + CRLF()
   ENDIF

   ::ohtm:cStr += CRLF() + CRLF() + "<!---  End of Form --->" + CRLF() + CRLF() + CRLF()

   RETURN Self

/* Retrieve a form control object by name
*/
METHOD GetControl( cName ) CLASS THtmlForm

   LOCAL nPos

   IF ( nPos := AScan( ::aControls, {| e | e:name == cName } ) ) > 0
      RETURN ::aControls[ nPos ]
   ENDIF

   RETURN NIL

/* Return current form object
*/
FUNCTION HtmlFormObject()
   RETURN s_oForm
