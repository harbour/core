/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Editing and Forms Class for HTMLLIB
 *
 * Copyright 2000 Manos Aspradakis <maspr@otenet.gr>
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
/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 2000 Luiz Rafael Culik <culik@sl.conex.net>
 *    Porting this library to Harbour
 *
 * See doc/license.txt for licensing terms.
 *
 */


#include "hbclass.ch"
#include "default.ch"
#include "forms.ch"
#include "html.ch"

#define _OPTION_TEXT     1
#define _OPTION_VALUE    2
#define _OPTION_LABEL    3
#define _OPTION_SELECTED 4
#define _OPTION_DISABLED 5


STATIC soForm

/****
*
*     Class HControl()
*
*
*/

CLASS HControl // ALIAS hCtr 
DATA nH
DATA Document
DATA Form


DATA Caption
DATA lBreak     INIT .F.


DATA Type       INIT "TEXT"
DATA Name
DATA Value
DATA Size
DATA rows, cols

DATA cOutPut    INIT ""

DATA Picture    INIT "@X"

DATA aOptions   INIT {}       // SELECT options

DATA TabIndex   INIT 0
DATA disabled   INIT .F.
DATA readOnly   INIT .F.
DATA Multiple   INIT .F.
DATA style
DATA id

DATA MaxLength                // length in chars
DATA MaxChars                 // length on screen

DATA Checked    INIT .F.      // checkboxes...

DATA Source, Align            // images...
DATA Wrap                     // textarea

DATA onBlur
DATA onChange
DATA onFocus
DATA onSelect
DATA onClick
DATA onMouseOver
DATA onMouseOut
DATA onMouseDown
DATA onMouseup
DATA onKeyPress
DATA onKeyDown
DATA onKeyUp
DATA onSelect

METHOD SetName( c )      INLINE ::Name        := c
METHOD SetValue( c )     INLINE ::Value       := c
METHOD SetStyle( c )     INLINE ::Style       := c
METHOD SetId( c )        INLINE ::id          := c
METHOD SetRows( c )      INLINE ::Rows        := c
METHOD SetCols( c )      INLINE ::Cols        := c
METHOD SetCaption(c)     INLINE ::Caption     := c
METHOD SetPicture(c)     INLINE ::picture     := c
METHOD SetOnBlur(c)      INLINE ::onBlur      := c
METHOD SetOnChange(c)    INLINE ::onChange    := c
METHOD SetOnFocus(c)     INLINE ::onFocus     := c
METHOD SetOnSelect(c)    INLINE ::onSelect    := c
METHOD SetOnClick(c)     INLINE ::onClick     := c
METHOD SetOnMsOver(c)    INLINE ::onMouseOver := c
METHOD SetOnMsOut(c)     INLINE ::onMouseOut  := c
METHOD SetSize( n )      INLINE ::Size        := n
METHOD SetMaxChars( n )  INLINE ::MaxChars    := n
METHOD SetChecked( l )   INLINE ::Checked     := l
METHOD SetAlign( c )     INLINE ::Align       := c
METHOD SetWrap( c )      INLINE ::wrap        := c
METHOD SetSource( c )    INLINE ::Source      := c
METHOD SetReadOnly( l )  INLINE ::readOnly    := l
METHOD SetDisabled( l )  INLINE ::disabled    := l
METHOD SetMultiple( l )  INLINE ::multiple    := l
METHOD SetOnMsDown(c)    INLINE ::onMouseDown := c
METHOD SetOnMsUp(c)      INLINE ::onMouseup   := c
METHOD SetOnKPress(c)    INLINE ::onKeyPress  := c
METHOD SetOnKDown(c)     INLINE ::onKeyDown   := c
METHOD SetOnKUp(c)       INLINE ::onKeyUp     := c
METHOD SetOnSelect(c)    INLINE ::onSelect    := c
      
//METHOD Debug()         INLINE __clsDebug( self )

METHOD Put(lPut)
METHOD AddOption( cOption, cValue, cLabel, lSelected, lDisabled )  
METHOD SetControl( name,rows,cols,size,maxchars,value,onfocus,;
                           onblur,onchange,onselect,onclick,onmsover,onmsout,;
                           onmsdown,onmsup,onkdown,onkup,onkprs, ;
                           pic,cap,dis,ro,lMulti,checked,;
                           align,wrap,type, Style, Id ) 

ENDCLASS



/****
*
*     HControl():Put()
*
*
*/

method Put(lPut)   CLASS HControl 
LOCAL i, cStr := ""

::nH   := pageHandle()
::form := currentForm()

::cOutput += IF( ::lBreak, CRLF()+"<BR>", CRLF() )

IF ::Caption != NIL
   ::cOutput += ::Caption + HTMLSPACE(2) +" <!-- "+::Type+" Control Caption -->"+CRLF()
ENDIF

IF ::Type == "SELECT"
::cOutPut += CRLF()+' <SELECT '+CRLF()
ELSE
::cOutPut += CRLF()+' <INPUT TYPE="'
::cOutput += ::Type +'"'+CRLF()
ENDIF

IF ::Name != NIL
::cOutput += space(2)+'     NAME="'+ ::Name      +'"' + CRLF()
ENDIF

IF ::Type = "TEXTAREA"
IF ::Rows != NIL
::cOutput += space(2)+'     ROWS="'+ NTRIM(::Rows)      +'"' + CRLF()
ENDIF

IF ::Cols != NIL
::cOutput += space(2)+'     COLS="'+ NTRIM(::Cols)      +'"' + CRLF()
ENDIF

IF ::Wrap != NIL
::cOutput += space(2)+'     WRAP="'+ ::Wrap +'"' + CRLF()
ENDIF
ENDIF // textArea

IF ::value != NIL
   IF ::Picture == NIL
      ::Picture := "@X"
   ENDIF
   ::cOutput += space(2)+'    VALUE="'+ TRANSFORM(::Value, ::Picture) +'"' + CRLF()
ENDIF

IF ::maxChars != NIL
::cOutput += space(2)+'MAXLENGTH="'+ NTRIM(::maxChars) +'"' + CRLF()
ENDIF

IF ::Size != NIL
::cOutput += space(2)+'     SIZE="'+ NTRIM(::Size)  +'"' + CRLF()
ENDIF

IF ::Id != NIL
::cOutput += space(2)+'       ID="'+ ::Id  +'"' + CRLF()
ENDIF

IF ::Style != NIL
::cOutput += space(2)+'    STYLE="'+ ::Style  +'"' + CRLF()
ENDIF

IF ::type = "IMAGE"
IF ::Source != NIL
::cOutput += space(2)+'      SRC="'+ ::Source  +'"' + CRLF()
ENDIF
ENDIF

IF ::Align != NIL
::cOutput += space(2)+'    ALIGN="'+ ::Align  +'"' + CRLF()
ENDIF

IF ::type = "RADIO" .OR. ::type = "CHECKBOX"
IF ::Checked
::cOutput += space(2)+'   CHECKED' +CRLF()
ENDIF
ENDIF

IF ::Disabled
::cOutput += space(2)+'  DISABLED' +CRLF()
ENDIF

IF ::Readonly
::cOutput += space(2)+'  READONLY' +CRLF()
ENDIF

IF ::onChange != NIL
::cOutput += space(2)+' onChange="'+::onChange+'"'+CRLF()
ENDIF

IF ::onFocus != NIL
::cOutput += space(2)+'  onFocus="'+::onFocus+'"'+CRLF()
ENDIF

IF ::onBlur != NIL
::cOutput += space(2)+'   onBlur="'+::onBlur+'"'+CRLF()
ENDIF

IF ::onSelect != NIL
::cOutput += space(2)+' onSelect="'+::onSelect+'"'+CRLF()
ENDIF

IF ::onClick != NIL
::cOutput += space(2)+'  onClick="'+::onClick+'"'+CRLF()
ENDIF

IF ::onMouseOver != NIL
::cOutput +=        'onMouseOver="'+::onMouseOver+'"'+CRLF()
ENDIF

IF ::onMouseOut != NIL
::cOutput += space(1)+'onMouseOut="'+::onMouseOut+'"'+CRLF()
ENDIF

IF ::onMouseDown != NIL
::cOutput +=        'onMouseDown="'+::onMouseDown+'"'+CRLF()
ENDIF

IF ::onMouseUp != NIL
::cOutput += space(1)+'onMouseUp="'+::onMouseUp+'"'+CRLF()
ENDIF

IF ::onKeyDown != NIL
::cOutput +=        'onKeyDown="'+::onKeyDown+'"'+CRLF()
ENDIF

IF ::onKeyUp != NIL
::cOutput += space(1)+'onKeyUp="'+::onKeyUp+'"'+CRLF()
ENDIF

IF ::onKeyPress != NIL
::cOutput += space(1)+'onKeyPress="'+::onKeyPress+'"'+CRLF()
ENDIF


::cOutput += " >"+CRLF()

FWrite( ::nH, ::cOutput )

IF ::Type == "SELECT"
   FOR i=1 TO LEN( ::aOptions )
       cStr := "<OPTION"
       cStr += IF( ::aOptions[i,_OPTION_VALUE] != NIL, ;
                   " value="+::Options[i,_OPTION_VALUE], "")
       cStr += IF( ::aOptions[i,_OPTION_LABEL] != NIL, ;
                   " label="+::Options[i,_OPTION_LABEL], "")
       cStr += IF( ::aOptions[i,_OPTION_SELECTED] == .T., ;
                   " SELECTED ", "")
       cStr += IF( ::aOptions[i,_OPTION_DISABLED] == .T., ;
                   " DISABLED ", "")
       cStr += ">"+::aOptions[i,_OPTION_TEXT]+"</OPTION>"+CRLF()
       FWrite( ::nH, cStr )
   NEXT
       FWrite( ::nH, "</SELECT>" )
ENDIF


RETURN Self



/****
*
*     HControl():AddOption()
*
*/

method AddOption( cOption, cValue, cLabel, lSelected, lDisabled ) CLASS HControl 
AADD( ::aOptions, { cOption, cValue, cLabel, lSelected, lDisabled } )
RETURN Self



/****
*
*     HControl():setControl()
*
*     Batch set control properties/methods
*
*     All default to NIL. See controls.ch
*/

method setControl( name,rows,cols,size,maxchars,value,onfocus,;
                           onblur,onchange,onselect,onclick,onmsover,onmsout,;
                           onmsdown,onmsup,onkdown,onkup,onkprs, ;
                           pic,cap,dis,ro,lMulti,checked,;
                           align,wrap,type, Style, Id ) CLASS HControl 
::name        := name
::rows        := rows
::cols        := cols
::size        := size
::maxChars    := maxchars
::value       := value
::onFocus     := onfocus
::onBlur      := onblur
::onChange    := onchange
::onSelect    := onselect
::onClick     := onclick
::onMouseOver := onmsover
::onMouseOut  := onmsout
::onMouseDown := onmsdown
::onMouseUp   := onmsup
::onKeyDown   := onkdown
::onKeyUp     := onkup
::onKeyPress  := onkprs
::picture     := pic
::caption     := cap
::disabled    := dis
::readonly    := ro
::Multiple    := lMulti
::Checked     := checked
::Align       := align
::Wrap        := wrap
::type        := type
::Style       := Style
::Id          := Id

RETURN Self

  

/****
*
*     Class Form()
*
*
*
*/


CLASS Form 

DATA nH
DATA aControls      init {}
DATA Name           init ""
DATA Action
DATA Method         //"mailto:name@site"
DATA EncType        init "multipart/form-data"
DATA onSubmit
DATA onReset
DATA Target
DATA Frame          init .F.
DATA Caption        init ""
DATA CaptionColor   init "black"
DATA CapFontColor   init "white"
DATA CaptionImage
DATA bgImage
DATA fontColor      init "black"
DATA Color          init "#9196A0"
DATA width          init 90

DATA cOutput        init  ""

METHOD setHandle( h )    INLINE ::nH               := h

METHOD setName( c )      INLINE ::Name             := c
METHOD setAction( c )    INLINE ::Action           := c
METHOD setMethod( c )    INLINE ::Method           := c
METHOD setEncType( c )   INLINE ::encType          := c
METHOD setOnSubmit( c )  INLINE ::onSubmit         := c
METHOD setOnReset( c )   INLINE ::onReset          := c
METHOD setTarget( c )    INLINE ::Target           := c
METHOD setCapClr( c )    INLINE ::CaptionColor     := c
METHOD setCapFntClr( c ) INLINE ::CapFontColor     := c
METHOD setCapImage( c )  INLINE ::CaptionImage     := c
METHOD setBgImage( c )   INLINE ::bgImage          := c
METHOD setFontColor( c ) INLINE ::FontColor        := c
METHOD setFrmColor( c )  INLINE ::Color            := c
METHOD setwidth( c )     INLINE ::width            := c


METHOD AddControl( o )   INLINE IF(Valtype(o)=="O",;
                                  (o:nH := ::nH, o:Form := Self),), ;
                                AADD( ::aControls, o )

METHOD PutControls()     INLINE AEVAL( ::aControls, {|e| e:Put() } )
METHOD New( cName, cAction, cMethod, lFrame, cCaption, nWidth )
METHOD Put(        lPutControls)
METHOD End()        
METHOD GetControl( cName)

//METHOD Debug()           INLINE __clsDebug( self ) NOSELF

ENDCLASS


/****
*
*     Form():New()
*
*
*/


method New( cName, cAction, cMethod, lFrame, cCaption, nWidth ) CLASS Form 

DEFAULT cName    := "Form1"
DEFAULT cMethod  := "POST"
DEFAULT lFrame   := .F.
DEFAULT cCaption := ""
DEFAULT nWidth   := 90
          
::Name    := cName
::Method  := cMethod

::nH      := PageHandle()

::Frame   := lFrame
::Caption := cCaption
::width   := nWidth

::aControls := {}

soForm := Self

RETURN Self



/****
*
*     Form():Put()
*
*
*/

method Put( lPutControls ) CLASS Form 

DEFAULT lPutControls := .F.

IF VALTYPE(::width) != "N"
   ::width := 90
ENDIF

IF VALTYPE( ::color ) != "C"
   ::Color := "#9196A0"
ENDIF

IF VALTYPE( ::fontColor ) != "C"
   ::fontColor := "black"
ENDIF

IF VALTYPE( ::CaptionColor ) != "C"
   ::CaptionColor := "black"
ENDIF


IF ::Frame
   ::cOutPut := CRLF()+CRLF()+CRLF()+"<!-------  Start of Form ------->" +CRLF()+CRLF()
    FWrite( ::nH, ::cOutput )
   ::cOutPut := '<TABLE BGCOLOR="#9196A0" '+CRLF()+;
                "       COLS=1 "+CRLF()+;
                "       ROWS=1 "+CRLF()+;
                "       CELLPADDING=3 "+CRLF()+;
                "       CELLSPACING=3 "+CRLF()+;
                "       WIDTH=" + NTRIM( ::width ) +"% "+CRLF()+;
                '       BORDERCOLORLIGHT="#000000" '+CRLF()+;
                '       BORDERCOLORDARK="#FFFFFF" ' +CRLF()+;
                "       BORDER "+CRLF()+;
                "       >"+CRLF()
    FWrite( ::nH, ::cOutput )
   ::cOutPut := '<TR BGCOLOR="'+::captionColor+'">'+CRLF()
   IF ::Caption != NIL
      ::cOutPut := "<TD"
          FWrite( ::nH, ::cOutput )
      IF ::captionImage != NIL
      ::cOutPut := ' BACKGROUND="'+::captionImage+'"'+CRLF()
          FWrite( ::nH, ::cOutput )
      ENDIF
      ::cOutPut := '>'
          FWrite( ::nH, ::cOutput )
      IF ::capFontColor != NIL
      ::cOutPut := '<FONT COLOR="'+::capFontColor+'">'+CRLF()
          FWrite( ::nH, ::cOutput )
      ENDIF

      ::cOutPut := "<B>"+::Caption+"</B>"+CRLF()
          FWrite( ::nH, ::cOutput )
      ::cOutPut := "</TD></TR>"
          FWrite( ::nH, ::cOutput )
   ENDIF

   ::cOutPut := '<TR BGCOLOR="'+::color+'">' +CRLF()
       FWrite( ::nH, ::cOutput )
   ::cOutPut := '<TD'
       FWrite( ::nH, ::cOutput )
   IF ::bgImage != NIL
   ::cOutPut := ' BACKGROUND="'+::bgImage+'"'+CRLF()
       FWrite( ::nH, ::cOutput )
   ENDIF
      ::cOutPut := '>'
          FWrite( ::nH, ::cOutput )

   IF ::fontColor != NIL
   ::cOutPut := '<FONT COLOR="'+::FontColor+'">'+CRLF()
       FWrite( ::nH, ::cOutput )
   ENDIF

ENDIF
::cOutput+= CRLF()+"<FORM " + CRLF()
    FWrite( ::nH, ::cOutput )

IF ::name != NIL
::cOutPut := space(5)+'    NAME="'+::Name+'"'+CRLF()
    FWrite( ::nH, ::cOutput )
ENDIF
IF ::method != NIL
::cOutPut := space(5)+'  METHOD="'+::Method+'"'+CRLF()
    FWrite( ::nH, ::cOutput )
ENDIF
IF ::Action != NIL
::cOutPut := space(5)+'  ACTION='+::Action+''+CRLF()
    FWrite( ::nH, ::cOutput )
ENDIF
IF ::Target != NIL
::cOutPut := space(5)+'  TARGET='+::Target+''+CRLF()
    FWrite( ::nH, ::cOutput )
ENDIF

IF ::Enctype != NIL
::cOutPut := space(5)+' ENCTYPE="'+::encType+'"'+CRLF()
    FWrite( ::nH, ::cOutput )
ENDIF

IF ::onSubmit != NIL
::cOutPut := space(5)+'onSubmit="'+::onSubmit+'"'+CRLF()
    FWrite( ::nH, ::cOutput )
ENDIF

IF ::onReset != NIL
::cOutPut := space(5)+' onReset="'+::onReset+'"'+CRLF()
    FWrite( ::nH, ::cOutput )
ENDIF

//::cOutPut := ">"+CRLF()

FWrite( ::nH, ::cOutput )

IF lPutControls
   AEVAL( ::aControls, {|e| IF( VALTYPE(e) == "O", ;
                                e:Put(), FWrite(::nH, e ) ) } )
ENDIF

RETURN Self



/****
*
*     Form():End()
*
*
*/

method End() CLASS Form 
FWrite( ::nH, "</FORM>" + CRLF() )
IF ::Frame
   FWrite( ::nH, "</TD>"+CRLF() )
   FWrite( ::nH, "</TR>"+CRLF() )
   FWrite( ::nH, "</TABLE>"+CRLF() )
Endif
FWrite( ::nH, CRLF()+CRLF()+"<!---  End of Form --->" +CRLF()+CRLF()+CRLF() )
RETURN Self



/****
*
*     Form():getControl()
*
*     Retrieve a form control object by name
*
*/

method getControl( cName ) CLASS Form 

LOCAL oRet
LOCAL nPos := ASCAN( ::aControls, {|e| e:name = cName } )
IF nPos > 0
   oRet := ::aControls[nPos]
ENDIF

RETURN oRet



/****
*
*     oForm()
*
*     Return current form
*
*/

FUNCTION oForm()
RETURN soForm


