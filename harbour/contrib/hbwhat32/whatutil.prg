/*
 * $Id$
 */

//Clipper-like Alert function

#include "common.ch"

#include "winuser.ch"
#include "What32.ch"
#include 'debug.ch'
#include 'wingdi.ch'

#include 'wintypes.ch'
#include 'cstruct.ch'

#define CTYPE_BOOL 5

//#DEFINE NTRIM( n ) AllTrim( str( n ) )

pragma pack(4)

typedef struct tagLOGFONT { ;
   LONG lfHeight;
   LONG lfWidth;
   LONG lfEscapement;
   LONG lfOrientation;
   LONG lfWeight;
   BYTE lfItalic;
   BYTE lfUnderline;
   BYTE lfStrikeOut;
   BYTE lfCharSet;
   BYTE lfOutPrecision;
   BYTE lfClipPrecision;
   BYTE lfQuality;
   BYTE lfPitchAndFamily;
   TCHAR lfFaceName[32];
} LOGFONT;

typedef struct tagNONCLIENTMETRICS { ;
    UINT    cbSize;
    int     iBorderWidth;
    int     iScrollWidth;
    int     iScrollHeight;
    int     iCaptionWidth;
    int     iCaptionHeight;
    LOGFONT lfCaptionFont;
    int     iSmCaptionWidth;
    int     iSmCaptionHeight;
    LOGFONT lfSmCaptionFont;
    int     iMenuWidth;
    int     iMenuHeight;
    LOGFONT lfMenuFont;
    LOGFONT lfStatusFont;
    LOGFONT lfMessageFont;
} NONCLIENTMETRICS



*-----------------------------------------------------------------------------*
// Vic McClung

function toUnicode( cString )
   local i, cTemp := ""
   for i := 1 to len(cString)
      cTemp += substr(cString, i, 1) + chr(0)
   next
   return cTemp + chr(0)

*-----------------------------------------------------------------------------*
// Vic McClung

function fromUnicode( cString )
   local i, cTemp := ""
   for i := 1 to len(cString) Step 2
      cTemp += substr(cString, i, 1)
   next
   return cTemp

*-----------------------------------------------------------------------------*
Function Alert( cMsg, aChoices )

   Local aDlg, i, n, aChoose, amSG
   Local hWnd, hDC
   Local lErr := .F., w , h, t := 0, cTitle, msgh, butwidth
   Local crpos := 0, txth := 0, atm := { }
   LOCAL hFont:=CreateFont( { 8, 0, 0, 0, 700, 0, 0, 0, 0, 1, 2, 1, 34, "MS Sans Serif" } )
   LOCAL hOldFont
   LOCAL xBase

   If !ISCHARACTER( cMsg )
      IF ISARRAY( cMsg )
         cMsg:=a2str(cMsg,";")
      Else
         cMsg := asString( cMsg )
      Endif
   EndIf

   cTitle := 'Alert'

   If aChoices == NIL
      aChoices := { "&Ok" }
   EndIf

   cMsg := StrTran( cMsg, ";", CR )

   If ( crpos := at( CR, cMsg ) ) > 0
      cTitle := Left( cMsg, crpos - 1 )
      cMsg := SubStr( cMsg, crpos + 1 )
   EndIf

   hDC := GetDC( 0 )
   hOldFont := SelectObject( hDC, hFont )

* ------------- total width without buttons

   w := GetTextExtentPoint32( hDC, AllTrim( cTitle ) ) [ 1 ]

   amSG := str2a( cMsg, CR )

   AEVAL( amSG, { | X | w := Max( w, GetTextExtentPoint32( hDC, AllTrim( X ) ) [ 1 ] ) } )
   w += 20

* --------- total width of choices, also add "&" to the choices (if needed)

   n := Len( aChoices )
   aChoose := array( n )

   txth := 8 //ATM[TM_Height]
   msgh := Len( amSG ) * txth
   For i := 1 To n
      butwidth := Max( 20, GetTextExtentPoint32( hDC, aChoices[ i ] ) [ 1 ] + 20 )
      t := Max( t, butwidth )
      aChoose[ i ] := iif( at( "&", aChoices[ i ] ) == 0, "&" + aChoices[ i ] , aChoices[ i ] )
   Next i

   SelectObject( hDC, hOldFont )
   ReleaseDC( 0, hDC )
   DeleteObject( hFont )


   butwidth := t
   t *= ( n + 1 )
   w := Max( w+40, t )
   h := msgh + 33
   //w /= 2
   xBase:=LOWORD(GetDialogBaseUnits())
   w:=(w*4)/xBase


* ---------- get space between choices
   butwidth:=(butwidth*4)/xBase
   t := Max( Int( ( w - butwidth * n ) / ( n + 1 ) ) , 0 )

* ----------- create dialog

   hWnd := GetFocus( ) // default parent

   aDlg := MakeDlgTemplate( cTitle, ;
                           WS_CAPTION + DS_MODALFRAME + WS_VISIBLE + 4 + WS_POPUP + DS_SETFONT, ;
                           0, 0, w, h, 8, 'MS Sans Serif' )

   For i := 1 To n
      aDlg := AddDlgItem( aDlg, i, "BUTTON", ;
                         BS_PUSHBUTTON + WS_TABSTOP + WS_CHILD + WS_VISIBLE, ;
                         i * ( butwidth + t ) - butwidth, h - 16, butwidth, 14, ;
                         aChoose[ i ] )
   Next i


   aDlg := AddDlgItem( aDlg, "", "STATIC", ;
                      WS_BORDER + WS_CHILD + WS_VISIBLE, ;
                      0, 0, w , msgh + 14, ;
                      "" )

   aDlg := AddDlgItem( aDlg, "", "STATIC", ;
                      SS_CENTER + WS_CHILD + WS_VISIBLE, ;
                      2, 8, w - 4, msgh, ;
                      cMsg )

   MessageBeep( MB_OK )

   i := DialogBox( ,aDlg, hWnd, { | hDlg, nMsg, nwParam, nlParam | HB_SYMBOL_UNUSED( nlParam ), AlertProc( hDlg, nMsg, nwParam, nlParam ) } )

   SetFocus( hWnd )

   Return i

*----------------------------------------------------------------------------*

Function AlertProc( hDlg, nMsg, nwParam, nlParam )

   HB_SYMBOL_UNUSED( nlParam )

   Do Case
   Case nMsg == WM_INITDIALOG
      CenterWindow( hDlg )
      Return( 1 )

   Case nMsg == WM_COMMAND
      EndDialog( hDlg, nwParam )

   EndCase

   Return( 0 )

*------------------------------------------------------------------------------*
// returns ceiling of a number

Function Ceiling( x )

   Return( iif( x - Int( x ) > 0, Int( x ) + 1, x ) )


*-----------------------------------------------------------------------------*
// use STM_SETICON ???
// Was WM_USER in 16 bit

Function SetIcon(hDlg,id,hicon)

  Return(SendDlgItemMessage(hDlg,id,STM_SETICON,hicon,0))

*-----------------------------------------------------------------------------*
// use STM_GETICON ???
// Was WM_USER+1 in 16 bit

Function GetIcon(hDlg,id)

  Return(SendDlgItemMessage(hDlg,id,STM_GETICON,0,0))


*------------------------------------------------------------------------------*
* uses current hDC and hFont
* returns array of lines, the cText was broken into
* assumes presence of spaces in the text !

Function WrapText(cText,nMaxSize,hFont)

  Local a:={Trim(cText)}
  Local i
  Local n
  Local c

  For i:=1 To Len(a)
    c:=""
    Do While UnMapDialogRect(a[i],hFont)[1] > nMaxSize
      If (n:=rat(' ',a[i])) > 0
        c:=SubStr(a[i],n+1)+' '+c
        a[i]:=Left(a[i],n-1)
      Else
        Exit
      EndIf
    EndDo
    If Len(c) > 0
      aAdd(a,Trim(c))
    EndIf
  Next

  Return(a)

*-----------------------------------------------------------------------------*

Function UnMapDialogRect(cText,hfont)

  Local nX,nY,nW,nH

  Local hDC := GetDC(0)
  Local hOldFont:=SelectObject(hDC,hFont)
  Local aTextExt:=GetTextExtentPoint32(hDC,;
  'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz')

  Local arect:={0,0,100,100}

  nW:=aTextExt[1]
  nH:=aTextExt[2]

  // Looks like this is what it should be (?)

  nW:=Int((Int(nW / 26) + 1)/2)
  nX:=GetTextExtentPoint32(hDC,cText)[1]
  nY:=nH

  SelectObject(hDC,hOldFont)
  ReleaseDC(0, hDC)

  Return({Ceiling(nX*4/nW),Ceiling(nY*8/nY)})


*-----------------------------------------------------------------------------*
*  CenterWindow() - Courtesy of Gerald Barber
*  Centers a window in it's parent window's client area
*  AJ modified to center within another  nominated window
*-----------------------------------------------------------------------------*

Function CenterWindow( hWnd, NewX, NewY, hParent )

   Local aChild_[ 4 ]
   Local iCWidth
   Local iCHeight
   Local aParent_[ 4 ]
   Local aPoint_[ 2 ]

   aChild_ := GetWindowRect( hWnd )
   iCWidth := aChild_[ 3 ] - aChild_[ 1 ]
   iCHeight := aChild_[ 4 ] - aChild_[ 2 ]

   IF hparent == NIL
      hParent := GetWindow( hWnd, GW_OWNER )

      IF hParent == 0
         hParent := GetDesktopWindow()
      ENDIF
   ENDIF

   aParent_ := GetClientRect( hParent )
   aPoint_ := { ( aParent_[ 3 ] / 2 ) , ( aParent_[ 4 ] / 2 ) }
   ClientToScreen( hParent, aPoint_ )
   aPoint_[ 1 ] -= ( iCWidth / 2 )
   aPoint_[ 2 ] -= ( iCHeight / 2 )
   ScreenToClient( hParent, aPoint_ )
   aPoint_[ 1 ] := Max( 0, aPoint_[ 1 ] )
   aPoint_[ 2 ] := Max( 0, aPoint_[ 2 ] )
   ClientToScreen( hParent, aPoint_ )

   If NewX # NIL .AND. NewY # NIL
      MoveWindow( hWnd, NewX, NewY, iCWidth, iCHeight, .F. )
   Else
      MoveWindow( hWnd, aPoint_[ 1 ] , aPoint_[ 2 ] , iCWidth, iCHeight, .F. )
   EndIf

   Return( NIL )


*-----------------------------------------------------------------------------*
/*
    # DEFINE SWP_NOSIZE 1
    # DEFINE SWP_NOMOVE 2
    # DEFINE SWP_NOZORDER 4
    # DEFINE SWP_NOACTIVATE 16
*/
*-----------------------------------------------------------------------------*

Function SetOnTop( hDlg, nmode )

   Local arect := GetWindowRect( hDlg )

   Return SetWindowPos( hDlg, nmode, ;
                        arect[ 1 ] , ;
                        arect[ 2 ] , ;
                        0, ;
                        0, ;
                        SWP_NOSIZE + SWP_NOMOVE + SWP_NOACTIVATE )

*-----------------------------------------------------------------------------*

Function asString( x )

   Local v := ValType( x )

   Do Case
   Case v == "C"
   Case v == "N"
      Return AllTrim( str( x ) )
   Case v == "L"
      If x
         Return ".T."
      Else
         Return ".F."
      EndIf
   Case v == "D"
      Return dtoc( x )
   Case v == "U"
      Return "NIL"
   Case v == "A"
      Return "<Array>"
   Case v == "O"
      Return x:classname()
   Case v == "B"
      Return "<Block>"
   Otherwise
      Return ""
   End Case

   Return( x )


*-----------------------------------------------------------------------------*

Function str2a ( string, parser )

   Local retar    := { }
   Local commapos := 0

   If parser == NIL
      parser := ','
   EndIf

   Do While Len( string ) > 0
      commapos := at( parser, string )
      If commapos > 0
         aAdd( retar, Left( string, commapos - 1 ) )
         string := SubStr( string, commapos + Len( parser ) )
      Else
         aAdd( retar, string )
         string := ''
      EndIf
   EndDo

   Return( retar )

*-----------------------------------------------------------------------------*
FUNCTION a2str( a, parser )

  LOCAL retstr := ''
  LOCAL i

  IF parser == NIL
    parser := ','
  ENDIF

  FOR i := 1 TO Len( a )
    IF i # 1
      retstr += parser
    ENDIF
    retstr += asstring( a[ i ] )
  NEXT

  RETURN( retstr )




*-----------------------------------------------------------------------------*

STATIC FUNCTION cTypes2aTypes(cTypes)

LOCAL aTypes:={}
LOCAL cType
LOCAL i,j

      ctypes:=strtran(ctypes," ","")
      cTypes:=substr(cTypes,2)
      DO While !EMPTY(cTypes)
         IF LEFT(ctypes,1)=="{"
            AADD(atypes,ctypes2atypes(@ctypes))
            cTypes:=SUBSTR(ctypes,1)
         ELSE
            if (i :=AT( ",", cTypes )) > 0
               ctype:=Left( cTypes, i - 1 )
            else
               ctype:=ctypes
            endif
            IF (j:=AT("}",ctype)) > 0

               // TBD: add multiple arrays!!

                ctype:=LEFT(ctype,j-1)
                IF !EMPTY(ctype)
                  AADD(atypes,ctype)
                endif
                cTypes := SubStr( cTypes, j + 1 )
                exit
            Endif
            if !EMPTY(cType)
               AADD(atypes,ctype)
            endif
            cTypes := SubStr( cTypes, i + 1 )
         Endif
      enddo
      RETURN(aTypes)


* ..........................................................................

// use preprocessor a2bin()

Function Array2Bin( aValues, aTypes )

   Local cRet := ""
   Local cTempRet
   Local i, j
   Local nLen := Len( aValues )
   Local nDone := 0
   Local cType
   Local xValue
   Local xType
   Local nQty := 0

   IF VALTYPE(aTypes)=="C"
     atypes:=cTypes2atypes(atypes)
   endif

   For i := 1 To nLen

      cType     := ValType( xValue := aValues[ i ] )

      If nQty == 0

         xType  := aTypes[ i ]
         If valtype( xType ) == "A"
            If cType == "A"
               If ( cTempRet := Array2Bin( xValue, xType ) ) != NIL
                  cRet += cTempRet
                  nDone ++
                  Loop
               Else
                  Return( NIL )
               EndIf
            Else
               Return( NIL )
            EndIf
         EndIf

         If ( j := AT( "[", xType ) ) > 0
            nQty := VAL( SubStr( xType, j + 1, Len( xType ) - j - 1 ) )
            xType := VAL( Left( xType, j - 1 ) )
         Else
            xType := VAL( xType )
            nQty := 1
         EndIf

      EndIf

      Do Case
      Case xType == CTYPE_SHORT .OR. xType == CTYPE_UNSIGNED_SHORT
         If !( cType == "N" )
            Return NIL
         EndIf
         cRet += W2BIN( xValue )

      Case xType == CTYPE_INT .OR. xType == CTYPE_UNSIGNED_INT
         If !( cType == "N" )
            Return NIL
         EndIf
         cRet += L2BIN( xValue )

      Case xType == CTYPE_CHAR .OR. xType == CTYPE_UNSIGNED_CHAR
         If cType == "N"
            cRet += CHR( xValue )
         ElseIf cType == "C"
            cRet += Left( xValue, nQty )
            nQty := 1
         Else
            Return NIL
         EndIf

      Case xType == CTYPE_LONG
         If cType == "N"
            cRet += L2BIN( xValue )
         ElseIf cType == "L"
            cRet += L2BIN( iif( xValue, 1, 0 ) )
         Else
            Return NIL
         EndIf

      Case xType == CTYPE_UNSIGNED_LONG
         If !( cType == "N" )
            Return NIL
         EndIf
         cRet += U2BIN( xValue )

      Case xType == CTYPE_CHAR_PTR .OR. xType == CTYPE_UNSIGNED_CHAR_PTR
         If !( cType == "C" )
            Return NIL
         EndIf
         cRet += xValue

      Case xType == CTYPE_BOOL
         If cType == "L"
            cRet += U2BIN( iif( xValue, 1, 0 ) )
         ElseIf cType == "N"
            cRet += U2BIN( xValue )
         Else
            Return NIL
         EndIf

      Case xType == CTYPE_FLOAT
         If !( cType == "N" )
            Return NIL
         EndIf
         cRet += F2BIN( xValue )

      Case xType == CTYPE_DOUBLE
         If !( cType == "N" )
            Return NIL
         EndIf
         cRet += D2BIN( xValue )

      Otherwise
         Return NIL

      EndCase

      IF --nQty ==0
        nDone ++
      endif

   Next

   Return iif( nDone == nLen, cRet, NIL )


*  ..........................................................................

// use preprocessor bin2a()

Function Bin2Array( cBin, aTypes )

   Local aArr := { }
   Local nLen
   Local xType
   Local nDone := 0
   Local i := 0
   Local j
   Local nQty := 0

   IF VALTYPE(atypes)=="C"
      atypes:=ctypes2atypes(atypes)
   endif

   nLen := Len( aTypes )

   Do While nDone < nLen

      If nQty == 0

         If ++ i > Len( aTypes )
            Return( NIL )
         EndIf
         xType  := aTypes[ i ]
         If valtype( xType ) == "A"
            aAdd( aArr, Bin2Array( @cBin, xType ) )
            nDone ++
            Loop
         EndIf

         If ( j := AT( "[", xType ) ) > 0
            nQty := VAL( SubStr( xType, j + 1, Len( xType ) - j - 1 ) )
            xType := VAL( Left( xType, j - 1 ) )
         Else
            xType := VAL( xType )
            nQty := 1
         EndIf

      EndIf

      Do Case
      Case xType == CTYPE_INT .OR. xType == CTYPE_UNSIGNED_INT
         aAdd( aArr, BIN2L( cBin ) )
         cBin := SubStr( cBin, 5 )

      Case xType  == CTYPE_SHORT .OR. xType == CTYPE_UNSIGNED_SHORT
         aAdd( aArr, BIN2W( cBin ) )
         cBin := SubStr( cBin, 3 )

      Case xType == CTYPE_CHAR .OR. xType == CTYPE_UNSIGNED_CHAR
         aAdd( aArr, Left( cBin, nQty ) )
         cBin := SubStr( cBin, nQty + 1 )
         nQty := 1

      Case xType == CTYPE_LONG
         aAdd( aArr, BIN2L( cBin ) )
         cBin := SubStr( cBin, 5 )

      Case xType == CTYPE_CHAR_PTR .OR. xType == CTYPE_UNSIGNED_CHAR_PTR  // not sure
         aAdd( aArr, BIN2L( cBin ) )
         cBin := SubStr( cBin, 5 )

         /*
         AADD( aArr, LEFT( cBin, nQty )
         cBin := SUBSTR( cBin, nQty + 1 )
         */

      CASE xType == CTYPE_UNSIGNED_LONG
        aAdd( aArr, BIN2U( cBin ) )
        cBin := SubStr( cBin, 5 )

      Case xType == CTYPE_BOOL
         aAdd( aArr, iif( BIN2U( cBin ) == 0, .F., .T. ) )
         cBin := SubStr( cBin, 5 )

      Case xType == CTYPE_FLOAT
         aAdd( aArr, BIN2F( cBin ) )
         cBin := SubStr( cBin, 5 )

      Case xType == CTYPE_DOUBLE
         aAdd( aArr, BIN2D( cBin ) )
         cBin := SubStr( cBin, 9 )

      Otherwise
         Return NIL

      EndCase

      if --nQty == 0
         nDone ++
      endif

   EndDo

   Return iif( nDone == nLen, aArr, NIL )


*-----------------------------------------------------------------------------*

// translates current Clipper/Harbour colour to Windows RGB() values

FUNCTION WinColors( nfg, nbg )

  LOCAL acolors := { ; // valueas below are PROBABLY (!) correct.
  { 'N' , RGB( 0, 0, 0 ) } , ;
  { 'B' , RGB( 0, 0, 128 ) } , ;
  { 'G' , RGB( 0, 128, 0 ) } , ;
  { 'BG' , RGB( 0, 128, 128 ) } , ;
  { 'GB' , RGB( 0, 128, 128 ) } , ;
  { 'R' , RGB( 128, 0, 0 ) } , ;
  { 'RB' , RGB( 128, 0, 128 ) } , ;
  { 'BR' , RGB( 128, 0, 128 ) } , ;
  { 'GR' , RGB( 128, 128, 0 ) } , ;
  { 'RG' , RGB( 128, 128, 0 ) } , ;
  { 'W' , RGB( 192, 192, 192 ) } , ;
  { 'N+' , RGB( 128, 128, 128 ) } , ;
  { 'B+' , RGB( 0, 0, 255 ) } , ;
  { 'G+' , RGB( 0, 255, 0 ) } , ;
  { 'BG+', RGB( 0, 255, 255 ) } , ;
  { 'GB+', RGB( 0, 255, 255 ) } , ;
  { 'R+' , RGB( 255, 0, 0 ) } , ;
  { 'RB+', RGB( 255, 0, 255 ) } , ;
  { 'BR+', RGB( 255, 0, 255 ) } , ;
  { 'GR+', RGB( 255, 255, 0 ) } , ;
  { 'RG+', RGB( 255, 255, 0 ) } , ;
  { 'W+' , RGB( 255, 255, 255 ) } }

  LOCAL ccolor := Left( setcolor( ) , at( ',', setcolor( ) ) - 1 )
  LOCAL ishibg := ( '*' $ ccolor )
  LOCAL cfg := Upper(StrTran( Left( ccolor, at( '/', ccolor ) - 1 ) , '*', '' ))
  LOCAL cbg := Upper(StrTran(SubStr( ccolor, at( '/', ccolor ) + 1 ),'*','')) + iif( ishibg, '+', '' )
  LOCAL npos := 0

  nfg := RGB( 255, 255, 255 )
  nbg := RGB( 0, 0, 0 )

  IF ( npos := aScan( acolors, { | x | x[ 1 ] == cfg } ) ) > 0
    nfg := acolors[ npos, 2 ]
  ENDIF
  IF ( npos := aScan( acolors, { | x | x[ 1 ] == cbg } ) ) > 0
    nbg := acolors[ npos, 2 ]
  ENDIF

  RETURN( NIL )

*-----------------------------------------------------------------------------*

FUNCTION Proper(cStr)

local n,ch,nLen
local c:=""
local l:=.T.

cStr:=strtran(lower(alltrim(cStr)),"_"," ")
nlen:=len(cStr)

FOR n:=1 TO nLen
   ch:=substr(cStr,n,1)
   c+=iif(l,upper(ch),ch)
   l:=(ch==" ")
NEXT

RETURN(c)

//-----------------------------------------------------------------------------

Function FontCreate(cFont,nSize,lBold,lItalic,lVert,lUnder)
local aFont,hFont,nAngle
DEFAULT lItalic TO .F.
DEFAULT lBold   TO .T.
DEFAULT lUnder  TO .F.
DEFAULT lVert   TO .F.
nAngle:=IIF(lVert,900,0)
aFont := {nSize, 0, nAngle, nAngle, IIF(lBold,700,0), lItalic, lUnder, .F., 1, 1, 0, 0, 0, cFont}
hFont := CreateFont(aFont)
return hFont

//-----------------------------------------------------------------------------

FUNCTION GetMessageFont( nWeight ) // retrieves the current font used in MessageBox

   LOCAL cBuff
   LOCAL ncm IS NONCLIENTMETRICS

   ncm:cbSize := ncm:sizeof()
   cBuff := ncm:value

   SystemParametersInfo( SPI_GETNONCLIENTMETRICS, ncm:sizeof(), @cBuff, 0 )
   ncm:Buffer( cBuff )

   IF nWeight != NIL
      ncm:lfMessageFont:lfWeight := nWeight
   ENDIF

RETURN CreateFontIndirect( ncm:lfMessageFont:value )

