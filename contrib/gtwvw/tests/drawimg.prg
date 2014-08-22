/* This is a sample of how to display an image file (.bmp, .gif, .jpg)
   in four ways:
   1. Image stretched to fit a region
      (all four coordinates are defined)
   2. Height is defined, width is proportional to height
      (right coordinate is passed as NIL)
   3. Width is defined, height is proportional to width
      (bottom coordinate is passed as NIL)
   4. Actual image size
      (bottom and right coordinate are NIL)

   Also shown in this sample:
   1. Image Caching
      Image is read from file only once, and then cached.
      Test: After image is displayed, delete the image file.
            Image should be still displayed with no problem,
            since gtwvw already store it in memory.
   2. Transparency
      If this option is used, topleft pixel is used as the transparent mask
      of the image. */

#require "gtwvw"

#include "inkey.ch"
#include "setcurs.ch"
#include "hbclass.ch"

// WPAINTOBJ types
#define WPAINTOBJ_LABEL                  1
#define WPAINTOBJ_SHADEDRECT             2
#define WPAINTOBJ_TEXTBOX                3
#define WPAINTOBJ_PROGRESSBAR            4
#define WPAINTOBJ_BOXGET                 5
#define WPAINTOBJ_BOXRAISED              6
#define WPAINTOBJ_BOXRECESSED            7
#define WPAINTOBJ_BOXGROUP               8
#define WPAINTOBJ_BOXGROUPRAISED         9
#define WPAINTOBJ_IMAGE                 10
#define WPAINTOBJ_OUTLINE               11
#define WPAINTOBJ_LINE                  12
#define WPAINTOBJ_ELLIPSE               13
#define WPAINTOBJ_RECTANGLE             14
#define WPAINTOBJ_FILLRECTANGLE         15
#define WPAINTOBJ_ROUNDRECT             16
#define WPAINTOBJ_FOCUSRECT             17
#define WPAINTOBJ_GRIDHORZ              18
#define WPAINTOBJ_GRIDVERT              19
#define WPAINTOBJ_PICTURE               20
#define WPAINTOBJ_LABELEX               21
#define WPAINTOBJ_LINEEX                22
#define WPAINTOBJ_OUTLINEEX             23

// WCTRLOBJ types
#define WCTRLOBJ_PUSHBUTTON             1001
#define WCTRLOBJ_VSCROLLBAR             1002
#define WCTRLOBJ_HSCROLLBAR             1003
#define WCTRLOBJ_COMBOBOX               1004

// OBJECT OPERATION
#define WOBJ_ADD_OVERWRITE      1   // overwrite if exists
#define WOBJ_ADD_APPEND         2   // append even if exists (the same id can coexist)
#define WOBJ_ADD_NEWONLY        3   // cancel if exists

STATIC s_aPObjList := {}

PROCEDURE Main()

   LOCAL ntop, nleft, nbot, nrig, nmidver, nmidhor
   LOCAL cpict := hb_DirBase() + "vouch1.gif"
   LOCAL ltransp := .F.
   LOCAL nMaxCache
   LOCAL i, j, oWPaint
   LOCAL GetList := {}

#if defined( __HBSCRIPT__HBSHELL ) .AND. defined( __PLATFORM__WINDOWS )
   hbshell_gtSelect( "GTWVW" )
#endif

   ntop := 7
   nleft := 3
   nbot := MaxRow() - 2
   nrig := MaxCol() - 2
   nmidver := Int( ( ntop + nbot ) / 2 )
   nmidhor := Int( ( nleft + nrig ) / 2 )

   nMaxCache := wvw_SetMaxBMCache()

   SetColor( "N/W,N/GR*,,,N/W*" )
   wvw_SetCodepage( , 255 )
   wg_ResetWPaintObj( 0 )
   DO WHILE .T.
      CLS
      SetCursor( SC_NORMAL )
      cpict := PadR( cpict, 40 )
      @ 0, 0 SAY "FileName  :" GET cpict PICTURE "@K" VALID hb_FileExists( AllTrim( cpict ) )
      @ 1, 0 SAY "Transpar? :" GET ltransp PICTURE "Y"
      @ 2, 0 SAY "Max Cache :" GET nMaxCache PICTURE "999"
      @ 3, 0 SAY "NumOfCache=" + Str( wvw_NumBMCache(), 3 ) + ", " + ;
                 "Max NumOfCache=" + Str( wvw_SetMaxBMCache(), 3 )
      READ
      IF LastKey() == K_ESC
         EXIT
      ENDIF
      wvw_SetMaxBMCache( nMaxCache )
      @ 3, 0 SAY "NumOfCache=" + Str( wvw_NumBMCache(), 3 ) + ", " + ;
                 "Max NumOfCache=" + Str( wvw_SetMaxBMCache(), 3 )

      @ 5, 0 SAY "TOPLEFT: stretched image                 TOPRIGHT: fit vertically (proportional)"
      @ 6, 0 SAY "BOTLEFT: fit horizontally (proportional) BOTRIGHT: actual image size"

      cpict := AllTrim( cpict )

      // wvw_LoadPicture( 1, cpict ) // 2006-07-07

      SetCursor( SC_NONE )
      DispBegin()
      FOR i := ntop TO nbot
         FOR j := nleft TO nrig
            @ i, j SAY "X"
         NEXT
      NEXT
      @ ntop, nmidhor TO nbot, nmidhor
      @ nmidver, nleft TO nmidver, nrig
      @ ntop, nleft TO nbot, nrig
      DispEnd()

      // topleft panel, stretch/fit to panel
      oWPaint := wPaintObj():New( 0, WPAINTOBJ_IMAGE, "TOPLEFT", ntop + 1, nleft + 1, nmidver - 1, nmidhor - 1, , ltransp )
      oWPaint:cImage := cpict
      wg_AddWPaintObj( 0, oWPaint )

      // topright panel, fit vertically
      oWPaint := wPaintObj():New( 0, WPAINTOBJ_IMAGE, "TOPRIGHT", ntop + 1, nmidhor + 1, nmidver - 1, , , ltransp )
      oWPaint:cImage := cpict
      wg_AddWPaintObj( 0, oWPaint )

      // botleft panel, fit horizontally
      oWPaint := wPaintObj():New( 0, WPAINTOBJ_IMAGE, "BOTLEFT", nmidver + 1, nleft + 1, , nmidhor - 1, , ltransp )
      oWPaint:cImage := cpict
      wg_AddWPaintObj( 0, oWPaint )

      // botright panel, actual image size
      oWPaint := wPaintObj():New( 0, WPAINTOBJ_IMAGE, "BOTRIGHT", nmidver + 1, nmidhor + 1, , , , ltransp )
      oWPaint:cImage := cpict
      wg_AddWPaintObj( 0, oWPaint )

      Inkey( 0 )

      // delete all image objects
      wg_DelWPaintObj( 0, WPAINTOBJ_IMAGE )
   ENDDO
   SetCursor( SC_NORMAL )

   RETURN

// --- simple wpaint organizer ---

CREATE CLASS wGUIObj STATIC

   VAR nWinNum                      // parent window's number
   VAR lVisible                     // is the object visible
   VAR nType                        // Type
   VAR cId                          // Id
   VAR nRow1, nCol1, nRow2, nCol2   // mouse object region
   VAR aOffTLBR                     // offset in pixels

ENDCLASS

/* --- */

CREATE CLASS wPaintObj FROM wGUIObj STATIC

   // image like wvtimage
   VAR cImage
   VAR lTransp

   METHOD New( nWinNum, nType, cId, nRow1, nCol1, nRow2, nCol2, aOffTLBR, lTransp )
   METHOD Draw()
   METHOD Undraw()
   METHOD Hide()
   METHOD Show()

ENDCLASS

METHOD New( nWinNum, nType, cId, nRow1, nCol1, nRow2, nCol2, aOffTLBR, lTransp ) CLASS wPaintObj

   ::nWinNum := nWinNum
   ::lVisible := .T.

   ::nType := nType
   ::cId := cId
   ::nRow1 := nRow1
   ::nCol1 := nCol1
   ::nRow2 := nRow2
   ::nCol2 := nCol2

   ::aOffTLBR := AClone( hb_defaultValue( aOffTLBR, { 0, 0, 0, 0 } ) )

   ::lTransp := hb_defaultValue( lTransp, .F. )

   RETURN Self

METHOD PROCEDURE Draw() CLASS wPaintObj

   IF ::lVisible
      DO CASE
      CASE ::nType == WPAINTOBJ_IMAGE
         IF ! Empty( ::cImage )
            wvw_DrawImage( ::nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ;
               ::cImage, ::aOffTLBR, ::lTransp )
         ENDIF

      OTHERWISE
         // lBoxErrMessage()
      ENDCASE
   ENDIF

   RETURN

// undraw the object
// normally this is called with ::lVisible == .F.,
// otherwise the object will be redrawn by WVW_PAINT
METHOD PROCEDURE Undraw() CLASS wPaintObj

   LOCAL cScreen
   LOCAL nRow1, nCol1, nRow2, nCol2

   // to be safer, the area can be enlarged first
   LOCAL nMaxRow := MaxRow()
   LOCAL nMaxCol := MaxCol()

   DO CASE
   CASE ::nType == WPAINTOBJ_LABEL
      nRow1 := ::nRow1
      nCol1 := ::nCol1
      nRow2 := ::nRow2
      nCol2 := ::nCol2
   OTHERWISE
      nRow1 := Max( ::nRow1 - 1, 0 )
      nCol1 := Max( ::nCol1 - 1, 0 )
      nRow2 := Min( ::nRow2 + 1, nMaxRow )
      nCol2 := Min( ::nCol2 + 1, nMaxCol )
   ENDCASE

   cScreen := SaveScreen( nRow1, nCol1, nRow2, nCol2 )
   DispBegin()
   RestScreen( nRow1, nCol1, nRow2, nCol2, cScreen )
   DispEnd()

   RETURN

METHOD PROCEDURE Hide() CLASS wPaintObj

   // temporarily hides the object
   ::lVisible := .F.
   ::Undraw()

   RETURN

METHOD PROCEDURE Show() CLASS wPaintObj

   // show the object
   ::lVisible := .T.
   ::draw()

   RETURN

/* --- */

// clears all wPaint objects from window nWinNum
// if nObjNum specified, clears object >= nObjNum
STATIC PROCEDURE wg_ResetWPaintObj( nWinNum, nObjNum )

   DO WHILE Len( s_aPObjList ) < nWinNum + 1
      AAdd( s_aPObjList, {} )
   ENDDO

   ASize( s_aPObjList[ nWinNum + 1 ], hb_defaultValue( nObjNum, 0 ) )

   RETURN

// adds a WPaint object oWPaint into window nWinNum
// returns ::cId if successful. "" if failed.
STATIC PROCEDURE wg_AddWPaintObj( nWinNum, oWPaint, lStrict )

   LOCAL i

   // exist nType + cId ?
   IF ( i := AScan( s_aPObjList[ nWinNum + 1 ], {| x | x:nType == oWPaint:nType .AND. x:cId == oWPaint:cId } ) ) > 0
      // so we are about to overwrite now...
      // ::Hide() is ideal, but it can be slow
      // let's do it only of user want strict/perfect operation
      IF hb_defaultValue( lStrict, .F. )
         s_aPObjList[ nWinNum + 1 ][ i ]:Hide()
      ELSE
         s_aPObjList[ nWinNum + 1 ][ i ]:lVisible := .F.
      ENDIF
      s_aPObjList[ nWinNum + 1 ][ i ] := oWPaint
   ELSE
      AAdd( s_aPObjList[ nWinNum + 1 ], oWPaint )
   ENDIF

   // if it is visible, draw it now!
   IF oWPaint:lVisible
      oWPaint:draw()
   ENDIF

   RETURN

// deletes a WPaint object oWPaint from window nWinNum
// returns number of object deleted.
//
// NOTE: if cId is NIL, delete all object of type nType
STATIC PROCEDURE wg_DelWPaintObj( nWinNum, nType, cId, lStrict )

   LOCAL i
   LOCAL lDelAll := ! HB_ISSTRING( cId )
   LOCAL nLen

   // is nType set?
   IF nType >= 1
      // exist nType + cId ?
      i := 1
      nLen := Len( s_aPObjList[ nWinNum + 1 ] )
      DO WHILE i <= nLen
         IF s_aPObjList[ nWinNum + 1 ][ i ]:nType == nType .AND. ;
            ( lDelAll .OR. s_aPObjList[ nWinNum + 1 ][ i ]:cId == cId )
            IF hb_defaultValue( lStrict, .F. )
               s_aPObjList[ nWinNum + 1 ][ i ]:Hide()
            ELSE
               s_aPObjList[ nWinNum + 1 ][ i ]:lVisible := .F.
            ENDIF
            hb_ADel( s_aPObjList[ nWinNum + 1 ], i, .T. )
            nLen--
         ELSE
            i++
         ENDIF
      ENDDO
   ENDIF

   RETURN

FUNCTION WVW_Paint( nWinNum )  /* must be a public function */

   IF Len( s_aPObjList ) >= nWinNum + 1
      // simple redraw, ignoring wpaint obj dependency with each other:
     AEval( s_aPObjList[ nWinNum + 1 ], {| oWPaint | oWPaint:draw() } )
   ENDIF

   RETURN 0
