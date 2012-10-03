/*
 * $Id$
 */

/*
   compile and link this file together with wutil.prg

   This is a sample of how to display an image file (.bmp,.gif,.jpg)
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
      of the image.

   Remarks:
   Image drawing and wvw_paint management are performed by WUTIL.PRG.
   WUTIL.PRG is a simple "middle module" interfacing an application with
   gtwvw.
 */
#include "common.ch"
#include "hbclass.ch"

//WPAINTOBJ types
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

//WCTRLOBJ types
#define WCTRLOBJ_PUSHBUTTON             1001
#define WCTRLOBJ_VSCROLLBAR             1002
#define WCTRLOBJ_HSCROLLBAR             1003
#define WCTRLOBJ_COMBOBOX               1004

//OBJECT OPERATION
#define WOBJ_ADD_OVERWRITE      1   //overwrite if exists
#define WOBJ_ADD_APPEND         2   //append even if exists (the same id can coexist)
#define WOBJ_ADD_NEWONLY        3   //cancel if exists

STATIC s_aPObjList := {}

proc main
local ntop := 7,;
      nleft:= 3,;
      nbot := maxrow()-2,;
      nrig := maxcol()-2,;
      nmidver:=INT((ntop+nbot)/2),;
      nmidhor:=INT((nleft+nrig)/2)
local cpict := "vouch1.gif",;
      ltransp := .f.,;
      nMaxCache := wvw_SetMaxBMcache()
local i,j,oWPaint
local getlist := {}
   setcolor("N/W,N/GR*,,,N/W*")
   wvw_setcodepage(,255)
   wg_ResetWPaintObj( 0 )
   do while .t.
      CLS
      setcursor(1)
      cpict := padr(cpict,40)
      @ 0,0 say "FileName  :" get cpict pict "@K" valid file(alltrim(cpict))
      @ 1,0 say "Transpar? :" get ltransp pict "Y"
      @ 2,0 say "Max Cache :" get nMaxCache pict "999"
      @ 3,0 say "NumOfCache=" + transform(wvw_numBMcache(),"999") +;
                ", Max NumOfCache=" + transform(wvw_SetMaxBMcache(),"999")
      read
      if lastkey()==27
         exit
      endif
      wvw_SetMaxBMcache(nMaxCache)
      @ 3,0 say "NumOfCache=" + transform(wvw_numBMcache(),"999") +;
                ", Max NumOfCache=" + transform(wvw_SetMaxBMcache(),"999")

      @ 5,0 say "TOPLEFT: stretched image                 TOPRIGHT: fit vertically (proportional)"
      @ 6,0 say "BOTLEFT: fit horizontally (proportional) BOTRIGHT: actual image size"

      cpict := alltrim(cpict)

      //wvw_loadpicture( 1, cpict ) //20060707

      setcursor(0)
      DISPBEGIN()
      for i := ntop to nbot
         for j := nleft to nrig
            @ i,j say "X"
         next
      next
      @ ntop,nmidhor to nbot,nmidhor
      @ nmidver,nleft to nmidver,nrig
      @ ntop,nleft to nbot,nrig
      DISPEND()

      * topleft panel, stretch/fit to panel
      oWPaint := wPaintObj():New(0, WPAINTOBJ_IMAGE, "TOPLEFT", ntop+1, nleft+1, nmidver-1, nmidhor-1, NIL, ltransp)
      oWPaint:cImage := cpict
      wg_AddWPaintObj( 0, oWPaint )

      * topright panel, fit vertically
      oWPaint := wPaintObj():New(0, WPAINTOBJ_IMAGE, "TOPRIGHT", ntop+1, nmidhor+1, nmidver-1, NIL, NIL, ltransp)
      oWPaint:cImage := cpict
      wg_AddWPaintObj( 0, oWPaint )

      * botleft panel, fit horizontally
      oWPaint := wPaintObj():New(0, WPAINTOBJ_IMAGE, "BOTLEFT", nmidver+1, nleft+1, NIL, nmidhor-1, NIL, ltransp)
      oWPaint:cImage := cpict
      wg_AddWPaintObj( 0, oWPaint )

      * botright panel, actual image size
      oWPaint := wPaintObj():New(0, WPAINTOBJ_IMAGE, "BOTRIGHT", nmidver+1, nmidhor+1, NIL, NIL, NIL, ltransp)
      oWPaint:cImage := cpict
      wg_AddWPaintObj( 0, oWPaint )

      inkey(0)

      * delete all image objects
      wg_DelWPaintObj(0, WPAINTOBJ_IMAGE, NIL)
   enddo //while .t.
   setcursor(1)
return


*************** simple wpaint organizer *******

/*********************************************************/

CLASS wGUIObj
   DATA nWinNum                      //parent window's number
   DATA lVisible                     //is the object visible
   DATA nType                        //Type
   DATA cId                          //Id
   DATA nRow1, nCol1, nRow2, nCol2   //mouse object region
   DATA aOffTLBR                     //offset in pixels

ENDCLASS //wGUIObj

/*********************************************************/

CLASS wPaintObj FROM wGUIObj
   * image like wvtimage
   DATA cImage
   DATA lTransp

   METHOD New()
   METHOD Draw()
   METHOD Undraw()
   METHOD Hide()
   METHOD Show()
ENDCLASS //wPaintOBJ

METHOD New(nWinNum, nType, cId, nRow1, nCol1, nRow2, nCol2, aOffTLBR, lTransp) CLASS wPaintObj
   default aOffTLBR to {0,0,0,0}
   default lTransp to .f.

   ::nWinNum := nWinNum
   ::lVisible := .t.

   ::nType := nType
   ::cId := cId
   ::nRow1 := nRow1
   ::nCol1 := nCol1
   ::nRow2 := nRow2
   ::nCol2 := nCol2

   if !(valtype(aOffTLBR)=="A")
      aOffTLBR := {0,0,0,0}
   endif

   ::aOffTLBR := aclone(aOffTLBR)

   ::lTransp := lTransp
RETURN Self

METHOD Draw() CLASS wPaintObj
   if !::lVisible
      return NIL
   endif

   do case
   case ::nType==WPAINTOBJ_IMAGE
      if !empty(::cImage)
         WVW_DRAWIMAGE( ::nWinNum, ::nRow1, ::nCol1, ::nRow2, ::nCol2, ;
                        ::cImage, ::aOffTLBR, ::lTransp )
      endif

   otherwise
      * lBoxErrMessage()
   endcase

RETURN NIL  //DRAW()

METHOD Undraw() CLASS wPaintObj
* undraw the object
* normally this is called with ::lVisible == .f.,
* otherwise the object will be redrawn by WVW_PAINT
local cScreen
local nRow1, nCol1, nRow2, nCol2, nMaxRow, nMaxCol
   * to be safer, the area can be enlarged first
   nMaxRow := maxrow()
   nMaxCol := maxcol()

   do case
   case ::nType==WPAINTOBJ_LABEL
      nRow1 := ::nRow1
      nCol1 := ::nCol1
      nRow2 := ::nRow2
      nCol2 := ::nCol2
   otherwise
      nRow1 := max(::nRow1-1, 0)
      nCol1 := max(::nCol1-1, 0)
      nRow2 := min(::nRow2+1, nMaxRow)
      nCol2 := min(::nCol2+1, nMaxCol)
   endcase

   cScreen := savescreen(nRow1, nCol1, nRow2, nCol2)
   DISPBEGIN()
   restscreen(nRow1, nCol1, nRow2, nCol2, cScreen)
   DISPEND()
RETURN NIL //undraw()

METHOD Hide() CLASS wPaintObj
* temporarily hides the object
   ::lVisible := .f.
   ::Undraw()
RETURN NIL

METHOD Show() CLASS wPaintObj
* show the object
   ::lVisible := .t.
   ::draw()
RETURN NIL

/*********************************************************/

function wg_ResetWPaintObj( nWinNum, nObjNum, lStrict )
* clears all wPaint objects from window nWinNum
* if nObjNum specified, clears object >= nObjNum
local i
   default nObjNum to 0
   default lStrict to .f.

   do while len(s_aPObjList) < nWinNum+1
      aadd( s_aPObjList, {} )
   enddo

      asize(s_aPObjList[ nWinNum+1 ], nObjNum)

return .t.

function wg_AddWPaintObj( nWinNum, oWPaint, lStrict, nOperation )
* adds a WPaint object oWPaint into window nWinNum
* returns ::cId if successful. "" if failed.
local i
local nLen, aRect //20050720
   default lStrict to .f.
   default nOperation to WOBJ_ADD_OVERWRITE

   * simplified:
   nOperation := WOBJ_ADD_OVERWRITE

   * parameter checking...
   * ...

      * exist nType + cId ?
      i := ASCAN(s_aPObjList[ nWinNum+1 ], {| x | x:nType==oWPaint:nType .and. x:cId==oWPaint:cId})

   if i > 0
      * so we are about to overwrite now...
      //::Hide() is ideal, but it can be slow
      //let's do it only of user want strict/perfect operation
      if lStrict
         s_aPObjList[ nWinNum+1 ][i]:Hide()
      else
         s_aPObjList[ nWinNum+1 ][i]:lVisible := .f.
      endif
      s_aPObjList[ nWinNum+1 ][i] := oWPaint

   else
      aadd( s_aPObjList[ nWinNum+1 ], oWPaint )
   endif

   * if it is visible, draw it now!
   if oWPaint:lVisible
      oWPaint:draw()
   endif
return oWPaint:cId //20040811 was .t.

function wg_DelWPaintObj( nWinNum, nType, cId, lStrict )
* deletes a WPaint object oWPaint from window nWinNum
* returns number of object deleted.
*
*NOTE: if cId is NIL, delete all object of type nType
local i
local lDelAll := (cId == NIL)
local nDeleted := 0
local nLen
local cCurId
   default lStrict to .f.

   * is nType set?
   if nType < 1
      return nDeleted
   endif

   * exist nType + cId ?
   i := 1
   nLen := len(s_aPObjList[ nWinNum+1 ])
   do while i <= nLen
      if s_aPObjList[ nWinNum+1 ][i]:nType==nType .and.;
         (lDelAll .or. s_aPObjList[ nWinNum+1 ][i]:cId==cId)
         if lStrict
            s_aPObjList[ nWinNum+1 ][i]:Hide()
         else
            s_aPObjList[ nWinNum+1 ][i]:lVisible := .f.
         endif
         cCurId := s_aPObjList[ nWinNum+1 ][i]:cId
         adel(s_aPObjList[ nWinNum+1 ], i)
         asize(s_aPObjList[ nWinNum+1 ], --nLen)
         nDeleted++
      else
         i++
      endif
   enddo
return nDeleted
