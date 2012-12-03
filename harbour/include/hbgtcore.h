/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Internal Terminal API
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
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

/* NOTE: The declaration of hb_gtSetPos(), hb_gtGetPos(), hb_gtWrite(),
         hb_gtWriteAt(), hb_gtRepChar(), hb_gtBox(), hb_gtBoxS(), hb_gtBoxD()
         hb_gtInit() differs in parameter types from the original CA-Cl*pper
         versions. [vszakats] */

#ifndef HB_GTCORE_H_
#define HB_GTCORE_H_

#include "hbapigt.h"
#include "hbapicdp.h"

HB_EXTERN_BEGIN

/* convert lower case suffixes to upper */
#define HB_GT_nul    HB_GT_NUL
#define HB_GT_std    HB_GT_STD
#define HB_GT_cgi    HB_GT_CGI
#define HB_GT_pca    HB_GT_PCA
#define HB_GT_crs    HB_GT_CRS
#define HB_GT_sln    HB_GT_SLN
#define HB_GT_win    HB_GT_WIN
#define HB_GT_wvt    HB_GT_WVT
#define HB_GT_dos    HB_GT_DOS
#define HB_GT_os2    HB_GT_OS2
#define HB_GT_tpl    HB_GT_TPL
#define HB_GT_trm    HB_GT_TRM
#define HB_GT_QTc    HB_GT_QTC
#define HB_GT_xvt    HB_GT_XVT
#define HB_GT_xwc    HB_GT_XWC
#define HB_GT_gui    HB_GT_GUI

/* These hacks are needed to force preprocessing if id/x is also a macro */
#define _HB_GT_PREF_( id )      _HB_GT_PREF__( id )
#define _HB_GT_PREF__( id )     HB_GT_##id

#define HB_GT_REQUEST( id )      HB_GT_REQUEST_( _HB_GT_PREF_( id ) )
#define HB_GT_REQUEST_( id )     HB_GT_REQUEST__( id )
#define HB_GT_REQUEST__( id )    HB_FUNC_EXTERN( id ); \
                                 extern void hb_gt_ForceLink_##id( void ); \
                                 void hb_gt_ForceLink_##id( void ) \
                                 { \
                                    HB_FUNC_EXEC( id ); \
                                 }

#define HB_GT_ANNOUNCE( id )     HB_GT_ANNOUNCE_( _HB_GT_PREF_( id ) )
#define HB_GT_ANNOUNCE_( id )    HB_GT_ANNOUNCE__( id )
#define HB_GT_ANNOUNCE__( id )   HB_FUNC( id ) {} HB_FUNC( id##_DEFAULT ) {}

#define HB_GT_DRVNAME( id )      HB_MACRO2STRING( id )

#define HB_GT_FUNC( x )          HB_GT_FUNC_( x, HB_GT_NAME )
#define HB_GT_FUNC_( x, id )     HB_GT_FUNC__( x, id )
#define HB_GT_FUNC__( x, id )    hb##_##id##_##x

/* forward declaration */
struct _HB_GT_BASE;

#define HB_GT_PTR       struct _HB_GT_BASE *

typedef struct
{
   HB_BOOL   (* Lock) ( HB_GT_PTR );
   void      (* Unlock) ( HB_GT_PTR );
   void      (* Init) ( HB_GT_PTR, HB_FHANDLE, HB_FHANDLE, HB_FHANDLE );
   void      (* Exit) ( HB_GT_PTR );
   void *    (* New) ( HB_GT_PTR );
   void      (* Free) ( HB_GT_PTR );
   void      (* Mark) ( HB_GT_PTR );
   HB_BOOL   (* Resize) ( HB_GT_PTR, int, int );
   HB_BOOL   (* SetMode) ( HB_GT_PTR, int, int );
   void      (* GetSize) ( HB_GT_PTR, int *, int * );
   void      (* SemiCold) ( HB_GT_PTR );
   void      (* ColdArea) ( HB_GT_PTR, int, int, int, int );
   void      (* ExposeArea) ( HB_GT_PTR, int, int, int, int );
   void      (* ScrollArea) ( HB_GT_PTR, int, int, int, int, int, HB_USHORT, int, int );
   void      (* TouchLine) ( HB_GT_PTR, int );
   void      (* TouchCell) ( HB_GT_PTR, int, int );
   void      (* Redraw) ( HB_GT_PTR, int, int, int );
   void      (* RedrawDiff) ( HB_GT_PTR );
   void      (* Refresh) ( HB_GT_PTR );
   void      (* Flush) ( HB_GT_PTR );
   int       (* MaxCol) ( HB_GT_PTR );
   int       (* MaxRow) ( HB_GT_PTR );
   HB_BOOL   (* CheckPos) ( HB_GT_PTR, int, int, long * );
   void      (* SetPos) ( HB_GT_PTR, int, int );
   void      (* GetPos) ( HB_GT_PTR, int *, int * );
   HB_BOOL   (* IsColor) ( HB_GT_PTR );
   void      (* GetColorStr) ( HB_GT_PTR, char * );
   void      (* SetColorStr) ( HB_GT_PTR, const char * );
   void      (* ColorSelect) ( HB_GT_PTR, int );
   int       (* GetColor) ( HB_GT_PTR );
   int       (* ColorNum) ( HB_GT_PTR, const char * );
   void      (* ColorsToString) ( HB_GT_PTR, int *, int, char *, int );
   void      (* StringToColors) ( HB_GT_PTR, const char *, int **, int * );
   void      (* GetColorData) ( HB_GT_PTR, int **, int *, int * );
   int       (* GetClearColor) ( HB_GT_PTR );
   void      (* SetClearColor) ( HB_GT_PTR, int );
   HB_USHORT (* GetClearChar) ( HB_GT_PTR );
   void      (* SetClearChar) ( HB_GT_PTR, HB_USHORT );
   int       (* GetCursorStyle) ( HB_GT_PTR );
   void      (* SetCursorStyle) ( HB_GT_PTR, int );
   void      (* GetScrCursor) ( HB_GT_PTR, int *, int *, int * );
   HB_BOOL   (* GetScrChar) ( HB_GT_PTR, int, int, int *, HB_BYTE *, HB_USHORT * );
   HB_BOOL   (* PutScrChar) ( HB_GT_PTR, int, int, int, HB_BYTE, HB_USHORT );
   HB_BOOL   (* GetScrUC) ( HB_GT_PTR, int, int, int *, HB_BYTE *, HB_UCHAR *, HB_BOOL );
   void      (* DispBegin) ( HB_GT_PTR );
   void      (* DispEnd) ( HB_GT_PTR );
   int       (* DispCount) ( HB_GT_PTR );
   HB_BOOL   (* GetChar) ( HB_GT_PTR, int, int, int *, HB_BYTE *, HB_USHORT * );
   HB_BOOL   (* PutChar) ( HB_GT_PTR, int, int, int, HB_BYTE, HB_USHORT );
   long      (* RectSize) ( HB_GT_PTR, int, int, int, int );
   void      (* Save) ( HB_GT_PTR, int, int, int, int, void * );
   void      (* Rest) ( HB_GT_PTR, int, int, int, int, const void * );
   int       (* PutText) ( HB_GT_PTR, int, int, int, const char *, HB_SIZE );
   int       (* PutTextW) ( HB_GT_PTR, int, int, int, const HB_WCHAR *, HB_SIZE );
   void      (* Replicate) ( HB_GT_PTR, int, int, int, HB_BYTE, HB_USHORT, HB_SIZE );
   void      (* WriteAt) ( HB_GT_PTR, int, int, const char *, HB_SIZE );
   void      (* WriteAtW) ( HB_GT_PTR, int, int, const HB_WCHAR *, HB_SIZE );
   void      (* Write) ( HB_GT_PTR, const char *, HB_SIZE );
   void      (* WriteW) ( HB_GT_PTR, const HB_WCHAR *, HB_SIZE );
   void      (* WriteCon) ( HB_GT_PTR, const char *, HB_SIZE );
   void      (* WriteConW) ( HB_GT_PTR, const HB_WCHAR *, HB_SIZE );
   void      (* SetAttribute) ( HB_GT_PTR, int, int, int, int, int );
   void      (* DrawShadow) ( HB_GT_PTR, int, int, int, int, int );
   void      (* Scroll) ( HB_GT_PTR, int, int, int, int, int, HB_USHORT, int, int );
   void      (* ScrollUp) ( HB_GT_PTR, int, int, HB_USHORT );
   void      (* Box) ( HB_GT_PTR, int, int, int, int, const char *, int );
   void      (* BoxW) ( HB_GT_PTR, int, int, int, int, const HB_WCHAR *, int );
   void      (* BoxD) ( HB_GT_PTR, int, int, int, int, const char *, int );
   void      (* BoxS) ( HB_GT_PTR, int, int, int, int, const char *, int );
   void      (* HorizLine) ( HB_GT_PTR, int, int, int, HB_USHORT, int );
   void      (* VertLine) ( HB_GT_PTR, int, int, int, HB_USHORT, int );
   HB_BOOL   (* GetBlink) ( HB_GT_PTR );
   void      (* SetBlink) ( HB_GT_PTR, HB_BOOL );
   void      (* SetSnowFlag) ( HB_GT_PTR, HB_BOOL );
   const char * (* Version) ( HB_GT_PTR, int );
   HB_BOOL   (* Suspend) ( HB_GT_PTR );
   HB_BOOL   (* Resume) ( HB_GT_PTR );
   HB_BOOL   (* PreExt) ( HB_GT_PTR );
   HB_BOOL   (* PostExt) ( HB_GT_PTR );
   void      (* OutStd) ( HB_GT_PTR, const char *, HB_SIZE );
   void      (* OutErr) ( HB_GT_PTR, const char *, HB_SIZE );
   void      (* Tone) ( HB_GT_PTR, double, double );
   void      (* Bell) ( HB_GT_PTR );
   HB_BOOL   (* Info) ( HB_GT_PTR, int, PHB_GT_INFO );
   int       (* Alert) ( HB_GT_PTR, PHB_ITEM, PHB_ITEM, int, int, double );
   int       (* SetFlag) ( HB_GT_PTR, int, int );

   /* internationalization */
   HB_BOOL   (* SetDispCP) ( HB_GT_PTR, const char *, const char *, HB_BOOL );
   HB_BOOL   (* SetKeyCP) ( HB_GT_PTR, const char *, const char * );

   /* keyboard */
   int       (* ReadKey) ( HB_GT_PTR, int );

   int       (* InkeyGet) ( HB_GT_PTR, HB_BOOL fWait, double dSeconds, int iEventMask );
   void      (* InkeyPut) ( HB_GT_PTR, int iKey );
   void      (* InkeyIns) ( HB_GT_PTR, int iKey );
   int       (* InkeyLast) ( HB_GT_PTR, int iEventMask );
   int       (* InkeyNext) ( HB_GT_PTR, int iEventMask );
   void      (* InkeyPoll) ( HB_GT_PTR );
   void      (* InkeySetText) ( HB_GT_PTR, const char * szText, HB_SIZE nLen );
   int       (* InkeySetLast) ( HB_GT_PTR, int iKey );
   void      (* InkeyReset) ( HB_GT_PTR );
   void      (* InkeyExit) ( HB_GT_PTR );

   /* mouse */
   void      (* MouseInit) ( HB_GT_PTR );
   void      (* MouseExit) ( HB_GT_PTR );
   HB_BOOL   (* MouseIsPresent) ( HB_GT_PTR );
   void      (* MouseShow) ( HB_GT_PTR );
   void      (* MouseHide) ( HB_GT_PTR );
   HB_BOOL   (* MouseGetCursor) ( HB_GT_PTR );
   void      (* MouseSetCursor) ( HB_GT_PTR, HB_BOOL );
   int       (* MouseCol) ( HB_GT_PTR );
   int       (* MouseRow) ( HB_GT_PTR );
   void      (* MouseGetPos) ( HB_GT_PTR, int *, int * );
   void      (* MouseSetPos) ( HB_GT_PTR, int, int );
   void      (* MouseSetBounds) ( HB_GT_PTR, int, int, int, int );
   void      (* MouseGetBounds) ( HB_GT_PTR, int *, int *, int *, int * );
   int       (* MouseStorageSize) ( HB_GT_PTR );
   void      (* MouseSaveState) ( HB_GT_PTR, void * );
   void      (* MouseRestoreState) ( HB_GT_PTR, const void * );
   int       (* MouseGetDoubleClickSpeed) ( HB_GT_PTR );
   void      (* MouseSetDoubleClickSpeed) ( HB_GT_PTR, int );
   int       (* MouseCountButton) ( HB_GT_PTR );
   HB_BOOL   (* MouseButtonState) ( HB_GT_PTR, int );
   HB_BOOL   (* MouseButtonPressed) ( HB_GT_PTR, int, int *, int * );
   HB_BOOL   (* MouseButtonReleased) ( HB_GT_PTR, int, int *, int * );
   int       (* MouseReadKey) ( HB_GT_PTR, int );

   /* Graphics API */
   int       (* GfxPrimitive) ( HB_GT_PTR, int, int, int, int, int, int );
   void      (* GfxText) ( HB_GT_PTR, int, int, const char *, int, int, int );

   void      (* WhoCares) ( HB_GT_PTR, void * );

} HB_GT_FUNCS, * PHB_GT_FUNCS;

typedef int ( * GTENTRYP_V )( void );

#define GTFUNCSCOUNT   ( sizeof( HB_GT_FUNCS ) / sizeof( GTENTRYP_V ) )

#define HB_GT_MAX_      32
#define HB_GT_NAME_MAX_ 8

typedef struct _HB_GT_INIT
{
   const char     * id;
   HB_BOOL        (* init) ( PHB_GT_FUNCS );
   PHB_GT_FUNCS   pSuperTable;
   int *          pGtId;
} HB_GT_INIT, * PHB_GT_INIT;


typedef union
{
   struct
   {
      HB_U16   usChar;
      HB_BYTE  bColor;
      HB_BYTE  bAttr;
   } c;
   HB_U32   uiValue;
} HB_SCREENCELL;
typedef HB_SCREENCELL * PHB_SCREENCELL;


typedef struct _HB_GT_BASE
{
   PHB_GT_FUNCS   pFuncTable;

   PHB_ITEM       pMutex;
   int            iUsed;

   int            iRow;             /* cursor row position */
   int            iCol;             /* cursor column position */

   int            iHeight;          /* window height */
   int            iWidth;           /* window width */

   PHB_SCREENCELL screenBuffer;     /* window foreground (board) current buffer */
   PHB_SCREENCELL prevBuffer;       /* window foreground (board) previous buffer */

   HB_BOOL *      pLines;           /* Touched Window lines */
   HB_BOOL        fRefresh;         /* Should Window be refreshed */

   HB_BOOL        fVgaCell;
   HB_BOOL        fIsColor;
   HB_BOOL        fBlinking;
   HB_BOOL        fStdOutCon;
   HB_BOOL        fStdErrCon;
   int            iCursorShape;
   int            iDispCount;
   int            iExtCount;
   HB_USHORT      usClearChar;
   int            iClearColor;
   HB_FHANDLE     hStdIn;
   HB_FHANDLE     hStdOut;
   HB_FHANDLE     hStdErr;

   HB_BOOL        fDispTrans;
   PHB_CODEPAGE   cdpTerm;
   PHB_CODEPAGE   cdpHost;
   PHB_CODEPAGE   cdpBox;
   PHB_CODEPAGE   cdpIn;

   int            iColorIndex;
   int            iColorCount;
   int *          pColor;

   int            iDoubleClickSpeed; /* In milliseconds */
   HB_BOOL        fMouseVisible;
   int            iMouseLastRow;
   int            iMouseLastCol;
   HB_MAXINT      nMouseLeftTimer;
   HB_MAXINT      nMouseRightTimer;
   HB_MAXINT      nMouseMiddleTimer;

   int            defaultKeyBuffer[ HB_DEFAULT_INKEY_BUFSIZE + 1 ];

   int *          inkeyBuffer;
   int            inkeyBufferSize;
   int            inkeyHead;
   int            inkeyTail;
   int            iLastPut;
   int            inkeyLast;
   HB_WCHAR *     StrBuffer;
   HB_SIZE        StrBufferSize;
   HB_SIZE        StrBufferPos;

   PHB_ITEM       pNotifierBlock;
   PHB_ITEM       pInkeyFilterBlock;
   PHB_ITEM       pInkeyReadBlock;
   PHB_ITEM       pCargo;

   void *         pGTData[ HB_GT_MAX_ ];    /* local GT data */

} HB_GT_BASE, * PHB_GT_BASE, * PHB_GT;

extern HB_EXPORT PHB_GT hb_gt_Base( void );
extern HB_EXPORT void hb_gt_BaseFree( PHB_GT pGT );
extern HB_EXPORT PHB_GT hb_gt_ItemBase( PHB_ITEM pItemGT );
extern HB_EXPORT void hb_gt_gcMark( void );

#define HB_GTLOCAL(g)   (g)->pGTData[*HB_GTID_PTR]

#define HB_GTSELF_TERMCP(g)                     ((g)->cdpTerm ? (g)->cdpTerm : HB_GTSELF_HOSTCP(g))
#define HB_GTSELF_HOSTCP(g)                     ((g)->cdpHost ? (g)->cdpHost : hb_vmCDP())
#define HB_GTSELF_BOXCP(g)                      ((g)->cdpBox ? (g)->cdpBox : HB_GTSELF_HOSTCP(g))
#define HB_GTSELF_INCP(g)                       ((g)->cdpIn ? (g)->cdpIn : hb_vmCDP())

#define HB_GTSELF_CPTERM(g)                     ((g)->cdpTerm)
#define HB_GTSELF_CPHOST(g)                     ((g)->cdpHost)
#define HB_GTSELF_CPBOX(g)                      ((g)->cdpBox)
#define HB_GTSELF_CPIN(g)                       ((g)->cdpIn)

#define HB_GTSELF_KEYTRANS(g,k)                 (((k)>=127 && (k)<=255 && (g)->cdpIn) ? hb_cdpGetWC((g)->cdpIn,(HB_UCHAR)(k),0) : 0)


#define HB_GTSELF_LOCK(g)                       (g)->pFuncTable->Lock(g)
#define HB_GTSELF_UNLOCK(g)                     (g)->pFuncTable->Unlock(g)
#define HB_GTSELF_INIT(g,i,o,e)                 (g)->pFuncTable->Init(g,i,o,e)
#define HB_GTSELF_EXIT(g)                       (g)->pFuncTable->Exit(g)
#define HB_GTSELF_NEW(g)                        (g)->pFuncTable->New(g)
#define HB_GTSELF_FREE(g)                       (g)->pFuncTable->Free(g)
#define HB_GTSELF_MARK(g)                       (g)->pFuncTable->Mark(g)
#define HB_GTSELF_RESIZE(g,r,c)                 (g)->pFuncTable->Resize(g,r,c)
#define HB_GTSELF_SETMODE(g,r,c)                (g)->pFuncTable->SetMode(g,r,c)
#define HB_GTSELF_GETSIZE(g,pr,pc)              (g)->pFuncTable->GetSize(g,pr,pc)
#define HB_GTSELF_SEMICOLD(g)                   (g)->pFuncTable->SemiCold(g)
#define HB_GTSELF_COLDAREA(g,t,l,b,r)           (g)->pFuncTable->ColdArea(g,t,l,b,r)
#define HB_GTSELF_EXPOSEAREA(g,t,l,b,r)         (g)->pFuncTable->ExposeArea(g,t,l,b,r)
#define HB_GTSELF_SCROLLAREA(g,t,l,b,r,m,u,v,h) (g)->pFuncTable->ScrollArea(g,t,l,b,r,m,u,v,h)
#define HB_GTSELF_TOUCHLINE(g,r)                (g)->pFuncTable->TouchLine(g,r)
#define HB_GTSELF_TOUCHCELL(g,r,c)              (g)->pFuncTable->TouchCell(g,r,c)
#define HB_GTSELF_REDRAW(g,r,c,l)               (g)->pFuncTable->Redraw(g,r,c,l)
#define HB_GTSELF_REDRAWDIFF(g)                 (g)->pFuncTable->RedrawDiff(g)
#define HB_GTSELF_REFRESH(g)                    (g)->pFuncTable->Refresh(g)
#define HB_GTSELF_FLUSH(g)                      (g)->pFuncTable->Flush(g)
#define HB_GTSELF_MAXCOL(g)                     (g)->pFuncTable->MaxCol(g)
#define HB_GTSELF_MAXROW(g)                     (g)->pFuncTable->MaxRow(g)
#define HB_GTSELF_CHECKPOS(g,r,c,l)             (g)->pFuncTable->CheckPos(g,r,c,l)
#define HB_GTSELF_SETPOS(g,r,c)                 (g)->pFuncTable->SetPos(g,r,c)
#define HB_GTSELF_GETPOS(g,pr,pc)               (g)->pFuncTable->GetPos(g,pr,pc)
#define HB_GTSELF_ISCOLOR(g)                    (g)->pFuncTable->IsColor(g)
#define HB_GTSELF_GETCOLORSTR(g,s)              (g)->pFuncTable->GetColorStr(g,s)
#define HB_GTSELF_SETCOLORSTR(g,s)              (g)->pFuncTable->SetColorStr(g,s)
#define HB_GTSELF_COLORSELECT(g,c)              (g)->pFuncTable->ColorSelect(g,c)
#define HB_GTSELF_GETCOLOR(g)                   (g)->pFuncTable->GetColor(g)
#define HB_GTSELF_COLORNUM(g,s)                 (g)->pFuncTable->ColorNum(g,s)
#define HB_GTSELF_COLORSTOSTRING(g,pc,i,ps,n)   (g)->pFuncTable->ColorsToString(g,pc,i,ps,n)
#define HB_GTSELF_STRINGTOCOLORS(g,ps,pc,pi)    (g)->pFuncTable->StringToColors(g,ps,pc,pi)
#define HB_GTSELF_GETCOLORDATA(g,pc,pn,pi)      (g)->pFuncTable->GetColorData(g,pc,pn,pi)
#define HB_GTSELF_GETCLEARCOLOR(g)              (g)->pFuncTable->GetClearColor(g)
#define HB_GTSELF_SETCLEARCOLOR(g,c)            (g)->pFuncTable->SetClearColor(g,c)
#define HB_GTSELF_GETCLEARCHAR(g)               (g)->pFuncTable->GetClearChar(g)
#define HB_GTSELF_SETCLEARCHAR(g,c)             (g)->pFuncTable->SetClearChar(g,c)
#define HB_GTSELF_GETCURSORSTYLE(g)             (g)->pFuncTable->GetCursorStyle(g)
#define HB_GTSELF_SETCURSORSTYLE(g,s)           (g)->pFuncTable->SetCursorStyle(g,s)
#define HB_GTSELF_GETSCRCURSOR(g,pr,pc,ps)      (g)->pFuncTable->GetScrCursor(g,pr,pc,ps)
#define HB_GTSELF_GETSCRCHAR(g,r,c,pm,pa,pc)    (g)->pFuncTable->GetScrChar(g,r,c,pm,pa,pc)
#define HB_GTSELF_PUTSCRCHAR(g,r,c,m,a,u)       (g)->pFuncTable->PutScrChar(g,r,c,m,a,u)
#define HB_GTSELF_GETSCRUC(g,r,c,pm,pa,pc,f)    (g)->pFuncTable->GetScrUC(g,r,c,pm,pa,pc,f)
#define HB_GTSELF_DISPBEGIN(g)                  (g)->pFuncTable->DispBegin(g)
#define HB_GTSELF_DISPEND(g)                    (g)->pFuncTable->DispEnd(g)
#define HB_GTSELF_DISPCOUNT(g)                  (g)->pFuncTable->DispCount(g)
#define HB_GTSELF_GETCHAR(g,r,c,pm,pa,pc)       (g)->pFuncTable->GetChar(g,r,c,pm,pa,pc)
#define HB_GTSELF_PUTCHAR(g,r,c,m,a,u)          (g)->pFuncTable->PutChar(g,r,c,m,a,u)
#define HB_GTSELF_RECTSIZE(g,t,l,b,r)           (g)->pFuncTable->RectSize(g,t,l,b,r)
#define HB_GTSELF_SAVE(g,t,l,b,r,p)             (g)->pFuncTable->Save(g,t,l,b,r,p)
#define HB_GTSELF_REST(g,t,l,b,r,p)             (g)->pFuncTable->Rest(g,t,l,b,r,p)
#define HB_GTSELF_PUTTEXT(g,r,c,m,s,l)          (g)->pFuncTable->PutText(g,r,c,m,s,l)
#define HB_GTSELF_PUTTEXTW(g,r,c,m,s,l)         (g)->pFuncTable->PutTextW(g,r,c,m,s,l)
#define HB_GTSELF_REPLICATE(g,r,c,m,a,u,l)      (g)->pFuncTable->Replicate(g,r,c,m,a,u,l)
#define HB_GTSELF_WRITEAT(g,r,c,s,l)            (g)->pFuncTable->WriteAt(g,r,c,s,l)
#define HB_GTSELF_WRITEATW(g,r,c,s,l)           (g)->pFuncTable->WriteAtW(g,r,c,s,l)
#define HB_GTSELF_WRITE(g,s,l)                  (g)->pFuncTable->Write(g,s,l)
#define HB_GTSELF_WRITEW(g,s,l)                 (g)->pFuncTable->WriteW(g,s,l)
#define HB_GTSELF_WRITECON(g,s,l)               (g)->pFuncTable->WriteCon(g,s,l)
#define HB_GTSELF_WRITECONW(g,s,l)              (g)->pFuncTable->WriteConW(g,s,l)
#define HB_GTSELF_SETATTRIBUTE(g,t,l,b,r,m)     (g)->pFuncTable->SetAttribute(g,t,l,b,r,m)
#define HB_GTSELF_DRAWSHADOW(g,t,l,b,r,m)       (g)->pFuncTable->DrawShadow(g,t,l,b,r,m)
#define HB_GTSELF_SCROLL(g,t,l,b,r,m,u,v,h)     (g)->pFuncTable->Scroll(g,t,l,b,r,m,u,v,h)
#define HB_GTSELF_SCROLLUP(g,r,m,u)             (g)->pFuncTable->ScrollUp(g,r,m,u)
#define HB_GTSELF_BOX(g,t,l,b,r,f,m)            (g)->pFuncTable->Box(g,t,l,b,r,f,m)
#define HB_GTSELF_BOXW(g,t,l,b,r,f,m)           (g)->pFuncTable->BoxW(g,t,l,b,r,f,m)
#define HB_GTSELF_BOXD(g,t,l,b,r,f,m)           (g)->pFuncTable->BoxD(g,t,l,b,r,f,m)
#define HB_GTSELF_BOXS(g,t,l,b,r,f,m)           (g)->pFuncTable->BoxS(g,t,l,b,r,f,m)
#define HB_GTSELF_HORIZLINE(g,h,l,r,u,m)        (g)->pFuncTable->HorizLine(g,h,l,r,u,m)
#define HB_GTSELF_VERTLINE(g,c,t,b,u,m)         (g)->pFuncTable->VertLine(g,c,t,b,u,m)
#define HB_GTSELF_GETBLINK(g)                   (g)->pFuncTable->GetBlink(g)
#define HB_GTSELF_SETBLINK(g,b)                 (g)->pFuncTable->SetBlink(g,b)
#define HB_GTSELF_SETSNOWFLAG(g,b)              (g)->pFuncTable->SetSnowFlag(g,b)
#define HB_GTSELF_VERSION(g,i)                  (g)->pFuncTable->Version(g,i)
#define HB_GTSELF_SUSPEND(g)                    (g)->pFuncTable->Suspend(g)
#define HB_GTSELF_RESUME(g)                     (g)->pFuncTable->Resume(g)
#define HB_GTSELF_PREEXT(g)                     (g)->pFuncTable->PreExt(g)
#define HB_GTSELF_POSTEXT(g)                    (g)->pFuncTable->PostExt(g)
#define HB_GTSELF_OUTSTD(g,s,l)                 (g)->pFuncTable->OutStd(g,s,l)
#define HB_GTSELF_OUTERR(g,s,l)                 (g)->pFuncTable->OutErr(g,s,l)
#define HB_GTSELF_TONE(g,f,d)                   (g)->pFuncTable->Tone(g,f,d)
#define HB_GTSELF_BELL(g)                       (g)->pFuncTable->Bell(g)
#define HB_GTSELF_INFO(g,i,p)                   (g)->pFuncTable->Info(g,i,p)
#define HB_GTSELF_ALERT(g,m,o,n,h,d)            (g)->pFuncTable->Alert(g,m,o,n,h,d)
#define HB_GTSELF_SETFLAG(g,i,f)                (g)->pFuncTable->SetFlag(g,i,f)
#define HB_GTSELF_SETDISPCP(g,t,h,b)            (g)->pFuncTable->SetDispCP(g,t,h,b)
#define HB_GTSELF_SETKEYCP(g,t,h)               (g)->pFuncTable->SetKeyCP(g,t,h)
#define HB_GTSELF_READKEY(g,m)                  (g)->pFuncTable->ReadKey(g,m)
#define HB_GTSELF_INKEYGET(g,w,d,m)             (g)->pFuncTable->InkeyGet(g,w,d,m)
#define HB_GTSELF_INKEYPUT(g,k)                 (g)->pFuncTable->InkeyPut(g,k)
#define HB_GTSELF_INKEYINS(g,k)                 (g)->pFuncTable->InkeyIns(g,k)
#define HB_GTSELF_INKEYLAST(g,m)                (g)->pFuncTable->InkeyLast(g,m)
#define HB_GTSELF_INKEYNEXT(g,m)                (g)->pFuncTable->InkeyNext(g,m)
#define HB_GTSELF_INKEYPOLL(g)                  (g)->pFuncTable->InkeyPoll(g)
#define HB_GTSELF_INKEYSETTEXT(g,s,l)           (g)->pFuncTable->InkeySetText(g,s,l)
#define HB_GTSELF_INKEYSETLAST(g,k)             (g)->pFuncTable->InkeySetLast(g,k)
#define HB_GTSELF_INKEYRESET(g)                 (g)->pFuncTable->InkeyReset(g)
#define HB_GTSELF_INKEYEXIT(g)                  (g)->pFuncTable->InkeyExit(g)
#define HB_GTSELF_MOUSEINIT(g)                  (g)->pFuncTable->MouseInit(g)
#define HB_GTSELF_MOUSEEXIT(g)                  (g)->pFuncTable->MouseExit(g)
#define HB_GTSELF_MOUSEISPRESENT(g)             (g)->pFuncTable->MouseIsPresent(g)
#define HB_GTSELF_MOUSESHOW(g)                  (g)->pFuncTable->MouseShow(g)
#define HB_GTSELF_MOUSEHIDE(g)                  (g)->pFuncTable->MouseHide(g)
#define HB_GTSELF_MOUSEGETCURSOR(g)             (g)->pFuncTable->MouseGetCursor(g)
#define HB_GTSELF_MOUSESETCURSOR(g,v)           (g)->pFuncTable->MouseSetCursor(g,v)
#define HB_GTSELF_MOUSECOL(g)                   (g)->pFuncTable->MouseCol(g)
#define HB_GTSELF_MOUSEROW(g)                   (g)->pFuncTable->MouseRow(g)
#define HB_GTSELF_MOUSEGETPOS(g,pr,pc)          (g)->pFuncTable->MouseGetPos(g,pr,pc)
#define HB_GTSELF_MOUSESETPOS(g,r,c)            (g)->pFuncTable->MouseSetPos(g,r,c)
#define HB_GTSELF_MOUSESETBOUNDS(g,t,l,b,r)     (g)->pFuncTable->MouseSetBounds(g,t,l,b,r)
#define HB_GTSELF_MOUSEGETBOUNDS(g,t,l,b,r)     (g)->pFuncTable->MouseGetBounds(g,t,l,b,r)
#define HB_GTSELF_MOUSESTORAGESIZE(g)           (g)->pFuncTable->MouseStorageSize(g)
#define HB_GTSELF_MOUSESAVESTATE(g,p)           (g)->pFuncTable->MouseSaveState(g,p)
#define HB_GTSELF_MOUSERESTORESTATE(g,p)        (g)->pFuncTable->MouseRestoreState(g,p)
#define HB_GTSELF_MOUSEGETDOUBLECLICKSPEED(g)   (g)->pFuncTable->MouseGetDoubleClickSpeed(g)
#define HB_GTSELF_MOUSESETDOUBLECLICKSPEED(g,i) (g)->pFuncTable->MouseSetDoubleClickSpeed(g,i)
#define HB_GTSELF_MOUSECOUNTBUTTON(g)           (g)->pFuncTable->MouseCountButton(g)
#define HB_GTSELF_MOUSEBUTTONSTATE(g,b)         (g)->pFuncTable->MouseButtonState(g,b)
#define HB_GTSELF_MOUSEBUTTONPRESSED(g,b,r,c)   (g)->pFuncTable->MouseButtonPressed(g,b,r,c)
#define HB_GTSELF_MOUSEBUTTONRELEASED(g,b,r,c)  (g)->pFuncTable->MouseButtonReleased(g,b,r,c)
#define HB_GTSELF_MOUSEREADKEY(g,m)             (g)->pFuncTable->MouseReadKey(g,m)
#define HB_GTSELF_GFXPRIMITIVE(g,i,t,l,b,r,c)   (g)->pFuncTable->GfxPrimitive(g,i,t,l,b,r,c)
#define HB_GTSELF_GFXTEXT(g,t,l,s,c,h,w)        (g)->pFuncTable->GfxText(g,t,l,s,c,h,w)
#define HB_GTSELF_WHOCARES(g,p)                 (g)->pFuncTable->WhoCares(g,p)

#ifndef HB_GTSUPERTABLE
#  define HB_GTSUPERTABLE(g)  HB_GTSUPER
#endif

#define HB_GTSUPER_LOCK(g)                       (HB_GTSUPERTABLE(g))->Lock(g)
#define HB_GTSUPER_UNLOCK(g)                     (HB_GTSUPERTABLE(g))->Unlock(g)
#define HB_GTSUPER_INIT(g,i,o,e)                 (HB_GTSUPERTABLE(g))->Init(g,i,o,e)
#define HB_GTSUPER_EXIT(g)                       (HB_GTSUPERTABLE(g))->Exit(g)
#define HB_GTSUPER_NEW(g)                        (HB_GTSUPERTABLE(g))->New(g)
#define HB_GTSUPER_FREE(g)                       (HB_GTSUPERTABLE(g))->Free(g)
#define HB_GTSUPER_MARK(g)                       (HB_GTSUPERTABLE(g))->Mark(g)
#define HB_GTSUPER_RESIZE(g,r,c)                 (HB_GTSUPERTABLE(g))->Resize(g,r,c)
#define HB_GTSUPER_SETMODE(g,r,c)                (HB_GTSUPERTABLE(g))->SetMode(g,r,c)
#define HB_GTSUPER_GETSIZE(g,pr,pc)              (HB_GTSUPERTABLE(g))->GetSize(g,pr,pc)
#define HB_GTSUPER_SEMICOLD(g)                   (HB_GTSUPERTABLE(g))->SemiCold(g)
#define HB_GTSUPER_COLDAREA(g,t,l,b,r)           (HB_GTSUPERTABLE(g))->ColdArea(g,t,l,b,r)
#define HB_GTSUPER_EXPOSEAREA(g,t,l,b,r)         (HB_GTSUPERTABLE(g))->ExposeArea(g,t,l,b,r)
#define HB_GTSUPER_SCROLLAREA(g,t,l,b,r,m,u,v,h) (HB_GTSUPERTABLE(g))->ScrollArea(g,t,l,b,r,m,u,v,h)
#define HB_GTSUPER_TOUCHLINE(g,r)                (HB_GTSUPERTABLE(g))->TouchLine(g,r)
#define HB_GTSUPER_TOUCHCELL(g,r,c)              (HB_GTSUPERTABLE(g))->TouchCell(g,r,c)
#define HB_GTSUPER_REDRAW(g,r,c,l)               (HB_GTSUPERTABLE(g))->Redraw(g,r,c,l)
#define HB_GTSUPER_REDRAWDIFF(g)                 (HB_GTSUPERTABLE(g))->RedrawDiff(g)
#define HB_GTSUPER_REFRESH(g)                    (HB_GTSUPERTABLE(g))->Refresh(g)
#define HB_GTSUPER_FLUSH(g)                      (HB_GTSUPERTABLE(g))->Flush(g)
#define HB_GTSUPER_MAXCOL(g)                     (HB_GTSUPERTABLE(g))->MaxCol(g)
#define HB_GTSUPER_MAXROW(g)                     (HB_GTSUPERTABLE(g))->MaxRow(g)
#define HB_GTSUPER_CHECKPOS(g,r,c,l)             (HB_GTSUPERTABLE(g))->CheckPos(g,r,c,l)
#define HB_GTSUPER_SETPOS(g,r,c)                 (HB_GTSUPERTABLE(g))->SetPos(g,r,c)
#define HB_GTSUPER_GETPOS(g,pr,pc)               (HB_GTSUPERTABLE(g))->GetPos(g,pr,pc)
#define HB_GTSUPER_ISCOLOR(g)                    (HB_GTSUPERTABLE(g))->IsColor(g)
#define HB_GTSUPER_GETCOLORSTR(g,s)              (HB_GTSUPERTABLE(g))->GetColorStr(g,s)
#define HB_GTSUPER_SETCOLORSTR(g,s)              (HB_GTSUPERTABLE(g))->SetColorStr(g,s)
#define HB_GTSUPER_COLORSELECT(g,c)              (HB_GTSUPERTABLE(g))->ColorSelect(g,c)
#define HB_GTSUPER_GETCOLOR(g)                   (HB_GTSUPERTABLE(g))->GetColor(g)
#define HB_GTSUPER_COLORNUM(g,s)                 (HB_GTSUPERTABLE(g))->ColorNum(g,s)
#define HB_GTSUPER_COLORSTOSTRING(g,pc,i,ps,n)   (HB_GTSUPERTABLE(g))->ColorsToString(g,pc,i,ps,n)
#define HB_GTSUPER_STRINGTOCOLORS(g,ps,pc,pi)    (HB_GTSUPERTABLE(g))->StringToColors(g,ps,pc,pi)
#define HB_GTSUPER_GETCOLORDATA(g,pc,pn,pi)      (HB_GTSUPERTABLE(g))->GetColorData(g,pc,pn,pi)
#define HB_GTSUPER_GETCLEARCOLOR(g)              (HB_GTSUPERTABLE(g))->GetClearColor(g)
#define HB_GTSUPER_SETCLEARCOLOR(g,c)            (HB_GTSUPERTABLE(g))->SetClearColor(g,c)
#define HB_GTSUPER_GETCLEARCHAR(g)               (HB_GTSUPERTABLE(g))->GetClearChar(g)
#define HB_GTSUPER_SETCLEARCHAR(g,c)             (HB_GTSUPERTABLE(g))->SetClearChar(g,c)
#define HB_GTSUPER_GETCURSORSTYLE(g)             (HB_GTSUPERTABLE(g))->GetCursorStyle(g)
#define HB_GTSUPER_SETCURSORSTYLE(g,s)           (HB_GTSUPERTABLE(g))->SetCursorStyle(g,s)
#define HB_GTSUPER_GETSCRCURSOR(g,pr,pc,ps)      (HB_GTSUPERTABLE(g))->GetScrCursor(g,pr,pc,ps)
#define HB_GTSUPER_GETSCRCHAR(g,r,c,pm,pa,pc)    (HB_GTSUPERTABLE(g))->GetScrChar(g,r,c,pm,pa,pc)
#define HB_GTSUPER_PUTSCRCHAR(g,r,c,m,a,u)       (HB_GTSUPERTABLE(g))->PutScrChar(g,r,c,m,a,u)
#define HB_GTSUPER_GETSCRUC(g,r,c,pm,pa,pc,f)    (HB_GTSUPERTABLE(g))->GetScrUC(g,r,c,pm,pa,pc,f)
#define HB_GTSUPER_DISPBEGIN(g)                  (HB_GTSUPERTABLE(g))->DispBegin(g)
#define HB_GTSUPER_DISPEND(g)                    (HB_GTSUPERTABLE(g))->DispEnd(g)
#define HB_GTSUPER_DISPCOUNT(g)                  (HB_GTSUPERTABLE(g))->DispCount(g)
#define HB_GTSUPER_GETCHAR(g,r,c,pm,pa,pc)       (HB_GTSUPERTABLE(g))->GetChar(g,r,c,pm,pa,pc)
#define HB_GTSUPER_PUTCHAR(g,r,c,m,a,u)          (HB_GTSUPERTABLE(g))->PutChar(g,r,c,m,a,u)
#define HB_GTSUPER_RECTSIZE(g,t,l,b,r)           (HB_GTSUPERTABLE(g))->RectSize(g,t,l,b,r)
#define HB_GTSUPER_SAVE(g,t,l,b,r,p)             (HB_GTSUPERTABLE(g))->Save(g,t,l,b,r,p)
#define HB_GTSUPER_REST(g,t,l,b,r,p)             (HB_GTSUPERTABLE(g))->Rest(g,t,l,b,r,p)
#define HB_GTSUPER_PUTTEXT(g,r,c,m,s,l)          (HB_GTSUPERTABLE(g))->PutText(g,r,c,m,s,l)
#define HB_GTSUPER_PUTTEXTW(g,r,c,m,s,l)         (HB_GTSUPERTABLE(g))->PutTextW(g,r,c,m,s,l)
#define HB_GTSUPER_REPLICATE(g,r,c,m,a,u,l)      (HB_GTSUPERTABLE(g))->Replicate(g,r,c,m,a,u,l)
#define HB_GTSUPER_WRITEAT(g,r,c,s,l)            (HB_GTSUPERTABLE(g))->WriteAt(g,r,c,s,l)
#define HB_GTSUPER_WRITEATW(g,r,c,s,l)           (HB_GTSUPERTABLE(g))->WriteAtW(g,r,c,s,l)
#define HB_GTSUPER_WRITE(g,s,l)                  (HB_GTSUPERTABLE(g))->Write(g,s,l)
#define HB_GTSUPER_WRITEW(g,s,l)                 (HB_GTSUPERTABLE(g))->WriteW(g,s,l)
#define HB_GTSUPER_WRITECON(g,s,l)               (HB_GTSUPERTABLE(g))->WriteCon(g,s,l)
#define HB_GTSUPER_WRITECONW(g,s,l)              (HB_GTSUPERTABLE(g))->WriteConW(g,s,l)
#define HB_GTSUPER_SETATTRIBUTE(g,t,l,b,r,m)     (HB_GTSUPERTABLE(g))->SetAttribute(g,t,l,b,r,m)
#define HB_GTSUPER_DRAWSHADOW(g,t,l,b,r,m)       (HB_GTSUPERTABLE(g))->DrawShadow(g,t,l,b,r,m)
#define HB_GTSUPER_SCROLL(g,t,l,b,r,m,u,v,h)     (HB_GTSUPERTABLE(g))->Scroll(g,t,l,b,r,m,u,v,h)
#define HB_GTSUPER_SCROLLUP(g,r,m,u)             (HB_GTSUPERTABLE(g))->ScrollUp(g,r,m,u)
#define HB_GTSUPER_BOX(g,t,l,b,r,f,m)            (HB_GTSUPERTABLE(g))->Box(g,t,l,b,r,f,m)
#define HB_GTSUPER_BOXW(g,t,l,b,r,f,m)           (HB_GTSUPERTABLE(g))->BoxW(g,t,l,b,r,f,m)
#define HB_GTSUPER_BOXD(g,t,l,b,r,f,m)           (HB_GTSUPERTABLE(g))->BoxD(g,t,l,b,r,f,m)
#define HB_GTSUPER_BOXS(g,t,l,b,r,f,m)           (HB_GTSUPERTABLE(g))->BoxS(g,t,l,b,r,f,m)
#define HB_GTSUPER_HORIZLINE(g,h,l,r,u,m)        (HB_GTSUPERTABLE(g))->HorizLine(g,h,l,r,u,m)
#define HB_GTSUPER_VERTLINE(g,c,t,b,u,m)         (HB_GTSUPERTABLE(g))->VertLine(g,c,t,b,u,m)
#define HB_GTSUPER_GETBLINK(g)                   (HB_GTSUPERTABLE(g))->GetBlink(g)
#define HB_GTSUPER_SETBLINK(g,b)                 (HB_GTSUPERTABLE(g))->SetBlink(g,b)
#define HB_GTSUPER_SETSNOWFLAG(g,b)              (HB_GTSUPERTABLE(g))->SetSnowFlag(g,b)
#define HB_GTSUPER_VERSION(g,i)                  (HB_GTSUPERTABLE(g))->Version(g,i)
#define HB_GTSUPER_SUSPEND(g)                    (HB_GTSUPERTABLE(g))->Suspend(g)
#define HB_GTSUPER_RESUME(g)                     (HB_GTSUPERTABLE(g))->Resume(g)
#define HB_GTSUPER_PREEXT(g)                     (HB_GTSUPERTABLE(g))->PreExt(g)
#define HB_GTSUPER_POSTEXT(g)                    (HB_GTSUPERTABLE(g))->PostExt(g)
#define HB_GTSUPER_OUTSTD(g,s,l)                 (HB_GTSUPERTABLE(g))->OutStd(g,s,l)
#define HB_GTSUPER_OUTERR(g,s,l)                 (HB_GTSUPERTABLE(g))->OutErr(g,s,l)
#define HB_GTSUPER_TONE(g,f,d)                   (HB_GTSUPERTABLE(g))->Tone(g,f,d)
#define HB_GTSUPER_BELL(g)                       (HB_GTSUPERTABLE(g))->Bell(g)
#define HB_GTSUPER_INFO(g,i,p)                   (HB_GTSUPERTABLE(g))->Info(g,i,p)
#define HB_GTSUPER_ALERT(g,m,o,n,h,d)            (HB_GTSUPERTABLE(g))->Alert(g,m,o,n,h,d)
#define HB_GTSUPER_SETFLAG(g,i,f)                (HB_GTSUPERTABLE(g))->SetFlag(g,i,f)
#define HB_GTSUPER_SETDISPCP(g,t,h,b)            (HB_GTSUPERTABLE(g))->SetDispCP(g,t,h,b)
#define HB_GTSUPER_SETKEYCP(g,t,h)               (HB_GTSUPERTABLE(g))->SetKeyCP(g,t,h)
#define HB_GTSUPER_READKEY(g,m)                  (HB_GTSUPERTABLE(g))->ReadKey(g,m)
#define HB_GTSUPER_INKEYGET(g,w,d,m)             (HB_GTSUPERTABLE(g))->InkeyGet(g,w,d,m)
#define HB_GTSUPER_INKEYPUT(g,k)                 (HB_GTSUPERTABLE(g))->InkeyPut(g,k)
#define HB_GTSUPER_INKEYINS(g,k)                 (HB_GTSUPERTABLE(g))->InkeyIns(g,k)
#define HB_GTSUPER_INKEYLAST(g,m)                (HB_GTSUPERTABLE(g))->InkeyLast(g,m)
#define HB_GTSUPER_INKEYNEXT(g,m)                (HB_GTSUPERTABLE(g))->InkeyNext(g,m)
#define HB_GTSUPER_INKEYPOLL(g)                  (HB_GTSUPERTABLE(g))->InkeyPoll(g)
#define HB_GTSUPER_INKEYSETTEXT(g,s,l)           (HB_GTSUPERTABLE(g))->InkeySetText(g,s,l)
#define HB_GTSUPER_INKEYSETLAST(g,k)             (HB_GTSUPERTABLE(g))->InkeySetLast(g,k)
#define HB_GTSUPER_INKEYRESET(g)                 (HB_GTSUPERTABLE(g))->InkeyReset(g)
#define HB_GTSUPER_INKEYEXIT(g)                  (HB_GTSUPERTABLE(g))->InkeyExit(g)
#define HB_GTSUPER_MOUSEINIT(g)                  (HB_GTSUPERTABLE(g))->MouseInit(g)
#define HB_GTSUPER_MOUSEEXIT(g)                  (HB_GTSUPERTABLE(g))->MouseExit(g)
#define HB_GTSUPER_MOUSEISPRESENT(g)             (HB_GTSUPERTABLE(g))->MouseIsPresent(g)
#define HB_GTSUPER_MOUSESHOW(g)                  (HB_GTSUPERTABLE(g))->MouseShow(g)
#define HB_GTSUPER_MOUSEHIDE(g)                  (HB_GTSUPERTABLE(g))->MouseHide(g)
#define HB_GTSUPER_MOUSEGETCURSOR(g)             (HB_GTSUPERTABLE(g))->MouseGetCursor(g)
#define HB_GTSUPER_MOUSESETCURSOR(g,v)           (HB_GTSUPERTABLE(g))->MouseSetCursor(g,v)
#define HB_GTSUPER_MOUSECOL(g)                   (HB_GTSUPERTABLE(g))->MouseCol(g)
#define HB_GTSUPER_MOUSEROW(g)                   (HB_GTSUPERTABLE(g))->MouseRow(g)
#define HB_GTSUPER_MOUSEGETPOS(g,pr,pc)          (HB_GTSUPERTABLE(g))->MouseGetPos(g,pr,pc)
#define HB_GTSUPER_MOUSESETPOS(g,r,c)            (HB_GTSUPERTABLE(g))->MouseSetPos(g,r,c)
#define HB_GTSUPER_MOUSESETBOUNDS(g,t,l,b,r)     (HB_GTSUPERTABLE(g))->MouseSetBounds(g,t,l,b,r)
#define HB_GTSUPER_MOUSEGETBOUNDS(g,t,l,b,r)     (HB_GTSUPERTABLE(g))->MouseGetBounds(g,t,l,b,r)
#define HB_GTSUPER_MOUSESTORAGESIZE(g)           (HB_GTSUPERTABLE(g))->MouseStorageSize(g)
#define HB_GTSUPER_MOUSESAVESTATE(g,p)           (HB_GTSUPERTABLE(g))->MouseSaveState(g,p)
#define HB_GTSUPER_MOUSERESTORESTATE(g,p)        (HB_GTSUPERTABLE(g))->MouseRestoreState(g,p)
#define HB_GTSUPER_MOUSEGETDOUBLECLICKSPEED(g)   (HB_GTSUPERTABLE(g))->MouseGetDoubleClickSpeed(g)
#define HB_GTSUPER_MOUSESETDOUBLECLICKSPEED(g,i) (HB_GTSUPERTABLE(g))->MouseSetDoubleClickSpeed(g,i)
#define HB_GTSUPER_MOUSECOUNTBUTTON(g)           (HB_GTSUPERTABLE(g))->MouseCountButton(g)
#define HB_GTSUPER_MOUSEBUTTONSTATE(g,b)         (HB_GTSUPERTABLE(g))->MouseButtonState(g,b)
#define HB_GTSUPER_MOUSEBUTTONPRESSED(g,b,r,c)   (HB_GTSUPERTABLE(g))->MouseButtonPressed(g,b,r,c)
#define HB_GTSUPER_MOUSEBUTTONRELEASED(g,b,r,c)  (HB_GTSUPERTABLE(g))->MouseButtonReleased(g,b,r,c)
#define HB_GTSUPER_MOUSEREADKEY(g,m)             (HB_GTSUPERTABLE(g))->MouseReadKey(g,m)
#define HB_GTSUPER_GFXPRIMITIVE(g,i,t,l,b,r,c)   (HB_GTSUPERTABLE(g))->GfxPrimitive(g,i,t,l,b,r,c)
#define HB_GTSUPER_GFXTEXT(g,t,l,s,c,h,w)        (HB_GTSUPERTABLE(g))->GfxText(g,t,l,s,c,h,w)
#define HB_GTSUPER_WHOCARES(g,p)                 (HB_GTSUPERTABLE(g))->WhoCares(g,p)

extern HB_EXPORT HB_BOOL hb_gtRegister( const HB_GT_INIT * gtInit );
extern HB_EXPORT PHB_GT  hb_gtLoad( const char * szGtName, PHB_GT pGT, PHB_GT_FUNCS pSuperTable );

/* low level GT functions common to different GTs supported by RTL */
extern int  hb_gt_chrmapinit( int * piTransTbl, const char * pszTerm, HB_BOOL fSetACSC );
extern HB_BOOL hb_gt_setClipboard( const char * szClipData, HB_SIZE nLen );
extern HB_BOOL hb_gt_getClipboard( char ** pszClipData, HB_SIZE * pnLen );
#if defined( HB_OS_WIN )
extern HB_EXPORT HB_BOOL hb_gt_winapi_setClipboard( HB_UINT uFormat, PHB_ITEM pItem );
extern HB_EXPORT HB_BOOL hb_gt_winapi_setClipboardRaw( HB_UINT uFormat, void * pData, HB_SIZE nSize );
extern HB_EXPORT HB_BOOL hb_gt_winapi_getClipboard( HB_UINT uFormat, PHB_ITEM pItem );
extern HB_EXPORT int     hb_gt_winapi_getKbdState( void );
extern HB_EXPORT void    hb_gt_winapi_setKbdState( int kbdShifts );
extern HB_EXPORT void    hb_gt_winapi_tone( double dFrequency, double dDuration );
#endif /* HB_OS_WIN */
#if defined( HB_OS_DOS ) || defined( HB_OS_WIN ) || defined( HB_OS_OS2 )
extern int hb_gt_dos_keyCodeTranslate( int iKey );
#endif /* HB_OS_DOS || HB_OS_WIN || HB_OS_OS2 */

/* macros to manipulate Harbour extended key codes */
#define HB_INKEY_EXT_MASK           0xF8000000
#define HB_INKEY_EXT_BIT            0x40000000
#define HB_INKEY_EXT_TYPEMASK       0xFF000000
#define HB_INKEY_EXT_VALBITS        16
#define HB_INKEY_EXT_VALMASK        ( ( 1 << HB_INKEY_EXT_VALBITS ) - 1 )
#define HB_INKEY_EXT_FLAGMASK       ( 0xFF << HB_INKEY_EXT_VALBITS )
#define HB_INKEY_EXT_KEY            0x01000000
#define HB_INKEY_EXT_CHAR           0x02000000
#define HB_INKEY_EXT_UNICODE        0x03000000
#define HB_INKEY_EXT_MOUSEKEY       0x04000000
#define HB_INKEY_EXT_MOUSEPOS       0x05000000
#define HB_INKEY_EXT_EVENT          0x06000000
#define HB_INKEY_EXT_POSBITS        12
#define HB_INKEY_EXT_POSMASK        ( ( 1 << HB_INKEY_EXT_POSBITS ) - 1 )

#define HB_INKEY_ISEXT( n )         ( ( ( n ) & HB_INKEY_EXT_MASK ) == HB_INKEY_EXT_BIT )
#define HB_INKEY_TYPE( n )          ( ( ( n ) ^ HB_INKEY_EXT_BIT ) & HB_INKEY_EXT_TYPEMASK )
#define HB_INKEY_ISKEY( n )         ( HB_INKEY_TYPE( n ) == HB_INKEY_EXT_KEY )
#define HB_INKEY_ISCHAR( n )        ( HB_INKEY_TYPE( n ) == HB_INKEY_EXT_CHAR )
#define HB_INKEY_ISUNICODE( n )     ( HB_INKEY_TYPE( n ) == HB_INKEY_EXT_UNICODE )
#define HB_INKEY_ISMOUSEKEY( n )    ( HB_INKEY_TYPE( n ) == HB_INKEY_EXT_MOUSEKEY )
#define HB_INKEY_ISMOUSEPOS( n )    ( HB_INKEY_TYPE( n ) == HB_INKEY_EXT_MOUSEPOS )
#define HB_INKEY_ISEVENT( n )       ( HB_INKEY_TYPE( n ) == HB_INKEY_EXT_EVENT )

#define HB_INKEY_NEW_VALF( v, f )   ( ( ( v ) & HB_INKEY_EXT_VALMASK ) | \
                                      ( ( ( f ) << HB_INKEY_EXT_VALBITS ) & HB_INKEY_EXT_FLAGMASK ) )

#define HB_INKEY_NEW_MKEY( b, f )   ( HB_INKEY_NEW_VALF( b, f ) | HB_INKEY_EXT_BIT | HB_INKEY_EXT_MOUSEKEY )
#define HB_INKEY_NEW_KEY( k, f )    ( HB_INKEY_NEW_VALF( k, f ) | HB_INKEY_EXT_BIT | HB_INKEY_EXT_KEY )
#define HB_INKEY_NEW_CHAR( b )      ( ( b ) | ( HB_INKEY_EXT_BIT | HB_INKEY_EXT_CHAR ) )
#define HB_INKEY_NEW_UNICODE( b )   ( ( b ) | ( HB_INKEY_EXT_BIT | HB_INKEY_EXT_UNICODE ) )

#define HB_INKEY_NEW_MPOS( x, y )   ( ( ( ( y ) & HB_INKEY_EXT_POSMASK ) << HB_INKEY_EXT_POSBITS ) | \
                                      ( ( x ) & HB_INKEY_EXT_POSMASK ) | \
                                      ( HB_INKEY_EXT_BIT | HB_INKEY_EXT_MOUSEPOS ) )

#define HB_INKEY_MOUSEPOSX( n )     ( ( n ) & HB_INKEY_EXT_POSMASK )
#define HB_INKEY_MOUSEPOSY( n )     ( ( ( n ) >> HB_INKEY_EXT_POSBITS ) & HB_INKEY_EXT_POSMASK )

#define HB_INKEY_VALUE( n )         ( ( n ) & HB_INKEY_EXT_VALMASK )
#define HB_INKEY_FLAGS( n )         ( ( ( n ) & HB_INKEY_EXT_FLAGMASK ) >> HB_INKEY_EXT_VALBITS )


HB_EXTERN_END

#endif /* HB_GTCORE_H_ */
