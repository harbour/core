/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Header file for the Internal Terminal API
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
 * www - http://www.harbour-project.org
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

/* NOTE: The declaration of hb_gtSetPos(), hb_gtGetPos(), hb_gtWrite(),
         hb_gtWriteAt(), hb_gtRepChar(), hb_gtBox(), hb_gtBoxS(), hb_gtBoxD()
         hb_gtInit() differs in parameter types from the original CA-Cl*pper
         versions. [vszakats] */

#ifndef HB_GTCORE_H_
#define HB_GTCORE_H_

#include "hbapigt.h"
#include "hbapicdp.h"

HB_EXTERN_BEGIN

/* extended attributes used by core screen buffer */
#define HB_GT_ATTR_BOX        0x01
#define HB_GT_ATTR_SHADOW     0x02
#define HB_GT_ATTR_UNDEF      0x40
#define HB_GT_ATTR_REFRESH    0x80

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
   BOOL     (* Lock) ( HB_GT_PTR );
   void     (* Unlock) ( HB_GT_PTR );
   void     (* Init) ( HB_GT_PTR, HB_FHANDLE, HB_FHANDLE, HB_FHANDLE );
   void     (* Exit) ( HB_GT_PTR );
   void *   (* New) ( HB_GT_PTR );
   void     (* Free) ( HB_GT_PTR );
   BOOL     (* Resize ) ( HB_GT_PTR, int, int );
   BOOL     (* SetMode) ( HB_GT_PTR, int, int );
   void     (* GetSize) ( HB_GT_PTR, int *, int * );
   void     (* SemiCold) ( HB_GT_PTR );
   void     (* ColdArea) ( HB_GT_PTR, int, int, int, int );
   void     (* ExposeArea) ( HB_GT_PTR, int, int, int, int );
   void     (* ScrollArea) ( HB_GT_PTR, int, int, int, int, BYTE, BYTE, int, int );
   void     (* TouchCell) ( HB_GT_PTR, int, int );
   void     (* Redraw) ( HB_GT_PTR, int, int, int );
   void     (* Refresh) ( HB_GT_PTR );
   void     (* Flush) ( HB_GT_PTR );
   int      (* MaxCol) ( HB_GT_PTR );
   int      (* MaxRow) ( HB_GT_PTR );
   BOOL     (* CheckPos) ( HB_GT_PTR, int, int, long * );
   void     (* SetPos) ( HB_GT_PTR, int, int );
   void     (* GetPos) ( HB_GT_PTR, int *, int * );
   BOOL     (* IsColor) ( HB_GT_PTR );
   void     (* GetColorStr) ( HB_GT_PTR, char * );
   void     (* SetColorStr) ( HB_GT_PTR, const char * );
   void     (* ColorSelect) ( HB_GT_PTR, int );
   int      (* GetColor) ( HB_GT_PTR );
   int      (* ColorNum) ( HB_GT_PTR, const char * );
   void     (* ColorsToString) ( HB_GT_PTR, int *, int, char *, int );
   void     (* StringToColors) ( HB_GT_PTR, const char *, int **, int * );
   void     (* GetColorData) ( HB_GT_PTR, int **, int *, int * );
   int      (* GetClearColor) ( HB_GT_PTR );
   void     (* SetClearColor) ( HB_GT_PTR, int );
   int      (* GetClearChar) ( HB_GT_PTR );
   void     (* SetClearChar) ( HB_GT_PTR, int );
   int      (* GetCursorStyle) ( HB_GT_PTR );
   void     (* SetCursorStyle) ( HB_GT_PTR, int );
   void     (* GetScrCursor) ( HB_GT_PTR, int *, int *, int * );
   BOOL     (* GetScrChar) ( HB_GT_PTR, int, int, BYTE *, BYTE *, USHORT * );
   BOOL     (* PutScrChar) ( HB_GT_PTR, int, int, BYTE, BYTE, USHORT );
   void     (* DispBegin) ( HB_GT_PTR );
   void     (* DispEnd) ( HB_GT_PTR );
   int      (* DispCount) ( HB_GT_PTR );
   BOOL     (* GetChar) ( HB_GT_PTR, int, int, BYTE *, BYTE *, USHORT * );
   BOOL     (* PutChar) ( HB_GT_PTR, int, int, BYTE, BYTE, USHORT );
   long     (* RectSize) ( HB_GT_PTR, int, int, int, int );
   void     (* Save) ( HB_GT_PTR, int, int, int, int, BYTE * );
   void     (* Rest) ( HB_GT_PTR, int, int, int, int, BYTE * );
   void     (* PutText) ( HB_GT_PTR, int, int, BYTE, BYTE *, ULONG );
   void     (* Replicate) ( HB_GT_PTR, int, int, BYTE, BYTE, USHORT, ULONG );
   void     (* WriteAt) ( HB_GT_PTR, int, int, BYTE *, ULONG );
   void     (* Write) ( HB_GT_PTR, BYTE *, ULONG );
   void     (* WriteCon) ( HB_GT_PTR, BYTE *, ULONG );
   void     (* SetAttribute) ( HB_GT_PTR, int, int, int, int, BYTE );
   void     (* DrawShadow) ( HB_GT_PTR, int, int, int, int, BYTE );
   void     (* Scroll) ( HB_GT_PTR, int, int, int, int, BYTE, BYTE, int, int );
   void     (* ScrollUp) ( HB_GT_PTR, int, BYTE, BYTE );
   void     (* Box) ( HB_GT_PTR, int, int, int, int, BYTE *, BYTE );
   void     (* BoxD) ( HB_GT_PTR, int, int, int, int, BYTE *, BYTE );
   void     (* BoxS) ( HB_GT_PTR, int, int, int, int, BYTE *, BYTE );
   void     (* HorizLine) ( HB_GT_PTR, int, int, int, BYTE, BYTE );
   void     (* VertLine) ( HB_GT_PTR, int, int, int, BYTE, BYTE );
   BOOL     (* GetBlink) ( HB_GT_PTR );
   void     (* SetBlink) ( HB_GT_PTR, BOOL );
   void     (* SetSnowFlag) ( HB_GT_PTR, BOOL );
   const char * (* Version) ( HB_GT_PTR, int );
   BOOL     (* Suspend) ( HB_GT_PTR );
   BOOL     (* Resume) ( HB_GT_PTR );
   BOOL     (* PreExt) ( HB_GT_PTR );
   BOOL     (* PostExt) ( HB_GT_PTR );
   void     (* OutStd) ( HB_GT_PTR, BYTE *, ULONG );
   void     (* OutErr) ( HB_GT_PTR, BYTE *, ULONG );
   void     (* Tone) ( HB_GT_PTR, double, double );
   void     (* Bell) ( HB_GT_PTR );
   BOOL     (* Info) ( HB_GT_PTR, int, PHB_GT_INFO );
   int      (* Alert) ( HB_GT_PTR, PHB_ITEM, PHB_ITEM, int, int, double );
   int      (* SetFlag) ( HB_GT_PTR, int, int );

   /* internationalization */
   BOOL     (* SetDispCP) ( HB_GT_PTR, char *, char *, BOOL );
   BOOL     (* SetKeyCP) ( HB_GT_PTR, char *, char * );

   /* keyboard */
   int      (* ReadKey) ( HB_GT_PTR, int );

   int      (* InkeyGet) ( HB_GT_PTR, BOOL fWait, double dSeconds, int iEventMask );
   void     (* InkeyPut) ( HB_GT_PTR, int iKey );
   void     (* InkeyIns) ( HB_GT_PTR, int iKey );
   int      (* InkeyLast) ( HB_GT_PTR, int iEventMask );
   int      (* InkeyNext) ( HB_GT_PTR, int iEventMask );
   void     (* InkeyPoll) ( HB_GT_PTR );
   void     (* InkeySetText) ( HB_GT_PTR, const char * szText, ULONG ulLen );
   int      (* InkeySetLast) ( HB_GT_PTR, int iKey );
   void     (* InkeyReset) ( HB_GT_PTR );
   void     (* InkeyExit) ( HB_GT_PTR );

   /* mouse */
   void     (* MouseInit) ( HB_GT_PTR );
   void     (* MouseExit) ( HB_GT_PTR );
   BOOL     (* MouseIsPresent) ( HB_GT_PTR );
   void     (* MouseShow) ( HB_GT_PTR );
   void     (* MouseHide) ( HB_GT_PTR );
   BOOL     (* MouseGetCursor) ( HB_GT_PTR );
   void     (* MouseSetCursor) ( HB_GT_PTR, BOOL );
   int      (* MouseCol) ( HB_GT_PTR );
   int      (* MouseRow) ( HB_GT_PTR );
   void     (* MouseGetPos) ( HB_GT_PTR, int *, int * );
   void     (* MouseSetPos) ( HB_GT_PTR, int, int );
   void     (* MouseSetBounds) ( HB_GT_PTR, int, int, int, int );
   void     (* MouseGetBounds) ( HB_GT_PTR, int *, int *, int *, int * );
   int      (* MouseStorageSize) ( HB_GT_PTR );
   void     (* MouseSaveState) ( HB_GT_PTR, BYTE * );
   void     (* MouseRestoreState) ( HB_GT_PTR, BYTE * );
   int      (* MouseGetDoubleClickSpeed) ( HB_GT_PTR );
   void     (* MouseSetDoubleClickSpeed) ( HB_GT_PTR, int );
   int      (* MouseCountButton) ( HB_GT_PTR );
   BOOL     (* MouseButtonState) ( HB_GT_PTR, int );
   BOOL     (* MouseButtonPressed) ( HB_GT_PTR, int, int *, int * );
   BOOL     (* MouseButtonReleased) ( HB_GT_PTR, int, int *, int * );
   int      (* MouseReadKey) ( HB_GT_PTR, int );

   /* Graphics API */
   int      (* GfxPrimitive) ( HB_GT_PTR, int, int, int, int, int, int );
   void     (* GfxText) ( HB_GT_PTR, int, int, char *, int, int, int );

#if 0
   /* keyboard */
   int     (* ExtendedKeySupport) ( HB_GT_PTR );

   /* GT CLIPBOARD functions */
   void    (* GetClipboard) ( HB_GT_PTR, char *, ULONG * );
   void    (* SetClipboard) ( HB_GT_PTR, char *, ULONG );
   ULONG   (* GetClipboardSize) ( HB_GT_PTR );

   void    (* ProcessMessages) ( HB_GT_PTR );

   /* GT to DRIVER communication functions */
   void    (* update ) ( HB_GT_PTR, int );
   int     (* info ) ( HB_GT_PTR, int, BOOL , int , void * );

#endif

   void    (* WhoCares) ( HB_GT_PTR, void * );

} HB_GT_FUNCS, * PHB_GT_FUNCS;

typedef int ( * GTENTRYP_V )( void );

#define GTFUNCSCOUNT   ( sizeof( HB_GT_FUNCS ) / sizeof( GTENTRYP_V ) )

#define HB_GT_MAX_      32
#define HB_GT_NAME_MAX_ 8

typedef struct _HB_GT_INIT
{
   const char     * id;
   BOOL           (* init) ( PHB_GT_FUNCS );
   PHB_GT_FUNCS   pSuperTable;
   int *          pGtId;
} HB_GT_INIT, * PHB_GT_INIT;


typedef union
{
   struct
   {
      UINT16   usChar;
      BYTE     bColor;
      BYTE     bAttr;
   } c;
   UINT32   uiValue;
} HB_SCREENCELL;
typedef HB_SCREENCELL * PHB_SCREENCELL;


typedef struct _HB_GT_BASE
{
   PHB_GT_FUNCS   pFuncTable;

   PHB_ITEM       pMutex;

   int            iRow;             /* cursor row position */
   int            iCol;             /* cursor column position */

   int            iHeight;          /* window height */
   int            iWidth;           /* window width */

   PHB_SCREENCELL screenBuffer;     /* window foreground (board) current buffer */
   PHB_SCREENCELL prevBuffer;       /* window foreground (board) previous buffer */

   BOOL *         pLines;           /* Touched Window lines */
   BOOL           fRefresh;         /* Should Window be refreshed */

   BOOL           fVgaCell;
   BOOL           fIsColor;
   BOOL           fBlinking;
   BOOL           fStdOutCon;
   BOOL           fStdErrCon;
   int            iCursorShape;
   USHORT         uiDispCount;
   USHORT         uiExtCount;
   USHORT         uiClearChar;
   BYTE           bClearColor;
   HB_FHANDLE     hStdIn;
   HB_FHANDLE     hStdOut;
   HB_FHANDLE     hStdErr;

   BOOL           fDispTrans;
   PHB_CODEPAGE   cdpTerm;
   PHB_CODEPAGE   cdpHost;

   int            iColorIndex;
   int            iColorCount;
   int *          pColor;

   int            iDoubleClickSpeed; /* In milliseconds */
   BOOL           fMouseVisible;
   int            iMouseLastRow;
   int            iMouseLastCol;
   HB_LONG        iMouseLeftTimer;
   HB_LONG        iMouseRightTimer;
   HB_LONG        iMouseMiddleTimer;

   int            defaultKeyBuffer[ HB_DEFAULT_INKEY_BUFSIZE + 1 ];

   int *          inkeyBuffer;
   int            inkeyBufferSize;
   int            inkeyHead;
   int            inkeyTail;
   int            iLastPut;
   int            inkeyLast;
   BYTE *         StrBuffer;
   ULONG          StrBufferSize;
   ULONG          StrBufferPos;

   PHB_ITEM       pNotifierBlock;

   void *         pGTData[HB_GT_MAX_];    /* local GT data */

} HB_GT_BASE, * PHB_GT_BASE, * PHB_GT;

extern PHB_GT hb_gt_Base( void );
extern void hb_gt_BaseFree( PHB_GT pGT );

#define HB_GTLOCAL(g)   (g)->pGTData[*HB_GTID_PTR]

#define HB_GTSELF_LOCK(g)                       (g)->pFuncTable->Lock(g)
#define HB_GTSELF_UNLOCK(g)                     (g)->pFuncTable->Unlock(g)
#define HB_GTSELF_INIT(g,i,o,e)                 (g)->pFuncTable->Init(g,i,o,e)
#define HB_GTSELF_EXIT(g)                       (g)->pFuncTable->Exit(g)
#define HB_GTSELF_NEW(g)                        (g)->pFuncTable->New(g)
#define HB_GTSELF_FREE(g)                       (g)->pFuncTable->Free(g)
#define HB_GTSELF_RESIZE(g,r,c)                 (g)->pFuncTable->Resize(g,r,c)
#define HB_GTSELF_SETMODE(g,r,c)                (g)->pFuncTable->SetMode(g,r,c)
#define HB_GTSELF_GETSIZE(g,pr,pc)              (g)->pFuncTable->GetSize(g,pr,pc)
#define HB_GTSELF_SEMICOLD(g)                   (g)->pFuncTable->SemiCold(g)
#define HB_GTSELF_COLDAREA(g,t,l,b,r)           (g)->pFuncTable->ColdArea(g,t,l,b,r)
#define HB_GTSELF_EXPOSEAREA(g,t,l,b,r)         (g)->pFuncTable->ExposeArea(g,t,l,b,r)
#define HB_GTSELF_SCROLLAREA(g,t,l,b,r,m,u,v,h) (g)->pFuncTable->ScrollArea(g,t,l,b,r,m,u,v,h)
#define HB_GTSELF_TOUCHCELL(g,r,c)              (g)->pFuncTable->TouchCell(g,r,c)
#define HB_GTSELF_REDRAW(g,r,c,l)               (g)->pFuncTable->Redraw(g,r,c,l)
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
#define HB_GTSELF_DISPBEGIN(g)                  (g)->pFuncTable->DispBegin(g)
#define HB_GTSELF_DISPEND(g)                    (g)->pFuncTable->DispEnd(g)
#define HB_GTSELF_DISPCOUNT(g)                  (g)->pFuncTable->DispCount(g)
#define HB_GTSELF_GETCHAR(g,r,c,pm,pa,pc)       (g)->pFuncTable->GetChar(g,r,c,pm,pa,pc)
#define HB_GTSELF_PUTCHAR(g,r,c,m,a,u)          (g)->pFuncTable->PutChar(g,r,c,m,a,u)
#define HB_GTSELF_RECTSIZE(g,t,l,b,r)           (g)->pFuncTable->RectSize(g,t,l,b,r)
#define HB_GTSELF_SAVE(g,t,l,b,r,p)             (g)->pFuncTable->Save(g,t,l,b,r,p)
#define HB_GTSELF_REST(g,t,l,b,r,p)             (g)->pFuncTable->Rest(g,t,l,b,r,p)
#define HB_GTSELF_PUTTEXT(g,r,c,m,s,l)          (g)->pFuncTable->PutText(g,r,c,m,s,l)
#define HB_GTSELF_REPLICATE(g,r,c,m,a,u,l)      (g)->pFuncTable->Replicate(g,r,c,m,a,u,l)
#define HB_GTSELF_WRITEAT(g,r,c,s,l)            (g)->pFuncTable->WriteAt(g,r,c,s,l)
#define HB_GTSELF_WRITE(g,s,l)                  (g)->pFuncTable->Write(g,s,l)
#define HB_GTSELF_WRITECON(g,s,l)               (g)->pFuncTable->WriteCon(g,s,l)
#define HB_GTSELF_SETATTRIBUTE(g,t,l,b,r,m)     (g)->pFuncTable->SetAttribute(g,t,l,b,r,m)
#define HB_GTSELF_DRAWSHADOW(g,t,l,b,r,m)       (g)->pFuncTable->DrawShadow(g,t,l,b,r,m)
#define HB_GTSELF_SCROLL(g,t,l,b,r,m,u,v,h)     (g)->pFuncTable->Scroll(g,t,l,b,r,m,u,v,h)
#define HB_GTSELF_SCROLLUP(g,r,m,u)             (g)->pFuncTable->ScrollUp(g,r,m,u)
#define HB_GTSELF_BOX(g,t,l,b,r,f,m)            (g)->pFuncTable->Box(g,t,l,b,r,f,m)
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

#define HB_GTSUPER_LOCK(g)                       (HB_GTSUPER)->Lock(g)
#define HB_GTSUPER_UNLOCK(g)                     (HB_GTSUPER)->Unlock(g)
#define HB_GTSUPER_INIT(g,i,o,e)                 (HB_GTSUPER)->Init(g,i,o,e)
#define HB_GTSUPER_EXIT(g)                       (HB_GTSUPER)->Exit(g)
#define HB_GTSUPER_NEW(g)                        (HB_GTSUPER)->New(g)
#define HB_GTSUPER_FREE(g)                       (HB_GTSUPER)->Free(g)
#define HB_GTSUPER_RESIZE(g,r,c)                 (HB_GTSUPER)->Resize(g,r,c)
#define HB_GTSUPER_SETMODE(g,r,c)                (HB_GTSUPER)->SetMode(g,r,c)
#define HB_GTSUPER_GETSIZE(g,pr,pc)              (HB_GTSUPER)->GetSize(g,pr,pc)
#define HB_GTSUPER_SEMICOLD(g)                   (HB_GTSUPER)->SemiCold(g)
#define HB_GTSUPER_COLDAREA(g,t,l,b,r)           (HB_GTSUPER)->ColdArea(g,t,l,b,r)
#define HB_GTSUPER_EXPOSEAREA(g,t,l,b,r)         (HB_GTSUPER)->ExposeArea(g,t,l,b,r)
#define HB_GTSUPER_SCROLLAREA(g,t,l,b,r,m,u,v,h) (HB_GTSUPER)->ScrollArea(g,t,l,b,r,m,u,v,h)
#define HB_GTSUPER_TOUCHCELL(g,r,c)              (HB_GTSUPER)->TouchCell(g,r,c)
#define HB_GTSUPER_REDRAW(g,r,c,l)               (HB_GTSUPER)->Redraw(g,r,c,l)
#define HB_GTSUPER_REFRESH(g)                    (HB_GTSUPER)->Refresh(g)
#define HB_GTSUPER_FLUSH(g)                      (HB_GTSUPER)->Flush(g)
#define HB_GTSUPER_MAXCOL(g)                     (HB_GTSUPER)->MaxCol(g)
#define HB_GTSUPER_MAXROW(g)                     (HB_GTSUPER)->MaxRow(g)
#define HB_GTSUPER_CHECKPOS(g,r,c,l)             (HB_GTSUPER)->CheckPos(g,r,c,l)
#define HB_GTSUPER_SETPOS(g,r,c)                 (HB_GTSUPER)->SetPos(g,r,c)
#define HB_GTSUPER_GETPOS(g,pr,pc)               (HB_GTSUPER)->GetPos(g,pr,pc)
#define HB_GTSUPER_ISCOLOR(g)                    (HB_GTSUPER)->IsColor(g)
#define HB_GTSUPER_GETCOLORSTR(g,s)              (HB_GTSUPER)->GetColorStr(g,s)
#define HB_GTSUPER_SETCOLORSTR(g,s)              (HB_GTSUPER)->SetColorStr(g,s)
#define HB_GTSUPER_COLORSELECT(g,c)              (HB_GTSUPER)->ColorSelect(g,c)
#define HB_GTSUPER_GETCOLOR(g)                   (HB_GTSUPER)->GetColor(g)
#define HB_GTSUPER_COLORNUM(g,s)                 (HB_GTSUPER)->ColorNum(g,s)
#define HB_GTSUPER_COLORSTOSTRING(g,pc,i,ps,n)   (HB_GTSUPER)->ColorsToString(g,pc,i,ps,n)
#define HB_GTSUPER_STRINGTOCOLORS(g,ps,pc,pi)    (HB_GTSUPER)->StringToColors(g,ps,pc,pi)
#define HB_GTSUPER_GETCOLORDATA(g,pc,pn,pi)      (HB_GTSUPER)->GetColorData(g,pc,pn,pi)
#define HB_GTSUPER_GETCLEARCOLOR(g)              (HB_GTSUPER)->GetClearColor(g)
#define HB_GTSUPER_SETCLEARCOLOR(g,c)            (HB_GTSUPER)->SetClearColor(g,c)
#define HB_GTSUPER_GETCLEARCHAR(g)               (HB_GTSUPER)->GetClearChar(g)
#define HB_GTSUPER_SETCLEARCHAR(g,c)             (HB_GTSUPER)->SetClearChar(g,c)
#define HB_GTSUPER_GETCURSORSTYLE(g)             (HB_GTSUPER)->GetCursorStyle(g)
#define HB_GTSUPER_SETCURSORSTYLE(g,s)           (HB_GTSUPER)->SetCursorStyle(g,s)
#define HB_GTSUPER_GETSCRCURSOR(g,pr,pc,ps)      (HB_GTSUPER)->GetScrCursor(g,pr,pc,ps)
#define HB_GTSUPER_GETSCRCHAR(g,r,c,pm,pa,pc)    (HB_GTSUPER)->GetScrChar(g,r,c,pm,pa,pc)
#define HB_GTSUPER_PUTSCRCHAR(g,r,c,m,a,u)       (HB_GTSUPER)->PutScrChar(g,r,c,m,a,u)
#define HB_GTSUPER_DISPBEGIN(g)                  (HB_GTSUPER)->DispBegin(g)
#define HB_GTSUPER_DISPEND(g)                    (HB_GTSUPER)->DispEnd(g)
#define HB_GTSUPER_DISPCOUNT(g)                  (HB_GTSUPER)->DispCount(g)
#define HB_GTSUPER_GETCHAR(g,r,c,pm,pa,pc)       (HB_GTSUPER)->GetChar(g,r,c,pm,pa,pc)
#define HB_GTSUPER_PUTCHAR(g,r,c,m,a,u)          (HB_GTSUPER)->PutChar(g,r,c,m,a,u)
#define HB_GTSUPER_RECTSIZE(g,t,l,b,r)           (HB_GTSUPER)->RectSize(g,t,l,b,r)
#define HB_GTSUPER_SAVE(g,t,l,b,r,p)             (HB_GTSUPER)->Save(g,t,l,b,r,p)
#define HB_GTSUPER_REST(g,t,l,b,r,p)             (HB_GTSUPER)->Rest(g,t,l,b,r,p)
#define HB_GTSUPER_PUTTEXT(g,r,c,m,s,l)          (HB_GTSUPER)->PutText(g,r,c,m,s,l)
#define HB_GTSUPER_REPLICATE(g,r,c,m,a,u,l)      (HB_GTSUPER)->Replicate(g,r,c,m,a,u,l)
#define HB_GTSUPER_WRITEAT(g,r,c,s,l)            (HB_GTSUPER)->WriteAt(g,r,c,s,l)
#define HB_GTSUPER_WRITE(g,s,l)                  (HB_GTSUPER)->Write(g,s,l)
#define HB_GTSUPER_WRITECON(g,s,l)               (HB_GTSUPER)->WriteCon(g,s,l)
#define HB_GTSUPER_SETATTRIBUTE(g,t,l,b,r,m)     (HB_GTSUPER)->SetAttribute(g,t,l,b,r,m)
#define HB_GTSUPER_DRAWSHADOW(g,t,l,b,r,m)       (HB_GTSUPER)->DrawShadow(g,t,l,b,r,m)
#define HB_GTSUPER_SCROLL(g,t,l,b,r,m,u,v,h)     (HB_GTSUPER)->Scroll(g,t,l,b,r,m,u,v,h)
#define HB_GTSUPER_SCROLLUP(g,r,m,u)             (HB_GTSUPER)->ScrollUp(g,r,m,u)
#define HB_GTSUPER_BOX(g,t,l,b,r,f,m)            (HB_GTSUPER)->Box(g,t,l,b,r,f,m)
#define HB_GTSUPER_BOXD(g,t,l,b,r,f,m)           (HB_GTSUPER)->BoxD(g,t,l,b,r,f,m)
#define HB_GTSUPER_BOXS(g,t,l,b,r,f,m)           (HB_GTSUPER)->BoxS(g,t,l,b,r,f,m)
#define HB_GTSUPER_HORIZLINE(g,h,l,r,u,m)        (HB_GTSUPER)->HorizLine(g,h,l,r,u,m)
#define HB_GTSUPER_VERTLINE(g,c,t,b,u,m)         (HB_GTSUPER)->VertLine(g,c,t,b,u,m)
#define HB_GTSUPER_GETBLINK(g)                   (HB_GTSUPER)->GetBlink(g)
#define HB_GTSUPER_SETBLINK(g,b)                 (HB_GTSUPER)->SetBlink(g,b)
#define HB_GTSUPER_SETSNOWFLAG(g,b)              (HB_GTSUPER)->SetSnowFlag(g,b)
#define HB_GTSUPER_VERSION(g,i)                  (HB_GTSUPER)->Version(g,i)
#define HB_GTSUPER_SUSPEND(g)                    (HB_GTSUPER)->Suspend(g)
#define HB_GTSUPER_RESUME(g)                     (HB_GTSUPER)->Resume(g)
#define HB_GTSUPER_PREEXT(g)                     (HB_GTSUPER)->PreExt(g)
#define HB_GTSUPER_POSTEXT(g)                    (HB_GTSUPER)->PostExt(g)
#define HB_GTSUPER_OUTSTD(g,s,l)                 (HB_GTSUPER)->OutStd(g,s,l)
#define HB_GTSUPER_OUTERR(g,s,l)                 (HB_GTSUPER)->OutErr(g,s,l)
#define HB_GTSUPER_TONE(g,f,d)                   (HB_GTSUPER)->Tone(g,f,d)
#define HB_GTSUPER_BELL(g)                       (HB_GTSUPER)->Bell(g)
#define HB_GTSUPER_INFO(g,i,p)                   (HB_GTSUPER)->Info(g,i,p)
#define HB_GTSUPER_ALERT(g,m,o,n,h,d)            (HB_GTSUPER)->Alert(g,m,o,n,h,d)
#define HB_GTSUPER_SETFLAG(g,i,f)                (HB_GTSUPER)->SetFlag(g,i,f)
#define HB_GTSUPER_SETDISPCP(g,t,h,b)            (HB_GTSUPER)->SetDispCP(g,t,h,b)
#define HB_GTSUPER_SETKEYCP(g,t,h)               (HB_GTSUPER)->SetKeyCP(g,t,h)
#define HB_GTSUPER_READKEY(g,m)                  (HB_GTSUPER)->ReadKey(g,m)
#define HB_GTSUPER_INKEYGET(g,w,d,m)             (HB_GTSUPER)->InkeyGet(g,w,d,m)
#define HB_GTSUPER_INKEYPUT(g,k)                 (HB_GTSUPER)->InkeyPut(g,k)
#define HB_GTSUPER_INKEYINS(g,k)                 (HB_GTSUPER)->InkeyIns(g,k)
#define HB_GTSUPER_INKEYLAST(g,m)                (HB_GTSUPER)->InkeyLast(g,m)
#define HB_GTSUPER_INKEYNEXT(g,m)                (HB_GTSUPER)->InkeyNext(g,m)
#define HB_GTSUPER_INKEYPOLL(g)                  (HB_GTSUPER)->InkeyPoll(g)
#define HB_GTSUPER_INKEYSETTEXT(g,s,l)           (HB_GTSUPER)->InkeySetText(g,s,l)
#define HB_GTSUPER_INKEYSETLAST(g,k)             (HB_GTSUPER)->InkeySetLast(g,k)
#define HB_GTSUPER_INKEYRESET(g)                 (HB_GTSUPER)->InkeyReset(g)
#define HB_GTSUPER_INKEYEXIT(g)                  (HB_GTSUPER)->InkeyExit(g)
#define HB_GTSUPER_MOUSEINIT(g)                  (HB_GTSUPER)->MouseInit(g)
#define HB_GTSUPER_MOUSEEXIT(g)                  (HB_GTSUPER)->MouseExit(g)
#define HB_GTSUPER_MOUSEISPRESENT(g)             (HB_GTSUPER)->MouseIsPresent(g)
#define HB_GTSUPER_MOUSESHOW(g)                  (HB_GTSUPER)->MouseShow(g)
#define HB_GTSUPER_MOUSEHIDE(g)                  (HB_GTSUPER)->MouseHide(g)
#define HB_GTSUPER_MOUSEGETCURSOR(g)             (HB_GTSUPER)->MouseGetCursor(g)
#define HB_GTSUPER_MOUSESETCURSOR(g,v)           (HB_GTSUPER)->MouseSetCursor(g,v)
#define HB_GTSUPER_MOUSECOL(g)                   (HB_GTSUPER)->MouseCol(g)
#define HB_GTSUPER_MOUSEROW(g)                   (HB_GTSUPER)->MouseRow(g)
#define HB_GTSUPER_MOUSEGETPOS(g,pr,pc)          (HB_GTSUPER)->MouseGetPos(g,pr,pc)
#define HB_GTSUPER_MOUSESETPOS(g,r,c)            (HB_GTSUPER)->MouseSetPos(g,r,c)
#define HB_GTSUPER_MOUSESETBOUNDS(g,t,l,b,r)     (HB_GTSUPER)->MouseSetBounds(g,t,l,b,r)
#define HB_GTSUPER_MOUSEGETBOUNDS(g,t,l,b,r)     (HB_GTSUPER)->MouseGetBounds(g,t,l,b,r)
#define HB_GTSUPER_MOUSESTORAGESIZE(g)           (HB_GTSUPER)->MouseStorageSize(g)
#define HB_GTSUPER_MOUSESAVESTATE(g,p)           (HB_GTSUPER)->MouseSaveState(g,p)
#define HB_GTSUPER_MOUSERESTORESTATE(g,p)        (HB_GTSUPER)->MouseRestoreState(g,p)
#define HB_GTSUPER_MOUSEGETDOUBLECLICKSPEED(g)   (HB_GTSUPER)->MouseGetDoubleClickSpeed(g)
#define HB_GTSUPER_MOUSESETDOUBLECLICKSPEED(g,i) (HB_GTSUPER)->MouseSetDoubleClickSpeed(g,i)
#define HB_GTSUPER_MOUSECOUNTBUTTON(g)           (HB_GTSUPER)->MouseCountButton(g)
#define HB_GTSUPER_MOUSEBUTTONSTATE(g,b)         (HB_GTSUPER)->MouseButtonState(g,b)
#define HB_GTSUPER_MOUSEBUTTONPRESSED(g,b,r,c)   (HB_GTSUPER)->MouseButtonPressed(g,b,r,c)
#define HB_GTSUPER_MOUSEBUTTONRELEASED(g,b,r,c)  (HB_GTSUPER)->MouseButtonReleased(g,b,r,c)
#define HB_GTSUPER_MOUSEREADKEY(g,m)             (HB_GTSUPER)->MouseReadKey(g,m)
#define HB_GTSUPER_GFXPRIMITIVE(g,i,t,l,b,r,c)   (HB_GTSUPER)->GfxPrimitive(g,i,t,l,b,r,c)
#define HB_GTSUPER_GFXTEXT(g,t,l,s,c,h,w)        (HB_GTSUPER)->GfxText(g,t,l,s,c,h,w)
#define HB_GTSUPER_WHOCARES(g,p)                 (HB_GTSUPER)->WhoCares(g,p)

extern HB_EXPORT void hb_gtSetDefault( const char * szGtName );
extern HB_EXPORT BOOL hb_gtRegister( const HB_GT_INIT * gtInit );
extern HB_EXPORT BOOL hb_gtLoad( const char * szGtName, PHB_GT_FUNCS pFuncTable );
extern HB_EXPORT BOOL hb_gtUnLoad( void );
extern HB_EXPORT void hb_gtStartupInit( void );


/* low level GT functions common to different GTs supported by RTL */
extern int  hb_gt_chrmapinit( int *piTransTbl, const char *pszTerm, BOOL fSetACSC );
extern BOOL hb_gt_setClipboard( char * szClipData, ULONG ulLen );
extern BOOL hb_gt_getClipboard( char ** pszClipData, ULONG *pulLen );
#if defined( HB_OS_WIN_32 )
extern BOOL hb_gt_w32_setClipboard( UINT uFormat, char * szClipData, ULONG ulLen );
extern BOOL hb_gt_w32_getClipboard( UINT uFormat, char ** pszClipData, ULONG *pulLen );
extern int  hb_gt_w32_getKbdState( void );
extern void hb_gt_w32_setKbdState( int kbdShifts );
extern void hb_gt_w32_tone( double dFrequency, double dDuration );
#endif /* HB_OS_WIN_32 */
#if defined( HB_OS_DOS ) || defined( HB_OS_WIN_32 ) || defined( HB_OS_OS2 )
int hb_gt_dos_keyCodeTranslate( int iKey );
#endif /* HB_OS_DOS || HB_OS_WIN_32 || HB_OS_OS2 */

HB_EXTERN_END

#endif /* HB_GTCORE_H_ */
