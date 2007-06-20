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

/* extended attributes used by core screen buffer */
#define HB_GT_ATTR_BOX        0x01
#define HB_GT_ATTR_SHADOW     0x02
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
#define HB_GT_QTc    HB_GT_QTC
#define HB_GT_xvt    HB_GT_XVT
#define HB_GT_xwc    HB_GT_XWC
#define HB_GT_gui    HB_GT_GUI
#define HB_GT_alleg  HB_GT_ALLEG

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

typedef struct
{
   void     (* Init) ( FHANDLE, FHANDLE, FHANDLE );
   void     (* Exit) ( void );
   void *   (* New) ( void );
   void     (* Free) ( void * );
   BOOL     (* Resize ) ( int, int );
   BOOL     (* SetMode) ( int, int );
   void     (* GetSize) ( int *, int * );
   void     (* ColdArea) ( int, int, int, int );
   void     (* ExposeArea) ( int, int, int, int );
   void     (* ScrollArea) ( int, int, int, int, BYTE, BYTE, int, int );
   void     (* TouchCell) ( int, int );
   void     (* Redraw) ( int, int, int );
   void     (* Refresh) ( void );
   void     (* Flush) ( void );
   int      (* MaxCol) ( void );
   int      (* MaxRow) ( void );
   BOOL     (* CheckPos) ( int, int, long * );
   void     (* SetPos) ( int, int );
   void     (* GetPos) ( int *, int * );
   BOOL     (* IsColor) ( void );
   void     (* GetColorStr) ( char * );
   void     (* SetColorStr) ( const char * );
   void     (* ColorSelect) ( int );
   int      (* GetColor) ( void );
   int      (* ColorNum) ( const char * );
   void     (* ColorsToString) ( int *, int, char *, int );
   void     (* StringToColors) ( const char *, int **, int * );
   void     (* GetColorData) ( int **, int *, int * );
   int      (* GetClearColor) ( void );
   void     (* SetClearColor) ( int );
   int      (* GetClearChar) ( void );
   void     (* SetClearChar) ( int );
   int      (* GetCursorStyle) ( void );
   void     (* SetCursorStyle) ( int );
   void     (* GetScrCursor) ( int *, int *, int * );
   BOOL     (* GetScrChar) ( int, int, BYTE *, BYTE *, USHORT * );
   BOOL     (* PutScrChar) ( int, int, BYTE, BYTE, USHORT );
   void     (* DispBegin) ( void );
   void     (* DispEnd) ( void );
   int      (* DispCount) ( void );
   BOOL     (* GetChar) ( int, int, BYTE *, BYTE *, USHORT * );
   BOOL     (* PutChar) ( int, int, BYTE, BYTE, USHORT );
   long     (* RectSize) ( int, int, int, int );
   void     (* Save) ( int, int, int, int, BYTE * );
   void     (* Rest) ( int, int, int, int, BYTE * );
   void     (* PutText) ( int, int, BYTE, BYTE *, ULONG );
   void     (* Replicate) ( int, int, BYTE, BYTE, USHORT, ULONG );
   void     (* WriteAt) ( int, int, BYTE *, ULONG );
   void     (* Write) ( BYTE *, ULONG );
   void     (* WriteCon) ( BYTE *, ULONG );
   void     (* SetAttribute) ( int, int, int, int, BYTE );
   void     (* DrawShadow) ( int, int, int, int, BYTE );
   void     (* Scroll) ( int, int, int, int, BYTE, BYTE, int, int );
   void     (* ScrollUp) ( int, BYTE, BYTE );
   void     (* Box) ( int, int, int, int, BYTE *, BYTE );
   void     (* BoxD) ( int, int, int, int, BYTE *, BYTE );
   void     (* BoxS) ( int, int, int, int, BYTE *, BYTE );
   void     (* HorizLine) ( int, int, int, BYTE, BYTE );
   void     (* VertLine) ( int, int, int, BYTE, BYTE );
   BOOL     (* GetBlink) ( void );
   void     (* SetBlink) ( BOOL );
   void     (* SetSnowFlag) ( BOOL );
   char *   (* Version) ( int );
   BOOL     (* Suspend) ( void );
   BOOL     (* Resume) ( void );
   BOOL     (* PreExt) ( void );
   BOOL     (* PostExt) ( void );
   void     (* OutStd) ( BYTE *, ULONG );
   void     (* OutErr) ( BYTE *, ULONG );
   void     (* Tone) ( double, double );
   void     (* Bell) ( void );
   BOOL     (* Info) ( int, PHB_GT_INFO );
   int      (* Alert) ( PHB_ITEM, PHB_ITEM, int, int, double );
   int      (* SetFlag) ( int, int );

   /* internationalization */
   BOOL     (* SetDispCP) ( char *, char *, BOOL );
   BOOL     (* SetKeyCP) ( char *, char * );

   /* keyboard */
   int      (* ReadKey) ( int );

   /* mouse */
   void     (* MouseInit) ( void );
   void     (* MouseExit) ( void );
   BOOL     (* MouseIsPresent) ( void );
   void     (* MouseShow) ( void );
   void     (* MouseHide) ( void );
   BOOL     (* MouseGetCursor) ( void );
   void     (* MouseSetCursor) ( BOOL );
   int      (* MouseCol) ( void );
   int      (* MouseRow) ( void );
   void     (* MouseGetPos) ( int *, int * );
   void     (* MouseSetPos) ( int, int );
   void     (* MouseSetBounds) ( int, int, int, int );
   void     (* MouseGetBounds) ( int *, int *, int *, int * );
   int      (* MouseStorageSize) ( void );
   void     (* MouseSaveState) ( BYTE * );
   void     (* MouseRestoreState) ( BYTE * );
   int      (* MouseGetDoubleClickSpeed) ( void );
   void     (* MouseSetDoubleClickSpeed) ( int );
   int      (* MouseCountButton) ( void );
   BOOL     (* MouseButtonState) ( int );
   BOOL     (* MouseButtonPressed) ( int, int *, int * );
   BOOL     (* MouseButtonReleased) ( int, int *, int * );
   int      (* MouseReadKey) ( int );

   /* Graphics API */
   int      (* GfxPrimitive) ( int, int, int, int, int, int );
   void     (* GfxText) ( int, int, char *, int, int, int );

#if 0
    /* keyboard */
    int     (* ExtendedKeySupport) ( void );

    /* GT CLIPBOARD functions */
    void    (* GetClipboard) ( char *, ULONG * );
    void    (* SetClipboard) ( char *, ULONG );
    ULONG   (* GetClipboardSize) ( void );

    void    (* ProcessMessages) ( void );

    /* GT to DRIVER communication functions */
    void    (* update ) ( int );
    int     (* info ) ( int, BOOL , int , void * );

#endif

   void    (* WhoCares) ( void * );

} HB_GT_FUNCS, * PHB_GT_FUNCS;

typedef int ( * GTENTRYP_V )( void );

#define GTFUNCSCOUNT   ( sizeof( HB_GT_FUNCS ) / sizeof( GTENTRYP_V ) )

#define HB_GT_MAX_      32
#define HB_GT_NAME_MAX_ 8

typedef struct _HB_GT_INIT
{
   char           * id;
   BOOL           (* init) ( PHB_GT_FUNCS );
   PHB_GT_FUNCS   pSuperTable;
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

/*
typedef struct
{
   int   iTop;
   int   iLeft;
   int   iBottom;
   int   iRight;
} HB_GT_RECT;
typedef HB_GT_RECT * PHB_GT_RECT;

typedef struct
{
   int   iRow;
   int   iCol;
} HB_GT_CORD;
typedef HB_GT_CORD * PHB_GT_CORD;
*/

typedef struct
{
   int            iHandle;          /* Window handle */

   int            iRow;             /* cursor row position */
   int            iCol;             /* cursor column position */

   int            iHeight;          /* window height */
   int            iWidth;           /* window width */

   PHB_SCREENCELL screenBuffer;     /* window foreground (board) current buffer */
   PHB_SCREENCELL prevBuffer;       /* window foreground (board) previous buffer */

   BOOL *         pLines;           /* Touched Window lines */
   BOOL           fRefresh;         /* Should Window be refreshed */

} HB_GT_BASE;
typedef HB_GT_BASE * PHB_GT_BASE;

extern void * hb_gt_New( void );
extern void   hb_gt_Free( void * pGtPtr );
extern void   hb_gt_Init( FHANDLE hStdIn, FHANDLE hStdOut, FHANDLE hStdErr );
extern void   hb_gt_Exit( void );
extern BOOL   hb_gt_CheckPos( int iRow, int iCol, long *plIndex );
extern void   hb_gt_GetPos( int * piRow, int * piCol );
extern void   hb_gt_SetPos( int iRow, int iCol );
extern int    hb_gt_MaxCol( void );
extern int    hb_gt_MaxRow( void );
extern BOOL   hb_gt_IsColor( void );
extern void   hb_gt_GetColorStr( char * );
extern void   hb_gt_SetColorStr( const char * );
extern void   hb_gt_ColorSelect( int iColorIndex );
extern int    hb_gt_GetColor( void );
extern int    hb_gt_ColorNum( const char * );
extern void   hb_gt_ColorsToString( int *, int, char *, int );
extern void   hb_gt_StringToColors( const char *, int **, int * );
extern void   hb_gt_GetColorData( int **, int *, int * );
extern int    hb_gt_GetClearColor( void );
extern void   hb_gt_SetClearColor( int );
extern int    hb_gt_GetClearChar( void );
extern void   hb_gt_SetClearChar( int );
extern int    hb_gt_GetCursorStyle( void );
extern void   hb_gt_SetCursorStyle( int iStyle );
extern void   hb_gt_GetScrCursor( int * piRow, int * piCol, int * piStyle );
extern BOOL   hb_gt_GetScrChar( int iRow, int iCol, BYTE * pbColor, BYTE * pbAttr, USHORT * pusChar );
extern BOOL   hb_gt_PutScrChar( int iRow, int iCol, BYTE bColor, BYTE bAttr, USHORT usChar );
extern BOOL   hb_gt_GetBlink( void );
extern void   hb_gt_SetBlink( BOOL fBlink );
extern void   hb_gt_SetSnowFlag( BOOL fNoSnow );
extern void   hb_gt_DispBegin( void );
extern void   hb_gt_DispEnd( void );
extern int    hb_gt_DispCount( void );
extern BOOL   hb_gt_PreExt( void );
extern BOOL   hb_gt_PostExt( void );
extern BOOL   hb_gt_Suspend( void );
extern BOOL   hb_gt_Resume( void );
extern char * hb_gt_Version( int iType );
extern BOOL   hb_gt_GetChar( int iRow, int iCol, BYTE * pbColor, BYTE * pbAttr, USHORT * pusChar );
extern BOOL   hb_gt_PutChar( int iRow, int iCol, BYTE bColor, BYTE bAttr, USHORT usChar );
extern void   hb_gt_PutText( int iRow, int iCol, BYTE bColor, BYTE * pText, ULONG ulLen );
extern void   hb_gt_Replicate( int iRow, int iCol, BYTE bColor, BYTE bAttr, USHORT usChar, ULONG ulLen );
extern void   hb_gt_WriteAt( int iRow, int iCol, BYTE * pText, ULONG ulLength );
extern void   hb_gt_Write( BYTE * pText, ULONG ulLength );
extern void   hb_gt_WriteCon( BYTE * pText, ULONG ulLength );
extern long   hb_gt_RectSize( int iTop, int iLeft, int iBottom, int iRight );
extern void   hb_gt_Save( int iTop, int iLeft, int iBottom, int iRight, BYTE * pBuffer );
extern void   hb_gt_Rest( int iTop, int iLeft, int iBottom, int iRight, BYTE * pBuffer );
extern void   hb_gt_SetAttribute( int iTop, int iLeft, int iBottom, int iRight, BYTE bColor );
extern void   hb_gt_DrawShadow( int iTop, int iLeft, int iBottom, int iRight, BYTE bColor );
extern void   hb_gt_Scroll( int iTop, int iLeft, int iBottom, int iRight, BYTE bColor, BYTE bChar, int iRows, int iCols );
extern void   hb_gt_ScrollUp( int iRows, BYTE bColor, BYTE bChar );
extern void   hb_gt_Box( int iTop, int iLeft, int iBottom, int iRight, BYTE * pbyFrame, BYTE bColor );
extern void   hb_gt_BoxS( int iTop, int iLeft, int iBottom, int iRight, BYTE * pbyFrame, BYTE bColor );
extern void   hb_gt_BoxD( int iTop, int iLeft, int iBottom, int iRight, BYTE * pbyFrame, BYTE bColor );
extern void   hb_gt_HorizLine( int iRow, int iLeft, int iRight, BYTE bChar, BYTE bColor );
extern void   hb_gt_VertLine( int iCol, int iTop, int iBottom, BYTE bChar, BYTE bColor );
extern BOOL   hb_gt_SetMode( int iRows, int iCols );
extern BOOL   hb_gt_Resize( int iRows, int iCols );
extern void   hb_gt_GetSize( int * piRows, int * piCols );
extern void   hb_gt_ColdArea( int iTop, int iLeft, int iBottom, int iRight );
extern void   hb_gt_ExposeArea( int iTop, int iLeft, int iBottom, int iRight );
extern void   hb_gt_ScrollArea( int iTop, int iLeft, int iBottom, int iRight, BYTE bColor, BYTE bChar, int iRows, int iCols );
extern void   hb_gt_TouchCell( int iRow, int iCol );
extern void   hb_gt_Redraw( int iRow, int iCol, int iSize );
extern void   hb_gt_Refresh( void );
extern void   hb_gt_Flush( void );
extern void   hb_gt_Tone( double dFrequency, double dDuration );
extern void   hb_gt_Bell( void );
extern void   hb_gt_OutStd( BYTE * pbyStr, ULONG ulLen );
extern void   hb_gt_OutErr( BYTE * pbyStr, ULONG ulLen );
extern BOOL   hb_gt_SetDispCP( char * pszTermCDP, char * pszHostCDP, BOOL fBox );
extern BOOL   hb_gt_SetKeyCP( char * pszTermCDP, char * pszHostCDP );
extern BOOL   hb_gt_Info( int iType, PHB_GT_INFO pInfo );
extern int    hb_gt_Alert( PHB_ITEM pMessage, PHB_ITEM pOptions, int iClrNorm, int iClrHigh, double dDelay );
extern int    hb_gt_SetFlag( int iType, int iNewValue );
extern int    hb_gt_ReadKey( int iEventMask );
extern void   hb_mouse_Init( void );
extern void   hb_mouse_Exit( void );
extern BOOL   hb_mouse_IsPresent( void );
extern void   hb_mouse_Show( void );
extern void   hb_mouse_Hide( void );
extern BOOL   hb_mouse_GetCursor( void );
extern void   hb_mouse_SetCursor( BOOL fVisible );
extern int    hb_mouse_Col( void );
extern int    hb_mouse_Row( void );
extern void   hb_mouse_SetPos( int iRow, int iCol );
extern void   hb_mouse_GetPos( int * piRow, int * piCol );
extern void   hb_mouse_SetBounds( int iTop, int iLeft, int iBottom, int iRight );
extern void   hb_mouse_GetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight );
extern int    hb_mouse_StorageSize( void );
extern void   hb_mouse_SaveState( BYTE * pBuffer );
extern void   hb_mouse_RestoreState( BYTE * pBuffer );
extern int    hb_mouse_GetDoubleClickSpeed( void );
extern void   hb_mouse_SetDoubleClickSpeed( int iSpeed );
extern int    hb_mouse_CountButton( void );
extern BOOL   hb_mouse_ButtonState( int iButton );
extern BOOL   hb_mouse_ButtonPressed( int iButton, int * piRow, int * piCol );
extern BOOL   hb_mouse_ButtonReleased( int iButton, int * piRow, int * piCol );
extern int    hb_mouse_ReadKey( int iEventMask );
extern int    hb_gt_GfxPrimitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor );
extern void   hb_gt_GfxText( int iTop, int iLeft, char * szText, int iColor, int iSize, int iWidth );
extern void   hb_gt_WhoCares( void * pCargo );

#define HB_GTSUPER_NEW()                     (HB_GTSUPER)->New()
#define HB_GTSUPER_FREE(p)                   (HB_GTSUPER)->Free(p)
#define HB_GTSUPER_INIT(i,o,e)               (HB_GTSUPER)->Init(i,o,e)
#define HB_GTSUPER_EXIT()                    (HB_GTSUPER)->Exit()
#define HB_GTSUPER_CHECKPOS(r,c,l)           (HB_GTSUPER)->CheckPos(r,c,l)
#define HB_GTSUPER_GETPOS(pr,pc)             (HB_GTSUPER)->GetPos(pr,pc)
#define HB_GTSUPER_SETPOS(r,c)               (HB_GTSUPER)->SetPos(r,c)
#define HB_GTSUPER_MAXCOL()                  (HB_GTSUPER)->MaxCol()
#define HB_GTSUPER_MAXROW()                  (HB_GTSUPER)->MaxRow()
#define HB_GTSUPER_ISCOLOR()                 (HB_GTSUPER)->IsColor()
#define HB_GTSUPER_GETCOLORSTR(s)            (HB_GTSUPER)->GetColorStr(s)
#define HB_GTSUPER_SETCOLORSTR(s)            (HB_GTSUPER)->SetColorStr(s)
#define HB_GTSUPER_COLORSELECT(c)            (HB_GTSUPER)->ColorSelect(c)
#define HB_GTSUPER_GETCOLOR()                (HB_GTSUPER)->GetColor()
#define HB_GTSUPER_COLORNUM(s)               (HB_GTSUPER)->ColorNum(s)
#define HB_GTSUPER_COLORSTOSTRING(pc,i,ps,n) (HB_GTSUPER)->ColorsToString(pc,i,ps,n)
#define HB_GTSUPER_STRINGTOCOLORS(ps,pc,pi)  (HB_GTSUPER)->StringToColors(ps,pc,pi)
#define HB_GTSUPER_GETCOLORDATA(pc,pn,pi)    (HB_GTSUPER)->GetColorData(pc,pn,pi)
#define HB_GTSUPER_GETCLEARCOLOR()           (HB_GTSUPER)->GetClearColor()
#define HB_GTSUPER_SETCLEARCOLOR(c)          (HB_GTSUPER)->SetClearColor(c)
#define HB_GTSUPER_GETCLEARCHAR()            (HB_GTSUPER)->GetClearChar()
#define HB_GTSUPER_SETCLEARCHAR(c)           (HB_GTSUPER)->SetClearChar(c)
#define HB_GTSUPER_GETCURSORSTYLE()          (HB_GTSUPER)->GetCursorStyle()
#define HB_GTSUPER_SETCURSORSTYLE(s)         (HB_GTSUPER)->SetCursorStyle(s)
#define HB_GTSUPER_GETSCRCURSOR(pr,pc,ps)    (HB_GTSUPER)->GetScrCursor(pr,pc,ps)
#define HB_GTSUPER_GETSCRCHAR(r,c,pm,pa,pc)  (HB_GTSUPER)->GetScrChar(r,c,pm,pa,pc)
#define HB_GTSUPER_PUTSCRCHAR(r,c,m,a,u)     (HB_GTSUPER)->PutScrChar(r,c,m,a,u)
#define HB_GTSUPER_GETBLINK()                (HB_GTSUPER)->GetBlink()
#define HB_GTSUPER_SETBLINK(b)               (HB_GTSUPER)->SetBlink(b)
#define HB_GTSUPER_SETSNOWFLAG(b)            (HB_GTSUPER)->SetSnowFlag(b)
#define HB_GTSUPER_DISPBEGIN()               (HB_GTSUPER)->DispBegin()
#define HB_GTSUPER_DISPEND()                 (HB_GTSUPER)->DispEnd()
#define HB_GTSUPER_DISPCOUNT()               (HB_GTSUPER)->DispCount()
#define HB_GTSUPER_PREEXT()                  (HB_GTSUPER)->PreExt()
#define HB_GTSUPER_POSTEXT()                 (HB_GTSUPER)->PostExt()
#define HB_GTSUPER_SUSPEND()                 (HB_GTSUPER)->Suspend()
#define HB_GTSUPER_RESUME()                  (HB_GTSUPER)->Resume()
#define HB_GTSUPER_VERSION(i)                (HB_GTSUPER)->Version(i)
#define HB_GTSUPER_GETCHAR(r,c,pm,pa,pc)     (HB_GTSUPER)->GetChar(r,c,pm,pa,pc)
#define HB_GTSUPER_PUTCHAR(r,c,m,a,u)        (HB_GTSUPER)->PutChar(r,c,m,a,u)
#define HB_GTSUPER_PUTTEXT(r,c,m,s,l)        (HB_GTSUPER)->PutText(r,c,m,s,l)
#define HB_GTSUPER_REPLICATE(r,c,m,a,u,l)    (HB_GTSUPER)->Replicate(r,c,m,a,u,l)
#define HB_GTSUPER_WRITE(s,l)                (HB_GTSUPER)->Write(s,l)
#define HB_GTSUPER_WRITEAT(r,c,s,l)          (HB_GTSUPER)->WriteAt(r,c,s,l)
#define HB_GTSUPER_WRITECON(s,l)             (HB_GTSUPER)->WriteCon(s,l)
#define HB_GTSUPER_OUTSTD(s,l)               (HB_GTSUPER)->OutStd(s,l)
#define HB_GTSUPER_OUTERR(s,l)               (HB_GTSUPER)->OutErr(s,l)
#define HB_GTSUPER_RECTSIZE(t,l,b,r)         (HB_GTSUPER)->RectSize(t,l,b,r)
#define HB_GTSUPER_SAVE(t,l,b,r,p)           (HB_GTSUPER)->Save(t,l,b,r,p)
#define HB_GTSUPER_REST(t,l,b,r,p)           (HB_GTSUPER)->Rest(t,l,b,r,p)
#define HB_GTSUPER_SETATTRIBUTE(t,l,b,r,m)   (HB_GTSUPER)->SetAttribute(t,l,b,r,m)
#define HB_GTSUPER_DRAWSHADOW(t,l,b,r,m)     (HB_GTSUPER)->DrawShadow(t,l,b,r,m)
#define HB_GTSUPER_SCROLL(t,l,b,r,m,u,v,h)   (HB_GTSUPER)->Scroll(t,l,b,r,m,u,v,h)
#define HB_GTSUPER_SCROLLUP(r,m,u)           (HB_GTSUPER)->ScrollUp(r,m,u)
#define HB_GTSUPER_BOX(t,l,b,r,f,m)          (HB_GTSUPER)->Box(t,l,b,r,f,m)
#define HB_GTSUPER_BOXS(t,l,b,r,f,m)         (HB_GTSUPER)->BoxS(t,l,b,r,f,m)
#define HB_GTSUPER_BOXD(t,l,b,r,f,m)         (HB_GTSUPER)->BoxD(t,l,b,r,f,m)
#define HB_GTSUPER_HORIZLINE(h,l,r,u,m)      (HB_GTSUPER)->HorizLine(h,l,r,u,m)
#define HB_GTSUPER_VERTLINE(c,t,b,u,m)       (HB_GTSUPER)->VertLine(c,t,b,u,m)
#define HB_GTSUPER_SETMODE(r,c)              (HB_GTSUPER)->SetMode(r,c)
#define HB_GTSUPER_RESIZE(r,c)               (HB_GTSUPER)->Resize(r,c)
#define HB_GTSUPER_GETSIZE(pr,pc)            (HB_GTSUPER)->GetSize(pr,pc)
#define HB_GTSUPER_COLDAREA(t,l,b,r)         (HB_GTSUPER)->ColdArea(t,l,b,r)
#define HB_GTSUPER_EXPOSEAREA(t,l,b,r)       (HB_GTSUPER)->ExposeArea(t,l,b,r)
#define HB_GTSUPER_SCROLLAREA(t,l,b,r,m,u,v,h) (HB_GTSUPER)->ScrollArea(t,l,b,r,m,u,v,h)
#define HB_GTSUPER_TOUCHCELL(r,c)            (HB_GTSUPER)->TouchCell(r,c)
#define HB_GTSUPER_REDRAW(r,c,l)             (HB_GTSUPER)->Redraw(r,c,l)
#define HB_GTSUPER_REFRESH()                 (HB_GTSUPER)->Refresh()
#define HB_GTSUPER_FLUSH()                   (HB_GTSUPER)->Flush()
#define HB_GTSUPER_TONE(f,d)                 (HB_GTSUPER)->Tone(f,d)
#define HB_GTSUPER_BELL()                    (HB_GTSUPER)->Bell()
#define HB_GTSUPER_SETDISPCP(t,h,b)          (HB_GTSUPER)->SetDispCP(t,h,b)
#define HB_GTSUPER_SETKEYCP(t,h)             (HB_GTSUPER)->SetKeyCP(t,h)
#define HB_GTSUPER_INFO(i,p)                 (HB_GTSUPER)->Info(i,p)
#define HB_GTSUPER_ALERT(m,o,n,h,d)          (HB_GTSUPER)->Alert(m,o,n,h,d)
#define HB_GTSUPER_SETFLAG(i,f)              (HB_GTSUPER)->SetFlag(i,f)
#define HB_GTSUPER_READKEY(m)                (HB_GTSUPER)->ReadKey(m)
#define HB_GTSUPER_MOUSEINIT()               (HB_GTSUPER)->MouseInit()
#define HB_GTSUPER_MOUSEEXIT()               (HB_GTSUPER)->MouseExit()
#define HB_GTSUPER_MOUSEISPRESENT()          (HB_GTSUPER)->MouseIsPresent()
#define HB_GTSUPER_MOUSESHOW()               (HB_GTSUPER)->MouseShow()
#define HB_GTSUPER_MOUSEHIDE()               (HB_GTSUPER)->MouseHide()
#define HB_GTSUPER_MOUSESGETCURSOR()         (HB_GTSUPER)->MouseGetCursor()
#define HB_GTSUPER_MOUSESSETCURSOR(v)        (HB_GTSUPER)->MouseSetCursor(v)
#define HB_GTSUPER_MOUSECOL()                (HB_GTSUPER)->MouseCol()
#define HB_GTSUPER_MOUSEROW()                (HB_GTSUPER)->MouseRow()
#define HB_GTSUPER_MOUSEGETPOS(pr,pc)        (HB_GTSUPER)->MouseGetPos(pr,pc)
#define HB_GTSUPER_MOUSESETPOS(r,c)          (HB_GTSUPER)->MouseSetPos(r,c)
#define HB_GTSUPER_MOUSESETBOUNDS(t,l,b,r)   (HB_GTSUPER)->MouseSetBounds(t,l,b,r)
#define HB_GTSUPER_MOUSEGETBOUNDS(t,l,b,r)   (HB_GTSUPER)->MouseGetBounds(t,l,b,r)
#define HB_GTSUPER_MOUSESTORAGESIZE()        (HB_GTSUPER)->MouseStorageSize()
#define HB_GTSUPER_MOUSESAVESTATE(p)         (HB_GTSUPER)->MouseSaveState(p)
#define HB_GTSUPER_MOUSERESTORESTATE(p)      (HB_GTSUPER)->MouseRestoreState(p)
#define HB_GTSUPER_MOUSEGETDOUBLECLICKSPEED() (HB_GTSUPER)->MouseGetDoubleClickSpeed()
#define HB_GTSUPER_MOUSESETDOUBLECLICKSPEED(i) (HB_GTSUPER)->MouseSetDoubleClickSpeed(i)
#define HB_GTSUPER_MOUSECOUNTBUTTON()        (HB_GTSUPER)->MouseCountButton()
#define HB_GTSUPER_MOUSEBUTTONSTATE(b)       (HB_GTSUPER)->MouseButtonState(b)
#define HB_GTSUPER_MOUSEBUTTONPRESSED(b,r,c) (HB_GTSUPER)->MouseButtonPressed(b,r,c)
#define HB_GTSUPER_MOUSEBUTTONRELEASED(b,r,c) (HB_GTSUPER)->MouseButtonReleased(b,r,c)
#define HB_GTSUPER_MOUSEREADKEY(m)           (HB_GTSUPER)->MouseReadKey(m)
#define HB_GTSUPER_GFXPRIMITIVE(g,t,l,b,r,c) (HB_GTSUPER)->GfxPrimitive(g,t,l,b,r,c)
#define HB_GTSUPER_GFXTEXT(t,l,s,c,h,w)      (HB_GTSUPER)->GfxPrimitive(t,l,s,c,h,w)
#define HB_GTSUPER_WHOCARES(p)               (HB_GTSUPER)->WhoCares(p)

extern HB_EXPORT void hb_gtSetDefault( const char * szGtName );
extern HB_EXPORT BOOL hb_gtRegister( PHB_GT_INIT gtInit );
extern HB_EXPORT BOOL hb_gtLoad( const char * szGtName, PHB_GT_FUNCS pFuncTable );
extern HB_EXPORT BOOL hb_gtUnLoad( void );
extern HB_EXPORT void hb_gtStartupInit( void );


/* low level GT functions common to different GTs supported by RTL */
extern int  hb_gt_chrmapinit( int *piTransTbl, const char *pszTerm );
#if defined( HB_OS_WIN_32 )
extern void hb_gt_w32_Tone( double dFrequency, double dDuration );
extern void hb_gt_w32_SetClipboard( UINT uFormat, char * szClipData, ULONG ulLen );
extern BOOL hb_gt_w32_GetClipboard( UINT uFormat, char ** pszClipData, ULONG *pulLen );
#endif /* HB_OS_WIN_32 */


#endif /* HB_GTCORE_H_ */
