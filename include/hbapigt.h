/*
 * Harbour Project source code:
 * Header file for the Terminal API
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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
 * Copyright 1999 David G. Holm <dholm@jsd-llc.com>
 *    Keyboard related declarations
 *    Cursor declarations
 * See above for licensing terms.
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
 *    Mouse related declarations
 *    Undocumented GT API declarations
 *
 * Copyright 2005 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
 *    Internal GT code reimplemented in different way
 *
 * See COPYING.txt for licensing terms.
 *
 */

#ifndef HB_APIGT_H_
#define HB_APIGT_H_

#include "hbapi.h"
#include "hbapicdp.h"

HB_EXTERN_BEGIN

#include "inkey.ch"
#include "setcurs.ch"
#include "hbgtinfo.ch"

/* maximum length of color string */
#define HB_CLRSTR_LEN           64

/* attributes for color strings, these are the same as the ones in color.ch
   but prefixed with HB_ to avoid collision. */
#define HB_CLR_STANDARD         0
#define HB_CLR_ENHANCED         1
#define HB_CLR_BORDER           2
#define HB_CLR_BACKGROUND       3
#define HB_CLR_UNSELECTED       4
#define HB_CLR_MAX_             HB_CLR_UNSELECTED

/* extended attributes used by core screen buffer */
#define HB_GT_ATTR_BOX        0x01
#define HB_GT_ATTR_SHADOW     0x02
#define HB_GT_ATTR_UNDEF      0x40
#define HB_GT_ATTR_REFRESH    0x80

/* strings for borders (same as box.ch, but defined for use by C) */
#define _B_SINGLE              "\xDA\xC4\xBF\xB3\xD9\xC4\xC0\xB3"  /* "┌─┐│┘─└│" */
#define _B_DOUBLE              "\xC9\xCD\xBB\xBA\xBC\xCD\xC8\xBA"  /* "╔═╗║╝═╚║" */
#define _B_SINGLE_DOUBLE       "\xD6\xC4\xB7\xBA\xBD\xC4\xD3\xBA"  /* "╓─╖║╜─╙║" */
#define _B_DOUBLE_SINGLE       "\xD5\xCD\xB8\xB3\xBE\xCD\xD4\xB3"  /* "╒═╕│╛═╘│" */
#define HB_B_SINGLE_V          '\xB3'                              /* '│' */
#define HB_B_SINGLE_H          '\xC4'                              /* '─' */
#define HB_B_DOUBLE_V          '\xBA'                              /* '║' */
#define HB_B_DOUBLE_H          '\xCD'                              /* '═' */

#define HB_B_SINGLE_W         { 0x250C, 0x2500, 0x2510, 0x2502, 0x2518, 0x2500, 0x2514, 0x2502, 0x0000 }
#define HB_B_DOUBLE_W         { 0x2554, 0x2550, 0x2557, 0x2551, 0x255D, 0x2550, 0x255A, 0x2551, 0x0000 }
#define HB_B_SINGLE_DOUBLE_W  { 0x2553, 0x2500, 0x2556, 0x2551, 0x255C, 0x2500, 0x2559, 0x2551, 0x0000 }
#define HB_B_DOUBLE_SINGLE_W  { 0x2552, 0x2550, 0x2555, 0x2502, 0x255B, 0x2550, 0x2558, 0x2502, 0x0000 }
#define HB_B_HALF_FULL_W      { 0x2588, 0x2580, 0x2588, 0x2588, 0x2588, 0x2584, 0x2588, 0x2588, 0x0000 }
#define HB_B_HALF_W           { 0x2590, 0x2580, 0x258C, 0x258C, 0x258C, 0x2584, 0x2590, 0x2590, 0x0000 }
#define HB_B_FULL_HALF_W      { 0x2590, 0x2588, 0x258C, 0x258C, 0x258C, 0x2588, 0x2590, 0x2590, 0x0000 }
#define HB_B_FULL_W           { 0x2588, 0x2588, 0x2588, 0x2588, 0x2588, 0x2588, 0x2588, 0x2588, 0x0000 }


#if defined( HB_COMPAT_C53 ) && ! defined( HB_CLP_STRICT )
#  define HB_DEFAULT_INKEY_BUFSIZE  50
#else
#  define HB_DEFAULT_INKEY_BUFSIZE  15
#endif


#define HB_STDIN_HANDLE    0
#define HB_STDOUT_HANDLE   1
#define HB_STDERR_HANDLE   2


/* structure used to pass/receive parameters in hb_gtInfo() */

typedef struct
{
   PHB_ITEM pNewVal;
   PHB_ITEM pResult;
   PHB_ITEM pNewVal2;
} HB_GT_INFO, * PHB_GT_INFO;

/* Public interface. These should never change, only be added to. */

extern void hb_gtIsGtRef( void * );

extern HB_EXPORT void   hb_gtStartupInit( void );
extern HB_EXPORT void   hb_gtSetDefault( const char * szGtName );
extern HB_EXPORT void * hb_gtAlloc( void * hGT );
extern HB_EXPORT void   hb_gtRelease( void * hGT );
extern HB_EXPORT void   hb_gtAttach( void * hGT );
extern HB_EXPORT void * hb_gtSwap( void * hGT );
extern HB_EXPORT HB_BOOL   hb_gtReload( const char * szGtName,
                                     HB_FHANDLE hFilenoStdin,
                                     HB_FHANDLE hFilenoStdout,
                                     HB_FHANDLE hFilenoStderr );
extern HB_EXPORT void * hb_gtCreate( const char * szGtName,
                                     HB_FHANDLE hFilenoStdin,
                                     HB_FHANDLE hFilenoStdout,
                                     HB_FHANDLE hFilenoStderr );

extern HB_EXPORT HB_ERRCODE hb_gtInit( HB_FHANDLE hFilenoStdin, HB_FHANDLE hFilenoStdout, HB_FHANDLE hFilenoStderr );
extern HB_EXPORT HB_ERRCODE hb_gtExit( void );
extern HB_EXPORT HB_ERRCODE hb_gtLock( void );
extern HB_EXPORT HB_ERRCODE hb_gtUnlock( void );
extern HB_EXPORT HB_ERRCODE hb_gtBox( int iTop, int iLeft, int iBottom, int iRight, const char * szFrame );
extern HB_EXPORT HB_ERRCODE hb_gtBoxD( int iTop, int iLeft, int iBottom, int iRight );
extern HB_EXPORT HB_ERRCODE hb_gtBoxS( int iTop, int iLeft, int iBottom, int iRight );
extern HB_EXPORT HB_ERRCODE hb_gtDrawBox( int iTop, int iLeft, int iBottom, int iRight, const char * szFrame, int iColor );
extern HB_EXPORT HB_ERRCODE hb_gtColorSelect( int iColorIndex );
extern HB_EXPORT int        hb_gtColorToN( const char * szColorString );
extern HB_EXPORT HB_ERRCODE hb_gtColorsToString( int * pColors, int iColorCount, char * pszColorString, int iBufSize );
extern HB_EXPORT HB_ERRCODE hb_gtDispBegin( void );
extern HB_EXPORT int        hb_gtDispCount( void );
extern HB_EXPORT HB_ERRCODE hb_gtDispEnd( void );
extern HB_EXPORT HB_ERRCODE hb_gtDrawShadow( int iTop, int iLeft, int iBottom, int iRight, int iColor );
extern HB_EXPORT HB_ERRCODE hb_gtGetBlink( HB_BOOL * pbBlink );
extern HB_EXPORT HB_ERRCODE hb_gtGetColorStr( char * pszColorString );
extern HB_EXPORT HB_ERRCODE hb_gtGetCursor( int * piCursorShape );
extern HB_EXPORT HB_ERRCODE hb_gtGetPos( int * piRow, int * piCol );
extern HB_EXPORT HB_BOOL    hb_gtIsColor( void );
extern HB_EXPORT int        hb_gtMaxCol( void );
extern HB_EXPORT int        hb_gtMaxRow( void );
extern HB_EXPORT HB_ERRCODE hb_gtPostExt( void );
extern HB_EXPORT HB_ERRCODE hb_gtPreExt( void );
extern HB_EXPORT HB_ERRCODE hb_gtSuspend( void ); /* prepare the reminal for shell output */
extern HB_EXPORT HB_ERRCODE hb_gtResume( void ); /* resume the terminal after the shell output */
extern HB_EXPORT int        hb_gtReadKey( int iEventMask );
extern HB_EXPORT HB_ERRCODE hb_gtRectSize( int iTop, int iLeft, int iBottom, int iRight, HB_SIZE * pnBuffSize );
extern HB_EXPORT HB_ERRCODE hb_gtRepChar( int iRow, int iCol, HB_USHORT usChar, HB_SIZE nCount );
extern HB_EXPORT HB_ERRCODE hb_gtSave( int iTop, int iLeft, int iBottom, int iRight, void * pScrBuff );
extern HB_EXPORT HB_ERRCODE hb_gtRest( int iTop, int iLeft, int iBottom, int iRight, const void * pScrBuff );
extern HB_EXPORT HB_ERRCODE hb_gtGetChar( int iRow, int iCol, int * piColor, HB_BYTE * pbAttr, HB_USHORT * pusChar );
extern HB_EXPORT HB_ERRCODE hb_gtPutChar( int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar );
extern HB_EXPORT HB_ERRCODE hb_gtBeginWrite( void );
extern HB_EXPORT HB_ERRCODE hb_gtEndWrite( void );
extern HB_EXPORT HB_ERRCODE hb_gtScrDim( int * piHeight, int * piWidth );
extern HB_EXPORT HB_ERRCODE hb_gtScroll( int iTop, int iLeft, int iBottom, int iRight, int iRows, int iCols );
extern HB_EXPORT HB_ERRCODE hb_gtScrollUp( int iRows );
extern HB_EXPORT HB_ERRCODE hb_gtSetAttribute( int iTop, int iLeft, int iBottom, int iRight, int iColor );
extern HB_EXPORT HB_ERRCODE hb_gtSetBlink( HB_BOOL bBlink );
extern HB_EXPORT HB_ERRCODE hb_gtSetColorStr( const char * pszColorString );
extern HB_EXPORT HB_ERRCODE hb_gtSetCursor( int iCursorShape );
extern HB_EXPORT HB_ERRCODE hb_gtSetMode( int iRows, int iCols );
extern HB_EXPORT HB_ERRCODE hb_gtSetPos( int iRow, int iCol );
extern HB_EXPORT HB_ERRCODE hb_gtSetSnowFlag( HB_BOOL bNoSnow );
extern HB_EXPORT HB_ERRCODE hb_gtTone( double dFrequency, double dDuration );
extern HB_EXPORT HB_ERRCODE hb_gtWrite( const char * szStr, HB_SIZE nLen );
extern HB_EXPORT HB_ERRCODE hb_gtWriteAt( int iRow, int iCol, const char * szStr, HB_SIZE nLen );
extern HB_EXPORT HB_ERRCODE hb_gtWriteCon( const char * szStr, HB_SIZE nLen );
extern HB_EXPORT HB_ERRCODE hb_gtPutText( int iRow, int iCol, const char * szStr, HB_SIZE nLength, int iColor );
extern HB_EXPORT const char * hb_gtVersion( int iType );
extern HB_EXPORT HB_ERRCODE hb_gtOutStd( const char * szStr, HB_SIZE nLen );
extern HB_EXPORT HB_ERRCODE hb_gtOutErr( const char * szStr, HB_SIZE nLen );
extern HB_EXPORT HB_ERRCODE hb_gtSetDispCP( const char * pszTermCDP, const char * pszHostCDP, HB_BOOL fBox );
extern HB_EXPORT HB_ERRCODE hb_gtSetKeyCP( const char * pszTermCDP, const char * pszHostCDP );
extern HB_EXPORT PHB_CODEPAGE hb_gtHostCP( void );
extern HB_EXPORT PHB_CODEPAGE hb_gtBoxCP( void );
extern HB_EXPORT HB_ERRCODE hb_gtInfo( int iType, PHB_GT_INFO pInfo );
extern HB_EXPORT int        hb_gtAlert( PHB_ITEM pMessage, PHB_ITEM pOptions, int iClrNorm, int iClrHigh, double dDelay );
extern HB_EXPORT int        hb_gtSetFlag( int iType, int iNewValue );
extern HB_EXPORT int        hb_gtGetCurrColor( void );
extern HB_EXPORT int        hb_gtGetClearColor( void );
extern HB_EXPORT HB_ERRCODE hb_gtSetClearColor( int iColor );
extern HB_EXPORT HB_USHORT  hb_gtGetClearChar( void );
extern HB_EXPORT HB_ERRCODE hb_gtSetClearChar( HB_USHORT usChar );
extern HB_EXPORT HB_ERRCODE hb_gtGetScrChar( int iRow, int iCol, int * piColor, HB_BYTE * pbAttr, HB_USHORT * pusChar );
extern HB_EXPORT HB_ERRCODE hb_gtPutScrChar( int iRow, int iCol, int iColor, HB_BYTE bAttr, HB_USHORT usChar );
extern HB_EXPORT HB_ERRCODE hb_gtFlush( void );
extern HB_EXPORT HB_ERRCODE hb_gtGetPosEx( int * piRow, int * piCol );
extern HB_EXPORT HB_ERRCODE hb_gtScrollEx( int iTop, int iLeft, int iBottom, int iRight, int iColor, int iChar, int iRows, int iCols );
extern HB_EXPORT HB_ERRCODE hb_gtBoxEx( int iTop, int iLeft, int iBottom, int iRight, const char * szFrame, int iColor );
extern HB_EXPORT int        hb_gtGfxPrimitive( int iType, int iTop, int iLeft, int iBottom, int iRight, int iColor );
extern HB_EXPORT HB_ERRCODE hb_gtGfxText( int iTop, int iLeft, const char * szText, int iColor, int iSize, int iWidth );

extern HB_EXPORT HB_BOOL    hb_mouseIsPresent( void );
extern HB_EXPORT HB_BOOL    hb_mouseGetCursor( void );
extern HB_EXPORT void       hb_mouseSetCursor( HB_BOOL bVisible );
extern HB_EXPORT int        hb_mouseCol( void );
extern HB_EXPORT int        hb_mouseRow( void );
extern HB_EXPORT void       hb_mouseGetPos( int * piRow, int * piCol );
extern HB_EXPORT void       hb_mouseSetPos( int iRow, int iCol );
extern HB_EXPORT void       hb_mouseSetBounds( int iTop, int iLeft, int iBottom, int iRight );
extern HB_EXPORT void       hb_mouseGetBounds( int * piTop, int * piLeft, int * piBottom, int * piRight );
extern HB_EXPORT int        hb_mouseStorageSize( void );
extern HB_EXPORT void       hb_mouseSaveState( void * pBuffer );
extern HB_EXPORT void       hb_mouseRestoreState( const void * pBuffer );
extern HB_EXPORT int        hb_mouseGetDoubleClickSpeed( void );
extern HB_EXPORT void       hb_mouseSetDoubleClickSpeed( int iSpeed );
extern HB_EXPORT int        hb_mouseCountButton( void );
extern HB_EXPORT HB_BOOL    hb_mouseButtonState( int iButton );
extern HB_EXPORT HB_BOOL    hb_mouseButtonPressed( int iButton, int * piRow, int * piCol );
extern HB_EXPORT HB_BOOL    hb_mouseButtonReleased( int iButton, int * piRow, int * piCol );
extern HB_EXPORT int        hb_mouseReadKey( int iEventMask );

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

/* Undocumented CA-Cl*pper 5.x GT API calls */

#define HB_GT_WND void
#define HB_GT_RGB void
#define HB_GT_SLR void

extern HB_EXPORT void       hb_gtWCreate( HB_GT_RECT * rect, HB_GT_WND ** wnd );
extern HB_EXPORT void       hb_gtWDestroy( HB_GT_WND * wnd );
extern HB_EXPORT HB_BOOL    hb_gtWFlash( void );
extern HB_EXPORT void       hb_gtWApp( HB_GT_WND ** wnd );
extern HB_EXPORT void       hb_gtWCurrent( HB_GT_WND * wnd );
extern HB_EXPORT void       hb_gtWPos( HB_GT_WND * wnd, HB_GT_RECT * rect );
extern HB_EXPORT HB_BOOL    hb_gtWVis( HB_GT_WND * wnd, HB_USHORT uiStatus );

extern HB_EXPORT HB_ERRCODE hb_gtSLR( HB_GT_SLR * pSLR ); /* System Level Request */
extern HB_EXPORT HB_ERRCODE hb_gtModalRead( void * );
extern HB_EXPORT HB_ERRCODE hb_gtFlushCursor( void );
extern HB_EXPORT HB_ERRCODE hb_gtSetColor( HB_GT_RGB * color );
extern HB_EXPORT HB_ERRCODE hb_gtGetColor( HB_GT_RGB * color );
extern HB_EXPORT HB_ERRCODE hb_gtSetBorder( HB_GT_RGB * color );


/* Keyboard related declarations */

#define HB_BREAK_FLAG 256 /* 256, because that's what DJGPP returns Ctrl+Break as.
                             Clipper has no key code 256, so it may as well be
                             used for all the Harbour builds that need it */

/* mouse buttons */
#define HB_MBUTTON_LEFT         0
#define HB_MBUTTON_RIGHT        1
#define HB_MBUTTON_MIDDLE       2


/* Harbour keyboard support functions */
extern HB_EXPORT int        hb_inkey( HB_BOOL bWait, double dSeconds, int iEvenMask ); /* Wait for keyboard input */
extern HB_EXPORT void       hb_inkeyPut( int ch );          /* Inserts an inkey code into the keyboard buffer */
extern HB_EXPORT void       hb_inkeyIns( int ch );          /* Inserts an inkey code into the keyboard buffer */
extern HB_EXPORT int        hb_inkeyLast( int iEvenMask );  /* Return the value of the last key that was extracted */
extern HB_EXPORT int        hb_inkeyNext( int iEvenMask );  /* Return the next key without extracting it */
extern HB_EXPORT void       hb_inkeyPoll( void );           /* Poll the console keyboard to stuff the Harbour buffer */
extern HB_EXPORT void       hb_inkeyReset( void );          /* Reset the Harbour keyboard buffer */
extern HB_EXPORT void       hb_inkeySetText( const char * szText, HB_SIZE nLen ); /* Set text into inkey buffer */
extern HB_EXPORT int        hb_inkeySetLast( int iKey );    /* Set new LastKey() value, return previous one */
extern HB_EXPORT void       hb_inkeyExit( void );           /* reset inkey pool to default state and free any allocated resources */

extern HB_EXPORT HB_SIZE    hb_inkeyKeyString( int iKey, char * buffer, HB_SIZE nSize ); /* convert key value to string */
extern HB_EXPORT int        hb_inkeyKeyStd( int iKey );     /* convert Harbour extended key code to cl*pper inkey code */
extern HB_EXPORT int        hb_inkeyKeyMod( int iKey );     /* extract keyboard modifiers HB_KF_* from Harbour extended key code */
extern HB_EXPORT int        hb_inkeyKeyVal( int iKey );     /* extract key/character code from Harbour extended key code */

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

#define HB_INKEY_NEW_MKEY( k, f )   ( HB_INKEY_NEW_VALF( k, f ) | HB_INKEY_EXT_BIT | HB_INKEY_EXT_MOUSEKEY )
#define HB_INKEY_NEW_KEY( k, f )    ( HB_INKEY_NEW_VALF( k, f ) | HB_INKEY_EXT_BIT | HB_INKEY_EXT_KEY )
#define HB_INKEY_NEW_CHAR( b )      ( ( b ) | ( HB_INKEY_EXT_BIT | HB_INKEY_EXT_CHAR ) )
#define HB_INKEY_NEW_CHARF( b, f )  ( HB_INKEY_NEW_VALF( b, f ) | ( HB_INKEY_EXT_BIT | HB_INKEY_EXT_CHAR ) )
#define HB_INKEY_NEW_UNICODE( b )   ( ( b ) | ( HB_INKEY_EXT_BIT | HB_INKEY_EXT_UNICODE ) )
#define HB_INKEY_NEW_UNICODEF( b, f ) ( HB_INKEY_NEW_VALF( b, f ) | ( HB_INKEY_EXT_BIT | HB_INKEY_EXT_UNICODE ) )

#define HB_INKEY_NEW_MPOS( x, y )   ( ( ( ( y ) & HB_INKEY_EXT_POSMASK ) << HB_INKEY_EXT_POSBITS ) | \
                                      ( ( x ) & HB_INKEY_EXT_POSMASK ) | \
                                      ( HB_INKEY_EXT_BIT | HB_INKEY_EXT_MOUSEPOS ) )

#define HB_INKEY_MOUSEPOSX( n )     ( ( n ) & HB_INKEY_EXT_POSMASK )
#define HB_INKEY_MOUSEPOSY( n )     ( ( ( n ) >> HB_INKEY_EXT_POSBITS ) & HB_INKEY_EXT_POSMASK )

#define HB_INKEY_VALUE( n )         ( ( n ) & HB_INKEY_EXT_VALMASK )
#define HB_INKEY_FLAGS( n )         ( ( ( n ) & HB_INKEY_EXT_FLAGMASK ) >> HB_INKEY_EXT_VALBITS )

HB_EXTERN_END

#endif /* HB_APIGT_H_ */
