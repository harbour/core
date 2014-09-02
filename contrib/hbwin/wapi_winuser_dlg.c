/*
 * Windows API functions
 *
 * Copyright 2011 Vailton Renato
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

#include "hbwin.h"
#include "hbwapi.h"
#include "hbvm.h"
#include "hbapiitm.h"

#include <windowsx.h>

/* Application-defined callback used with the CreateDialog and DialogBox... It
   processes messages sent to a modal or modeless dialog box. */
static BOOL CALLBACK wapi_DialogFuncProc( HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam )
{
   PHB_SYMB pSymbol;

   if( message == WM_INITDIALOG && lParam )
   {
      pSymbol = ( PHB_SYMB ) lParam;
      SetWindowLongPtr( hDlg, GWLP_USERDATA, ( LONG_PTR ) pSymbol );
   }
   else
      pSymbol = ( PHB_SYMB ) GetWindowLongPtr( hDlg, GWLP_USERDATA );

   if( pSymbol )
   {
      hb_vmPushSymbol( pSymbol );
      hb_vmPushNil();
      hb_vmPushPointer( hDlg );
      hb_vmPushNumInt( message );
      hb_vmPushNumInt( wParam );
      hb_vmPushNumInt( lParam );

      if( message == WM_COMMAND )
      {
         hb_vmPushInteger( ( int ) HIWORD( wParam ) );
         hb_vmPushInteger( ( int ) LOWORD( wParam ) );
         hb_vmDo( 6 );
      }
      else
         hb_vmDo( 4 );
   }

   return ( BOOL ) hb_parnl( -1 );
}

/* Creates a modal dialog box from a dialog box template resource. */
HB_FUNC( WAPI_DIALOGBOXPARAM )
{
   INT_PTR nResult = DialogBoxParam(
      hbwapi_par_raw_HINSTANCE( 1 ),                              /* hInstance */
      MAKEINTRESOURCE( hbwapi_par_INT( 2 ) ),                     /* lpTemplate */
      hbwapi_par_raw_HWND( 3 ),                                   /* hWndParent */
      ( DLGPROC ) wapi_DialogFuncProc,                            /* lpDialogFunc */
      ( LPARAM ) hb_itemGetSymbol( hb_param( 4, HB_IT_SYMBOL ) )  /* dwInitParam */
      );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_NINT( nResult );
}

/* Destroys a modal dialog box, causing the system to end any processing for the
   dialog box. */
HB_FUNC( WAPI_ENDDIALOG )
{
   hbwapi_ret_L( EndDialog( hbwapi_par_raw_HWND( 1 ),
                            hbwapi_par_INT( 2 ) ) );
   hbwapi_SetLastError( GetLastError() );
}

HB_FUNC( WAPI_CHECKDLGBUTTON )
{
   BOOL bResult = CheckDlgButton( hbwapi_par_raw_HWND( 1 ), hb_parni( 2 ),
                                  ( UINT ) ( HB_ISNUM( 3 ) ? hb_parni( 3 ) : hb_parl( 3 ) ) );
   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
}

HB_FUNC( WAPI_ISDLGBUTTONCHECKED )
{
   int iResult = IsDlgButtonChecked( hbwapi_par_raw_HWND( 1 ), hb_parni( 2 ) );
   hbwapi_SetLastError( GetLastError() );
   hb_retni( iResult );
}

/* Sets the title or text of a control in a dialog box. */
HB_FUNC( WAPI_SETDLGITEMTEXT )
{
   void * hStr;

   BOOL bResult = SetDlgItemText( hbwapi_par_raw_HWND( 1 ),
                                  hbwapi_par_INT( 2 ),
                                  HB_PARSTR( 3, &hStr, NULL ) );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_L( bResult );
   hb_strfree( hStr );
}

/* Retrieves the title or text associated with a control in a dialog box. */
HB_FUNC( WAPI_GETDLGITEMTEXT )
{
   int     nSize    = ( int ) SendMessage( GetDlgItem( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ) ), WM_GETTEXTLENGTH, 0, 0 );
   TCHAR * lpResult = ( TCHAR * ) hb_xgrab( ( nSize + 1 ) * sizeof( TCHAR ) );

   UINT nResult = GetDlgItemText( hbwapi_par_raw_HWND( 1 ),
                                  hbwapi_par_INT( 2 ),
                                  lpResult,
                                  nSize + 1 );

   HB_RETSTRLEN( lpResult, ( HB_SIZE ) nResult );
   hbwapi_SetLastError( GetLastError() );
   hb_xfree( lpResult );
}

/* Retrieves a handle to a control in the specified dialog box. */
HB_FUNC( WAPI_GETDLGITEM )
{
   hbwapi_ret_raw_HWND( GetDlgItem( hbwapi_par_raw_HWND( 1 ), hbwapi_par_INT( 2 ) ) );
   hbwapi_SetLastError( GetLastError() );
}

/* Adds a string to a list in a combo box. */
HB_FUNC( WAPI_COMBOBOX_ADDSTRING )
{
   void * hStr;
   int    iResult = ComboBox_AddString( hbwapi_par_raw_HWND( 1 ), HB_PARSTR( 2, &hStr, NULL ) );

   hbwapi_SetLastError( GetLastError() );
   hbwapi_ret_NI( iResult );
   hb_strfree( hStr );
}

HB_FUNC( WAPI_GETDIALOGBASEUNITS )
{
   hb_retnl( GetDialogBaseUnits() );
}

HB_FUNC( WAPI_SENDDLGITEMMESSAGE )  /* NOTE: unsafe function, may corrupt memory */
{
   void *  hText;
   HB_SIZE nLen;
   LPCTSTR szText = HB_PARSTR( 5, &hText, &nLen );

   LRESULT result;

   if( szText )
      szText = HB_STRUNSHARE( &hText, szText, nLen );

   result = SendDlgItemMessage( hbwapi_par_raw_HWND( 1 ),
                                hb_parni( 2 ),
                                ( UINT ) hb_parni( 3 ),
                                ( WPARAM ) ( HB_ISPOINTER( 4 ) ? ( HB_PTRDIFF ) hb_parptr( 4 ) : hb_parnint( 3 ) ),
                                szText ? ( LPARAM ) szText : ( LPARAM ) ( HB_ISPOINTER( 5 ) ? ( HB_PTRDIFF ) hb_parptr( 5 ) : hb_parnint( 5 ) ) );
   hbwapi_SetLastError( GetLastError() );
   hb_retnint( result );

   if( szText )
      HB_STORSTRLEN( szText, nLen, 5 );
   else
      hb_storc( NULL, 5 );

   hb_strfree( hText );
}

#define _BUFFERSIZE  65534  /* 64kB allows to build up to 255 items on the dialog */

/* Take an input pointer, return closest pointer that is
   aligned on a DWORD (4 byte) boundary. */
static LPWORD s_AlignOnDWORD( LPWORD p )
{
   HB_PTRDIFF ul = ( HB_PTRDIFF ) p;

   ul  += 3;
   ul >>= 2;
   ul <<= 2;

   return ( LPWORD ) ul;
}

HB_FUNC( __WAPI_DLGTEMPLATE_RAW_NEW )
{
   WORD * p;
   WORD * pdlgtemplate = p = ( WORD * ) hb_xgrabz( _BUFFERSIZE );
   WORD * pItems;

   /* Parameters: 12 arrays
      1 for DLG template
      11 for item properties */

   WORD    nItems = ( WORD ) hb_parvni( 1, 4 ), i;
   DWORD   lStyle = hb_parvnl( 1, 3 );
   HB_SIZE nchar;

   /* Start to fill in the DLGTEMPLATE information. Addressing by WORDs */

   *p++ = 1;                            /* version */
   *p++ = 0xFFFF;                       /* signature */
   *p++ = LOWORD( hb_parvnl( 1, 1 ) );  /* Help Id */
   *p++ = HIWORD( hb_parvnl( 1, 1 ) );

   *p++ = LOWORD( hb_parvnl( 1, 2 ) );  /* ext. style */
   *p++ = HIWORD( hb_parvnl( 1, 2 ) );

   *p++ = LOWORD( lStyle );
   *p++ = HIWORD( lStyle );

   pItems = p;

   *p++ = ( WORD ) nItems;              /* NumberOfItems */
   *p++ = ( short ) hb_parvni( 1, 5 );  /* x */
   *p++ = ( short ) hb_parvni( 1, 6 );  /* y */
   *p++ = ( short ) hb_parvni( 1, 7 );  /* cx */
   *p++ = ( short ) hb_parvni( 1, 8 );  /* cy */
   *p++ = ( short ) 0;                  /* Menu (ignored for now.) */
   *p++ = ( short ) 0x00;               /* Class also ignored */

   if( hb_parinfa( 1, 11 ) == HB_IT_STRING )
   {
      void *  hText;
      LPCWSTR szText = hb_wstrnull( hb_parastr_u16( 1, 11, HB_CDP_ENDIAN_NATIVE, &hText, &nchar ) );

      nchar = hb_wstrnlen( szText, nchar );

      if( nchar > 256 )
         nchar = 256;

      hb_wstrncpy( ( HB_WCHAR * ) p, szText, nchar );
      p += nchar + 1;

      hb_strfree( hText );
   }
   else
      *p++ = 0;

   /* add in the wPointSize and szFontName here iff the DS_SETFONT bit on */

   if( ( lStyle & DS_SETFONT ) != 0 )
   {
      void *  hText;
      LPCWSTR szText = hb_wstrnull( hb_parastr_u16( 1, 15, HB_CDP_ENDIAN_NATIVE, &hText, &nchar ) );

      *p++ = ( short ) hb_parvni( 1, 12 );
      *p++ = ( short ) hb_parvni( 1, 13 );
      *p++ = ( short ) hb_parvni( 1, 14 );

      nchar = hb_wstrnlen( szText, nchar );

      if( nchar > 256 )
         nchar = 256;

      hb_wstrncpy( ( HB_WCHAR * ) p, szText, nchar );
      p += nchar + 1;

      hb_strfree( hText );
   }

   for( i = 1; i <= nItems; i++ )
   {
      /* make sure each item starts on a DWORD boundary */
      p = s_AlignOnDWORD( p );

      *p++ = LOWORD( hb_parvnl( 2, i ) );  /* help id */
      *p++ = HIWORD( hb_parvnl( 2, i ) );

      *p++ = LOWORD( hb_parvnl( 3, i ) );  /* ext. style */
      *p++ = HIWORD( hb_parvnl( 3, i ) );

      *p++ = LOWORD( hb_parvnl( 4, i ) );  /* style */
      *p++ = HIWORD( hb_parvnl( 4, i ) );

      *p++ = ( short ) hb_parvni( 5, i );  /* x */
      *p++ = ( short ) hb_parvni( 6, i );  /* y */
      *p++ = ( short ) hb_parvni( 7, i );  /* cx */
      *p++ = ( short ) hb_parvni( 8, i );  /* cy */

      *p++ = LOWORD( hb_parvnl( 9, i ) );  /* id */
      *p++ = HIWORD( hb_parvnl( 9, i ) );  /* id */

      if( hb_parinfa( 10, i ) == HB_IT_STRING )
      {
         void *  hText;
         LPCWSTR szText = hb_parastr_u16( 10, i, HB_CDP_ENDIAN_NATIVE, &hText, &nchar );

         nchar = hb_wstrnlen( szText, nchar );

         if( nchar > 256 )
            nchar = 256;

         hb_wstrncpy( ( HB_WCHAR * ) p, szText, nchar );
         p += nchar + 1;

         hb_strfree( hText );
      }
      else
      {
         *p++ = 0xFFFF;
         *p++ = ( WORD ) hb_parvni( 10, i );
      }

      if( hb_parinfa( 11, i ) == HB_IT_STRING )
      {
         void *  hText;
         LPCWSTR szText = hb_parastr_u16( 11, i, HB_CDP_ENDIAN_NATIVE, &hText, &nchar );

         nchar = hb_wstrnlen( szText, nchar );

         if( nchar > 256 )
            nchar = 256;

         hb_wstrncpy( ( HB_WCHAR * ) p, szText, nchar );
         p += nchar + 1;

         hb_strfree( hText );
      }
      else
      {
         *p++ = 0xFFFF;
         *p++ = ( WORD ) hb_parvni( 11, i );
      }

      *p++ = 0x00;  /* extras (in array 12) */

      /* 768 is the maximum size of one item */
      if( ( ( HB_PTRDIFF ) p - ( HB_PTRDIFF ) pdlgtemplate ) > _BUFFERSIZE - 768 )
      {
         nItems = i;
         break;
      }
   }

   *pItems = ( WORD ) nItems;

   p = s_AlignOnDWORD( p );

   hb_retclen( ( char * ) pdlgtemplate, ( HB_PTRDIFF ) p - ( HB_PTRDIFF ) pdlgtemplate );

   hb_xfree( pdlgtemplate );
}
