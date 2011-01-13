/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    Mini GT for GUI programs.
 *    Now it supports only low level TONE and CLIPBOARD code for Windows
 *
 * Copyright 2006 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
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


/* NOTE: User programs should never call this layer directly! */

#define HB_GT_NAME   GUI

#include "hbgtcore.h"
#include "hbinit.h"
#include "hbapiitm.h"

#if defined( HB_OS_WIN )
   #include <windows.h>
   #include "hbwinuni.h"
#endif

/* *********************************************************************** */

static int           s_GtId;
static HB_GT_FUNCS   SuperTable;
#define HB_GTSUPER   (&SuperTable)
#define HB_GTID_PTR  (&s_GtId)

#if defined( HB_OS_WIN )

typedef struct
{
   const char *   name;
   HB_SIZE        len;
   int            id;
} _HB_BUTTON_ID;

static const _HB_BUTTON_ID s_buttons[] =
{
   { "OK",             2,     0x0001 },
   { "QUIT",           4,     0x0002 },
   { "CANCEL",         6,     0x0002 },
   { "ABORT",          5,     0x0002 },
   { "RETRY",          5,     0x0004 },
   { "AGAIN",          5,     0x0004 },
   { "TRY AGAIN",      9,     0x0004 },
   { "DEFAULT",        7,     0x0008 },
   { "CONTINUE",       8,     0x0008 },
   { "IGNORE",         6,     0x0008 },
   { "YES",            3,     0x0010 },
   { "NO",             2,     0x0020 }
};

#define _HB_BUTTON_COUNT      HB_SIZEOFARRAY( s_buttons )

static int hb_gt_gui_optionId( const char * pszOption )
{
   if( pszOption )
   {
      HB_SIZE nSize;
      int i;

      while( HB_ISSPACE( *pszOption ) )
         pszOption++;
      nSize = strlen( pszOption );
      while( nSize > 0 && HB_ISSPACE( pszOption[ nSize - 1 ] ) )
         nSize--;

      if( nSize >= 2 && nSize <= 9 )
      {
         for( i = 0; i < ( int ) _HB_BUTTON_COUNT; ++i )
         {
            if( nSize == s_buttons[ i ].len &&
                hb_strnicmp( s_buttons[ i ].name, pszOption, nSize ) == 0 )
            {
               return s_buttons[ i ].id;
            }
         }
      }
   }
   return 0;
}

static int hb_gt_gui_optionPos( int id, int iType, PHB_ITEM pOptions )
{
   int iButton = 0;

   switch( id )
   {
      case IDOK:
         iButton = 0x0001;
         break;
      case IDCANCEL:
         iButton = 0x0002;
         break;
      case IDABORT:
         iButton = 0x0002;
         break;
      case IDRETRY:
         iButton = 0x0004;
         break;
      case IDIGNORE:
         iButton = 0x0008;
         break;
      case IDYES:
         iButton = 0x0010;
         break;
      case IDNO:
         iButton = 0x0020;
         break;
#ifdef IDTRYAGAIN
      case IDTRYAGAIN:
         iButton = 0x0004;
         break;
#endif
#ifdef IDCONTINUE
      case IDCONTINUE:
         iButton = 0x0008;
         break;
#endif
   }
   if( iButton )
   {
      int iOptions = hb_arrayLen( pOptions ), i;

      for( i = 1; i <= iOptions; ++i )
      {
         id = hb_gt_gui_optionId( hb_arrayGetCPtr( pOptions, i ) );
         if( iButton == id || ( iOptions == 1 && iType == id ) )
            return i;
      }
   }
   return 0;
}

static int hb_gt_gui_Alert( PHB_GT pGT, PHB_ITEM pMessage, PHB_ITEM pOptions,
                            int iClrNorm, int iClrHigh, double dDelay )
{
   void * hText;
   LPCTSTR lpText = HB_ITEMGETSTR( pMessage, &hText, NULL );
   int iRet, iOptions = pOptions ? ( int ) hb_arrayLen( pOptions ) : 0;

   if( lpText && iOptions > 0 )
   {
      int i, iType = 0;
      UINT uType;

      for( i = 1; i <= iOptions; ++i )
         iType |= hb_gt_gui_optionId( hb_arrayGetCPtr( pOptions, i ) );

      switch( iType )
      {
         case 0x01:
            uType = MB_OK;
            break;
         case 0x03:
            uType = MB_OKCANCEL;
            break;
         case 0x06:
            uType = MB_RETRYCANCEL;
            break;
         case 0x0E:
#ifdef MB_CANCELTRYCONTINUE
            uType = hb_iswin2k() ? MB_CANCELTRYCONTINUE : MB_ABORTRETRYIGNORE;
#else
            uType = MB_ABORTRETRYIGNORE;
#endif
            break;
         case 0x12:
            uType = MB_OKCANCEL;
            break;
         case 0x21:
            uType = MB_YESNO;
            break;
         case 0x30:
            uType = MB_YESNO;
            break;
         case 0x32:
            uType = MB_YESNOCANCEL;
            break;
         default:
            uType = MB_OK;
            break;
      }

      iRet = MessageBox( NULL, lpText, TEXT( "" ), uType );
      iRet = hb_gt_gui_optionPos( iRet, iType, pOptions );
   }
   else
      iRet = HB_GTSUPER_ALERT( pGT, pMessage, pOptions, iClrNorm,
                               iClrHigh, dDelay );

   hb_strfree( hText );

   return iRet;
}

#endif /* HB_OS_WIN */

/* *********************************************************************** */

static const char * hb_gt_gui_Version( PHB_GT pGT, int iType )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_gui_Version(%p,%d)", pGT, iType ) );

   HB_SYMBOL_UNUSED( pGT );

   if ( iType == 0 )
      return HB_GT_DRVNAME( HB_GT_NAME );

   return "Harbour Terminal: Windows dummy console for GUI programs";
}

/* *********************************************************************** */
/* dDuration is in 'Ticks' (18.2 per second) */
static void hb_gt_gui_Tone( PHB_GT pGT, double dFrequency, double dDuration )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_gui_Tone(%p,%lf,%lf)", pGT, dFrequency, dDuration));

#if defined( HB_OS_WIN )
   HB_SYMBOL_UNUSED( pGT );
   hb_gt_winapi_tone( dFrequency, dDuration );
#else
   HB_GTSUPER_TONE( pGT, dFrequency, dDuration );
#endif
}

/* *********************************************************************** */

static HB_BOOL hb_gt_gui_Info( PHB_GT pGT, int iType, PHB_GT_INFO pInfo )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_gt_gui_Info(%p,%d,%p)", pGT, iType, pInfo ) );

   switch( iType )
   {
#if defined( HB_OS_WIN )
      case HB_GTI_CLIPBOARDDATA:
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_STRING )
#if defined( UNICODE )
            hb_gt_winapi_setClipboard( CF_UNICODETEXT, pInfo->pNewVal );
#else
            hb_gt_winapi_setClipboard( CF_TEXT, pInfo->pNewVal );
#endif
         else
         {
            if( pInfo->pResult == NULL )
               pInfo->pResult = hb_itemNew( NULL );
#if defined( UNICODE )
            hb_gt_winapi_getClipboard( CF_UNICODETEXT, pInfo->pResult );
#else
            hb_gt_winapi_getClipboard( CF_TEXT, pInfo->pResult );
#endif
         }
         break;

      case HB_GTI_KBDSHIFTS:
         pInfo->pResult = hb_itemPutNI( pInfo->pResult, hb_gt_winapi_getKbdState() );
         if( hb_itemType( pInfo->pNewVal ) & HB_IT_NUMERIC )
            hb_gt_winapi_setKbdState( hb_itemGetNI( pInfo->pNewVal ) );
         break;
#endif
      default:
         return HB_GTSUPER_INFO( pGT, iType, pInfo );
   }

   return HB_TRUE;
}

/* *********************************************************************** */

static HB_BOOL hb_gt_FuncInit( PHB_GT_FUNCS pFuncTable )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_gt_FuncInit(%p)", pFuncTable));

   pFuncTable->Version                    = hb_gt_gui_Version;
   pFuncTable->Tone                       = hb_gt_gui_Tone;
   pFuncTable->Info                       = hb_gt_gui_Info;
#if defined( HB_OS_WIN )
   pFuncTable->Alert                      = hb_gt_gui_Alert;
#endif

   return HB_TRUE;
}

/* *********************************************************************** */

#include "hbgtreg.h"

/* *********************************************************************** */
