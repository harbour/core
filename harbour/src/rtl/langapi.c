/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * The Language API
 *
 * Copyright 1999-2001 Viktor Szakats (harbour syenar.net)
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

#include "hbapi.h"
#include "hbapilng.h"

/* Language name: English */
/* ISO language code (2 chars): EN */
/* Codepage: 437 */

static HB_LANG s_lang_en =
{
   {
      /* Identification */

      "EN",                        /* ID */
      "English",                   /* Name (in English) */
      "English",                   /* Name (in native language) */
      "EN",                        /* RFC ID */
      "437",                       /* Codepage */
      "",                          /* Version */

      /* Month names */

      "January",
      "February",
      "March",
      "April",
      "May",
      "June",
      "July",
      "August",
      "September",
      "October",
      "November",
      "December",

      /* Day names */

      "Sunday",
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",

      /* CA-Cl*pper compatible natmsg items */

      "Database Files    # Records    Last Update     Size",
      "Do you want more samples?",
      "Page No.",
      "** Subtotal **",
      "* Subsubtotal *",
      "*** Total ***",
      "Ins",
      "   ",
      "Invalid date",
      "Range: ",
      " - ",
      "Y/N",
      "INVALID EXPRESSION",

      /* Error description names */

      "Unknown error",
      "Argument error",
      "Bound error",
      "String overflow",
      "Numeric overflow",
      "Zero divisor",
      "Numeric error",
      "Syntax error",
      "Operation too complex",
      "",
      "",
      "Memory low",
      "Undefined function",
      "No exported method",
      "Variable does not exist",
      "Alias does not exist",
      "No exported variable",
      "Illegal characters in alias",
      "Alias already in use",
      "",
      "Create error",
      "Open error",
      "Close error",
      "Read error",
      "Write error",
      "Print error",
      "",
      "",
      "",
      "",
      "Operation not supported",
      "Limit exceeded",
      "Corruption detected",
      "Data type error",
      "Data width error",
      "Workarea not in use",
      "Workarea not indexed",
      "Exclusive required",
      "Lock required",
      "Write not allowed",
      "Append lock failed",
      "Lock failure",
      "",
      "",
      "",
      "Object destructor failure",
      "array access",
      "array assign",
      "array dimension",
      "not an array",
      "conditional",

      /* Internal error names */

      "Unrecoverable error %lu: ",
      "Error recovery failure",
      "No ERRORBLOCK() for error",
      "Too many recursive error handler calls",
      "RDD invalid or failed to load",
      "Invalid method type from %s",
      "hb_xgrab can't allocate memory",
      "hb_xrealloc called with a NULL pointer",
      "hb_xrealloc called with an invalid pointer",
      "hb_xrealloc can't reallocate memory",
      "hb_xfree called with an invalid pointer",
      "hb_xfree called with a NULL pointer",
      "Can\'t locate the starting procedure: \'%s\'",
      "No starting procedure",
      "Unsupported VM opcode",
      "Symbol item expected from %s",
      "Invalid symbol type for self from %s",
      "Codeblock expected from %s",
      "Incorrect item type on the stack trying to pop from %s",
      "Stack underflow",
      "An item was going to be copied to itself from %s",
      "Invalid symbol item passed as memvar %s",
      "Memory buffer overflow",
      "hb_xgrab requested to allocate zero bytes",
      "hb_xrealloc requested to resize to zero bytes",
      "hb_xalloc requested to allocate zero bytes",

      "YYYY/MM/DD",
      "Y",
      "N"
   }
};

HB_LANG_ANNOUNCE( EN )

/* Always link in the default language */
/* HB_LANG_REQUEST( HB_LANG_DEFAULT ); */

/* NOTE: This is the maximum number of registered languages, later this can be
         made dynamic. */
#define HB_LANG_MAX_ 128

#define HB_LANG_ITEM_ID_ID         0
#define HB_LANG_ITEM_ID_NAME       1
#define HB_LANG_ITEM_ID_NAMENAT    2
#define HB_LANG_ITEM_ID_RFCID      3
#define HB_LANG_ITEM_ID_CODEPAGE   4

static PHB_LANG s_langList[ HB_LANG_MAX_ ] = { &s_lang_en };

static int hb_langFindPos( const char * pszID )
{
   int iPos;

   if( pszID )
   {
      for( iPos = 0; iPos < HB_LANG_MAX_; iPos++ )
      {
         if( s_langList[ iPos ] && strcmp( ( const char * ) s_langList[ iPos ]->pItemList[ HB_LANG_ITEM_BASE_ID + HB_LANG_ITEM_ID_ID ], pszID ) == 0 )
            return iPos;
      }
   }

   return -1;
}

HB_BOOL hb_langRegister( PHB_LANG lang )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_langRegister(%p)", lang));

   if( lang )
   {
      int iPos = hb_langFindPos( ( const char * ) lang->pItemList[ HB_LANG_ITEM_BASE_ID + HB_LANG_ITEM_ID_ID ] );

      if( iPos == -1 )
      {
         for( iPos = 0; iPos < HB_LANG_MAX_; iPos++ )
         {
            if( s_langList[ iPos ] == NULL )
            {
               s_langList[ iPos ] = lang;
               return HB_TRUE;
            }
         }
      }
      else
      {
         s_langList[ iPos ] = lang;
         return HB_TRUE;
      }
   }

   return HB_FALSE;
}

HB_BOOL hb_langDeRegister( const char * pszID )
{
   int iPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_langDeRegister(%s)", pszID));

   iPos = hb_langFindPos( pszID );

   if( iPos != -1 )
   {
      s_langList[ iPos ] = NULL;
      return HB_TRUE;
   }
   else
      return HB_FALSE;
}

PHB_LANG hb_langFind( const char * pszID )
{
   int iPos;

   HB_TRACE(HB_TR_DEBUG, ("hb_langFind(%s)", pszID));

   iPos = hb_langFindPos( pszID );

   return ( iPos != -1 ) ? s_langList[ iPos ] : NULL;
}

PHB_LANG hb_langSelect( PHB_LANG lang )
{
   PHB_LANG langOld;

   HB_TRACE(HB_TR_DEBUG, ("hb_langSelect(%p)", lang));

   langOld = hb_vmLang();
   if( lang )
      hb_vmSetLang( lang );

   return langOld;
}

const char * hb_langSelectID( const char * pszID )
{
   const char * pszIDOld = hb_langID();

   HB_TRACE(HB_TR_DEBUG, ("hb_langSelectID(%s)", pszID));

   hb_langSelect( hb_langFind( pszID ) );

   return pszIDOld;
}

const char * hb_langDGetItem( int iIndex )
{
   PHB_LANG lang;

   HB_TRACE(HB_TR_DEBUG, ("hb_langDGetItem(%i)", iIndex));

   lang = hb_vmLang();
   if( lang && iIndex >= 0 && iIndex < HB_LANG_ITEM_MAX_ )
      return ( const char * ) lang->pItemList[ iIndex ];
   else
      return NULL;
}

const char * hb_langID( void )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_langID()"));

   return hb_langDGetItem( HB_LANG_ITEM_BASE_ID + HB_LANG_ITEM_ID_ID );
}

/* NOTE: Caller must free the pointer. */

char * hb_langName( void )
{
   char * pszName;
   PHB_LANG lang;

   lang = hb_vmLang();
   if( lang )
   {
      pszName = ( char * ) hb_xgrab( 128 );
      hb_snprintf( pszName, 128, "Harbour Language: %s %s (%s)",
         hb_langDGetItem( HB_LANG_ITEM_BASE_ID + HB_LANG_ITEM_ID_ID ),
         hb_langDGetItem( HB_LANG_ITEM_BASE_ID + HB_LANG_ITEM_ID_NAME ),
         hb_langDGetItem( HB_LANG_ITEM_BASE_ID + HB_LANG_ITEM_ID_NAMENAT ) );
   }
   else
      pszName = hb_strdup( "Harbour Language: (not installed)" );

   return pszName;
}

/* Compatibility interface */

const char * hb_langDGetErrorDesc( int iIndex )
{
   HB_TRACE(HB_TR_DEBUG, ("hb_langDGetErrorDesc(%i)", iIndex));

   return hb_langDGetItem( HB_LANG_ITEM_BASE_ERRDESC + iIndex );
}

/* Harbour interface */

HB_FUNC( HB_LANGSELECT )
{
   const char * szNewLang;

   hb_retc( hb_langID() );

   szNewLang = hb_parc( 1 );
   if( szNewLang )
      hb_langSelectID( szNewLang );
}

HB_FUNC( HB_LANGNAME )
{
   hb_retc_buffer( hb_langName() );
}

HB_FUNC( HB_LANGERRMSG )
{
   hb_retc_const( hb_langDGetErrorDesc( hb_parnl( 1 ) ) );
}

HB_FUNC( HB_LANGMESSAGE )
{
   hb_retc_const( hb_langDGetItem( hb_parnl( 1 ) ) );
}
