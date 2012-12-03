/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * i18n support in Harbour compiler
 *
 * Copyright 2008 Mindaugas Kavaliauskas <dbtopas.at.dbtopas.lt>
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


#include "hbcomp.h"


/* ============================ I18N ============================ */
static PHB_I18NTABLE hb_compI18nCreate( void )
{
   PHB_I18NTABLE  pI18n;

   pI18n = ( PHB_I18NTABLE ) hb_xgrab( sizeof( HB_I18NTABLE ) );
   pI18n->pString = NULL;
   pI18n->uiCount = 0;
   pI18n->uiAllocated = 0;

   return pI18n;
}

void hb_compI18nFree( HB_COMP_DECL )
{
   PHB_I18NTABLE pI18n = HB_COMP_PARAM->pI18n;

   if( pI18n )
   {
      if( pI18n->pString )
      {
         HB_UINT ui;

         for( ui = 0; ui < pI18n->uiCount; ui++ )
         {
            if( pI18n->pString[ ui ].uiPosCount )
               hb_xfree( pI18n->pString[ ui ].pPosLst );
         }
         hb_xfree( pI18n->pString );
      }
      hb_xfree( pI18n );
      HB_COMP_PARAM->pI18n = NULL;
   }
}

static int hb_compI18nCompare( PHB_I18NSTRING pString, const char * pText, const char * pContext )
{
   int  i;

   i = pString->szText == pText ? 0 : pString->szText > pText ? 1 : -1;
   if( i == 0 && pString->szContext != pContext )
      i = pString->szContext > pContext ? 1 : -1;

   return i;
}

static PHB_I18NSTRING hb_compI18nAddSingle( HB_COMP_DECL, const char * szText, const char * szContext,
                                            const char * szModule, HB_UINT uiLine )
{
   PHB_I18NTABLE  pI18n;
   PHB_I18NSTRING pString;
   HB_UINT        uiLeft, uiRight, uiMiddle;
   int            iCompare;

   if( ! HB_COMP_PARAM->pI18n )
      HB_COMP_PARAM->pI18n = hb_compI18nCreate();
   pI18n = HB_COMP_PARAM->pI18n;

   szText = hb_compIdentifierNew( HB_COMP_PARAM, szText, HB_IDENT_COPY );
   if( szContext )
      szContext = hb_compIdentifierNew( HB_COMP_PARAM, szContext, HB_IDENT_COPY );

   if( pI18n->uiCount >= pI18n->uiAllocated )
   {
      if( pI18n->pString )
      {
         pI18n->uiAllocated += 32;
         pI18n->pString = ( PHB_I18NSTRING ) hb_xrealloc( pI18n->pString, sizeof( HB_I18NSTRING )
                                                                          * pI18n->uiAllocated );
      }
      else
      {
         pI18n->pString = ( PHB_I18NSTRING ) hb_xgrab( sizeof( HB_I18NSTRING ) * 32 );
         pI18n->uiAllocated = 32;
      }
   }

   uiLeft = 0;
   uiRight = pI18n->uiCount;

   while( uiLeft < uiRight )
   {
      uiMiddle = ( uiLeft + uiRight ) >> 1;

      iCompare = hb_compI18nCompare( &pI18n->pString[ uiMiddle ], szText, szContext );

      if( iCompare == 0 )
      {
         pString = &pI18n->pString[ uiMiddle ];

         if( pString->uiPosCount )
         {
            pString->pPosLst = ( PHB_I18NPOS ) hb_xrealloc( pString->pPosLst, ( pString->uiPosCount + 1 ) * sizeof( HB_I18NPOS ) );
            pString->pPosLst[ pString->uiPosCount ].uiLine = uiLine;
            pString->pPosLst[ pString->uiPosCount ].szFile = szModule;
            pString->uiPosCount++;
         }
         else
         {
            pString->pPosLst = ( PHB_I18NPOS ) hb_xgrab( sizeof( HB_I18NPOS ) );
            pString->pPosLst[ 0 ].uiLine = uiLine;
            pString->pPosLst[ 0 ].szFile = szModule;
            pString->uiPosCount = 1;
         }
         return pString;
      }
      else if( iCompare < 0 )
         uiLeft = uiMiddle + 1;
      else
         uiRight = uiMiddle;
   }

   memmove( &pI18n->pString[ uiLeft + 1 ], &pI18n->pString[ uiLeft ],
            ( pI18n->uiCount - uiLeft ) * sizeof( HB_I18NSTRING ) );

   pString = &pI18n->pString[ uiLeft ];
   pString->szText = szText;
   pString->szContext = szContext;
   pString->pPos.uiLine = uiLine;
   pString->pPos.szFile = szModule;
   pString->uiPosCount = 0;
   pString->uiPlurals = 0;

   pI18n->uiCount++;

   return pString;
}

void hb_compI18nAdd( HB_COMP_DECL, const char * szText, const char * szContext,
                     const char * szModule, HB_UINT uiLine )
{
   hb_compI18nAddSingle( HB_COMP_PARAM, szText, szContext, szModule, uiLine );
}

void hb_compI18nAddPlural( HB_COMP_DECL, const char ** szTexts, HB_ULONG ulCount, const char * szContext, const char * szModule, HB_UINT uiLine )
{
   PHB_I18NSTRING pString;

   pString = hb_compI18nAddSingle( HB_COMP_PARAM, szTexts[ 0 ], szContext, szModule, uiLine );
   if( ulCount == 1 )
   {
      /* set the same string as plural form to mark it as plural text */
      if( ! pString->uiPlurals )
      {
         pString->szPlurals[ 0 ] = pString->szText;
         pString->uiPlurals = 1;
      }
   }
   else
   {
      HB_ULONG ul, ulPlural;

      for( ul = 1; ul < ulCount && pString->uiPlurals < HB_I18N_PLURAL_MAX; ++ul )
      {
         const char * szText = hb_compIdentifierNew( HB_COMP_PARAM, szTexts[ ul ], HB_IDENT_COPY );

         for( ulPlural = 0; ulPlural < pString->uiPlurals; ++ulPlural )
         {
            if( pString->szPlurals[ ulPlural ] == szText )
            {
               szText = NULL;
               break;
            }
         }
         if( szText )
            pString->szPlurals[ pString->uiPlurals++ ] = szText;
      }
   }
}

static void hb_compI18nEscapeString( FILE * file, const char * szText )
{
   while( *szText )
   {
      if( ( HB_UCHAR ) *szText < ' ' )
      {
         if( *szText == '\t' )
            fprintf( file, "\\t" );
         else if( *szText == '\n' )
            fprintf( file, "\\n" );
         else if( *szText == '\r' )
            fprintf( file, "\\r" );
         else if( ( ( HB_UCHAR ) szText[ 1 ] >= '0' && ( HB_UCHAR ) szText[ 1 ] <= '9' ) ||
                  ( ( HB_UCHAR ) szText[ 1 ] >= 'A' && ( HB_UCHAR ) szText[ 1 ] <= 'F' ) ||
                  ( ( HB_UCHAR ) szText[ 1 ] >= 'a' && ( HB_UCHAR ) szText[ 1 ] <= 'f' ) )
            fprintf( file, "\\%03o", *szText );
         else
            fprintf( file, "\\x%02X", *szText );
      }
      else if( *szText == '"' )
         fprintf( file, "\\\"" );
      else if( *szText == '\\' )
         fprintf( file, "\\\\" );
      else
         fprintf( file, "%c", *szText );

      szText++;
   }
}

static char * hb_compI18nFileName( char * szBuffer, const char * szFileName )
{
   HB_UINT ui = 0;
   char ch;

   do
   {
      if( ui == HB_PATH_MAX - 1 )
         ch = '\0';
      else
      {
         ch = szFileName[ ui ];
         if( ch == '\\' )
            ch = '/';
      }
      szBuffer[ ui++ ] = ch;
   }
   while( ch );

   return szBuffer;
}

HB_BOOL hb_compI18nSave( HB_COMP_DECL, HB_BOOL fFinal )
{
   PHB_I18NTABLE    pI18n;
   PHB_I18NSTRING   pString;
   HB_FNAME         FileName;
   char             szFileName[ HB_PATH_MAX ];
   char*            szText;
   HB_UINT          uiIndex, uiLine;
   FILE*            file;

   pI18n = HB_COMP_PARAM->pI18n;
   if( ! pI18n )
      return HB_FALSE;

   FileName.szPath = FileName.szName = FileName.szExtension =
   FileName.szDrive = NULL;

   if( HB_COMP_PARAM->pOutPath )
   {
      FileName.szDrive = HB_COMP_PARAM->pOutPath->szDrive;
      FileName.szPath = HB_COMP_PARAM->pOutPath->szPath;
   }

   if( HB_COMP_PARAM->pI18nFileName )
   {
      if( HB_COMP_PARAM->pI18nFileName->szName )
         FileName.szName = HB_COMP_PARAM->pI18nFileName->szName;

      if( HB_COMP_PARAM->pI18nFileName->szExtension )
         FileName.szExtension = HB_COMP_PARAM->pI18nFileName->szExtension;

      if( HB_COMP_PARAM->pI18nFileName->szPath )
      {
         FileName.szDrive = HB_COMP_PARAM->pI18nFileName->szDrive;
         FileName.szPath = HB_COMP_PARAM->pI18nFileName->szPath;
      }
   }

   if( ! FileName.szName )
      FileName.szName = HB_COMP_PARAM->pFileName->szName;
   else if( ! fFinal )
      /* The exact file name was given generate single .pot file for
       * all compiled .prg files in final phase.
       */
      return HB_FALSE;

   if( ! FileName.szExtension )
      FileName.szExtension = ".pot";

   hb_fsFNameMerge( szFileName, &FileName );

   file = hb_fopen( szFileName, "w" );

   if( ! file )
   {
      hb_compGenError( HB_COMP_PARAM, hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return HB_FALSE;
   }

   szText = hb_verHarbour();
   fprintf( file, "#\n# This file is generated by %s\n#\n\n", szText );
   hb_xfree( szText );

   for( uiIndex = 0; uiIndex < pI18n->uiCount; uiIndex++ )
   {
      pString = &pI18n->pString[ uiIndex ];

      fprintf( file, "#: %s:%d", hb_compI18nFileName( szFileName, pString->pPos.szFile ),
                                 pString->pPos.uiLine );

      for( uiLine = 0; uiLine < pString->uiPosCount; ++uiLine )
         fprintf( file, " %s:%d", hb_compI18nFileName( szFileName, pString->pPosLst[ uiLine ].szFile ),
                                  pString->pPosLst[ uiLine ].uiLine );

      fprintf( file, "\n#, c-format\n" );

      if( pString->szContext )
      {
         fprintf( file, "msgctxt \"" );
         hb_compI18nEscapeString( file, pString->szContext );
         fprintf( file, "\"\n" );
      }

      fprintf( file, "msgid \"" );
      hb_compI18nEscapeString( file, pString->szText );
      for( uiLine = 0; uiLine < pString->uiPlurals; ++uiLine )
      {
         if( uiLine == 0 )
            fprintf( file, "\"\nmsgid_plural \"" );
         else
            fprintf( file, "\"\nmsgid_plural%d \"", uiLine + 1 );
         hb_compI18nEscapeString( file, pString->szPlurals[ uiLine ] );
      }
      fprintf( file, "\"\nmsgstr%s \"\"\n\n", pString->uiPlurals ? "[0]" : "" );
   }

   fclose( file );
   return HB_TRUE;
}
