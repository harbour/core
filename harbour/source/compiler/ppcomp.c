/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler C source with real code generation
 *
 * Copyright 2006 Przemyslaw Czerpak < druzus /at/ priv.onet.pl >
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
 * their web site at http://www.gnu.org/).
 *
 */


#include "hbcomp.h"
#include <errno.h>

static void hb_pp_ErrorGen( void * cargo,
                            char * szMsgTable[], char cPrefix, int iErrorCode,
                            const char * szParam1, const char * szParam2 )
{
   /* I do not know why but compiler expect line number 1 bigger then
      real line number */
   if( cPrefix == 'W' )
      hb_compGenWarning( ( HB_COMP_PTR ) cargo, szMsgTable, cPrefix, iErrorCode, szParam1, szParam2 );
   else
      hb_compGenError( ( HB_COMP_PTR ) cargo, szMsgTable, cPrefix, iErrorCode, szParam1, szParam2 );
}

static void hb_pp_PragmaDump( void * cargo, char * pBuffer, ULONG ulSize,
                              int iLine )
{
   PINLINE pInline;

   pInline = hb_compInlineAdd( ( HB_COMP_PTR ) cargo, NULL, iLine );
   pInline->pCode = ( BYTE * ) hb_xgrab( ulSize + 1 );
   memcpy( pInline->pCode, pBuffer, ulSize );
   pInline->pCode[ ulSize ] = '\0';
   pInline->lPCodeSize = ulSize;
}

static void hb_pp_hb_inLine( void * cargo, char * szFunc,
                             char * pBuffer, ULONG ulSize, int iLine )
{
   HB_COMP_DECL = ( HB_COMP_PTR ) cargo;

   if( HB_COMP_PARAM->iLanguage != LANG_C && HB_COMP_PARAM->iLanguage != LANG_OBJ_MODULE )
   {
      hb_compGenError( ( HB_COMP_PTR ) cargo, hb_comp_szErrors, 'F', HB_COMP_ERR_REQUIRES_C, NULL, NULL );
   }
   else
   {
      PINLINE pInline = hb_compInlineAdd( ( HB_COMP_PTR ) cargo,
         hb_compIdentifierNew( HB_COMP_PARAM, szFunc, HB_IDENT_COPY ), iLine );
      pInline->pCode = ( BYTE * ) hb_xgrab( ulSize + 1 );
      memcpy( pInline->pCode, pBuffer, ulSize );
      pInline->pCode[ ulSize ] = '\0';
      pInline->lPCodeSize = ulSize;
   }
}

static BOOL hb_pp_CompilerSwitch( void * cargo, const char * szSwitch,
                                  int iValue )
{
   HB_COMP_DECL = ( HB_COMP_PTR ) cargo;
   BOOL fError = FALSE;
   int i = strlen( szSwitch );

   if( i > 1 && szSwitch[ i - 1 ] - '0' == iValue )
      --i;

   if( i == 1 )
   {
      switch( szSwitch[ 0 ] )
      {
         case 'a':
         case 'A':
            HB_COMP_PARAM->fAutoMemvarAssume = iValue != 0;
            break;

         case 'b':
         case 'B':
            HB_COMP_PARAM->fDebugInfo = iValue != 0;
            break;

         case 'l':
         case 'L':
            HB_COMP_PARAM->fLineNumbers = iValue != 0;
            break;

         case 'n':
         case 'N':
            HB_COMP_PARAM->fStartProc = iValue != 0;
            break;

         case 'p':
         case 'P':
            HB_COMP_PARAM->fPPO = iValue != 0;
            break;

         case 'q':
         case 'Q':
            HB_COMP_PARAM->fQuiet = iValue != 0;
            break;

         case 'v':
         case 'V':
            HB_COMP_PARAM->fForceMemvars = iValue != 0;
            break;

         case 'w':
         case 'W':
            if( iValue >= 0 && iValue <= 3 )
               HB_COMP_PARAM->iWarnings = iValue;
            else
               fError = TRUE;
            break;

         case 'z':
         case 'Z':
            if( iValue )
               HB_COMP_PARAM->supported &= ~HB_COMPFLAG_SHORTCUTS;
            else
               HB_COMP_PARAM->supported |= HB_COMPFLAG_SHORTCUTS;
            break;

         default:
            fError = TRUE;
      }
   }
   else if( i == 2 )
   {
      if( hb_strnicmp( szSwitch, "es", 2 ) == 0 &&
          ( iValue == HB_EXITLEVEL_DEFAULT ||
            iValue == HB_EXITLEVEL_SETEXIT ||
            iValue == HB_EXITLEVEL_DELTARGET ) )
         HB_COMP_PARAM->iExitLevel = iValue;
      else
         fError = TRUE;
   }
   /* xHarbour extension */
   else if( i >= 4 && hb_strnicmp( szSwitch, "TEXTHIDDEN", i ) == 0 &&
            iValue >= 0 && iValue <= 1 )
      HB_COMP_PARAM->iHidden = iValue;
   else
      fError = TRUE;

   return fError;
}


void hb_compInitPP( HB_COMP_DECL, int argc, char * argv[] )
{
   HB_TRACE( HB_TR_DEBUG, ( "hb_compInitPP()" ) );

   if( HB_COMP_PARAM->pLex->pPP )
   {
      hb_pp_init( HB_COMP_PARAM->pLex->pPP, HB_COMP_PARAM->fQuiet,
                  HB_COMP_PARAM->iMaxTransCycles,
                  HB_COMP_PARAM, NULL, NULL,
                  hb_pp_ErrorGen, NULL, hb_pp_PragmaDump,
                  HB_COMP_ISSUPPORTED( HB_COMPFLAG_HB_INLINE ) ?
                  hb_pp_hb_inLine : NULL, hb_pp_CompilerSwitch );

      if( ! HB_COMP_PARAM->szStdCh )
         hb_pp_setStdRules( HB_COMP_PARAM->pLex->pPP );
      else if( HB_COMP_PARAM->szStdCh[ 0 ] > ' ' )
         hb_pp_readRules( HB_COMP_PARAM->pLex->pPP, HB_COMP_PARAM->szStdCh );
      else if( ! HB_COMP_PARAM->fQuiet )
      {
         printf( "Standard command definitions excluded.\n" );
         fflush( stdout );
      }

      hb_pp_initDynDefines( HB_COMP_PARAM->pLex->pPP );

      /* Add /D and /undef: command line or envvar defines */
      hb_compChkDefines( HB_COMP_PARAM, argc, argv );

      /* mark current rules as standard ones */
      hb_pp_setStdBase( HB_COMP_PARAM->pLex->pPP );
   }

   if( HB_COMP_PARAM->pFileName )
   {
      hb_xfree( ( void * ) HB_COMP_PARAM->pFileName );
      HB_COMP_PARAM->pFileName = NULL;
   }
}
