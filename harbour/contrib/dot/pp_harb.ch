/*
 * xBaseScript Project source code:
 * Pre-Processor / Dot prompt environment / Script Interpreter
 *
 * Copyright 2000-2001 Ron Pinkas <ronpinkas@profit-master.com>
 * www - http://www.xBaseScript.com
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
 */

#ifdef __HARBOUR__

#include "hbclass.ch"

//----------------------------------------------------------------------------//
CLASS  TInterpreter

   DATA cText
   DATA acPPed
   DATA cPPed
   DATA aCompiledProcs
   DATA aInitExit
   DATA nProcs

   METHOD New()              INLINE ( ::nProcs := 0, ::cText := "", ::acPPed := {}, ::aCompiledProcs := {}, ::aInitExit := { {}, {} }, Self )

   METHOD AddLine( cLine )   INLINE ( ::nProcs := 0, ::acPPed := {}, ::cText += ( cLine + Chr(10) ) )
   METHOD SetScript( cText ) INLINE ( ::nProcs := 0, ::acPPed := {}, ::cText := cText )
   METHOD GetPPO()           INLINE ( ::cPPed )

   METHOD Compile()
   METHOD Run()
   METHOD RunFile( cFile, aParams, cPPOExt, bBlanks ) INLINE PP_Run( cFile, aParams, cPPOExt, bBlanks )

   METHOD ClearRules()       INLINE PP_ResetRules()
   METHOD InitStdRules()     INLINE PP_InitStd()
   METHOD LoadClass()        INLINE PP_LoadClass()
   METHOD LoadFiveWin()      INLINE PP_LoadFw()
ENDCLASS

//----------------------------------------------------------------------------//
METHOD Run( p1, p2, p3, p4, p5, p6, p7, p8, p9 )

   LOCAL aParams := HB_aParams(), xRet

   IF ::nProcs == 0
      ::Compile()
   ENDIF

   IF ::nProcs > 0
      xRet := PP_Exec( ::aCompiledProcs, ::aInitExit, ::nProcs, aParams )
   ENDIF

RETURN xRet

//----------------------------------------------------------------------------//
METHOD Compile()

   LOCAL nLine, nLines, nProcId := 0

   IF Len( ::acPPed ) == 0
      PP_InitStd()
      PP_LoadRun()
      ::cPPed          := PP_PreProText( ::cText, ::acPPed )
      ::aCompiledProcs := {}
      ::aInitExit      := { {}, {} }
   ENDIF

   IF Len( ::aCompiledProcs ) == 0
      ErrorBlock( {|oErr| RP_Comp_Err( oErr, ::acPPed[nLine], nLine ) } )

      nLines := Len( ::acPPed )
      FOR nLine := 1 TO nLines
         PP_CompileLine( ::acPPed[nLine], nLine, ::aCompiledProcs, ::aInitExit, @nProcId )
      NEXT
   ENDIF

   ::nProcs := nProcId

RETURN nProcId > 0

//----------------------------------------------------------------------------//

#ifdef USE_C_BOOST

#ifdef __XHARBOUR__
   #pragma BEGINDUMP
      #define __XHARBOUR__
   #pragma ENDDUMP
#endif

#pragma BEGINDUMP

#include <ctype.h>
#include "hbapi.h"
#include "hbstack.h"
#include "hbapierr.h"
#include "hbapiitm.h"
#include "hbvm.h"
#include "hboo.ch"

#ifdef __XHARBOUR__
  #include "hbfast.h"
#endif

static BOOL s_bArrayPrefix = FALSE;

//----------------------------------------------------------------------------//
static HB_FUNC( SETARRAYPREFIX )
{
   PHB_ITEM pbArrayPrefix = hb_param( 1, HB_IT_LOGICAL );

   if( pbArrayPrefix != NULL )
   {
      s_bArrayPrefix = pbArrayPrefix->item.asLogical.value;
   }
}

//----------------------------------------------------------------------------//
static HB_FUNC( NEXTTOKEN )
{
   PHB_ITEM pLine       = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pDontRecord = hb_param( 2, HB_IT_LOGICAL );
   char *sLine, *pTmp;
   char sReturn[2048];
   char s2[3];
   BOOL lDontRecord;
   size_t Counter, nLen;

   if( pLine == NULL || pLine->item.asString.length == 0 )
   {
      hb_ret();
      return;
   }

   sLine = pLine->item.asString.value;
   nLen = pLine->item.asString.length;

   //printf( "\nProcessing: '%s'\n", sLine );

   if( pDontRecord == NULL )
   {
      lDontRecord = FALSE;
   }
   else
   {
      lDontRecord = pDontRecord->item.asLogical.value;
   }

   // *** To be removed after final testing !!!
   while( sLine[0] == ' ' )
   {
      sLine++; nLen--;
   }

   sReturn[0] = '\0';
   s2[2]      = '\0';

   if( nLen >= 2 )
   {
      s2[0] = sLine[0];
      s2[1] = sLine[1];

      if( strstr( "++\\--\\->\\:=\\==\\!=\\<>\\>=\\<=\\+=\\-=\\*=\\^=\\**\\/=\\%=\\??", (char*) s2 ) )
      {
         sReturn[0] = s2[0];
         sReturn[1] = s2[1];
         sReturn[2] = '\0';

         goto Done;
      }
      else if( s2[0] == '[' && s2[1] == '[' )
      {
         pTmp = strstr( sLine + 2, "]]" );
         if( pTmp == NULL )
         {
            sReturn[0] = '['; // Clipper does NOT consider '[[' a single token
            sReturn[1] = '\0';
         }
         else
         {
            strncpy( sReturn, sLine, ( pTmp - sLine ) + 2 );
            sReturn[( pTmp - sLine ) + 2] = '\0';
         }

         goto Done;
      }
   }

   if( isalpha( sLine[0] ) || sLine[0] == '_' )
   {
      sReturn[0] = sLine[0];
      Counter = 1;
      while( isalnum( sLine[Counter] ) || sLine[Counter] == '_' || sLine[Counter] == '\\' )
      {
         sReturn[Counter] = sLine[Counter];
         Counter++;
      }

      sReturn[Counter] = '\0';
      goto Done;
   }
   else if( isdigit( sLine[0] ) )
   {
      sReturn[0] = sLine[0];
      Counter = 1;
      while( isdigit( sLine[Counter] ) || sLine[Counter] == '\\' )
      {
         sReturn[Counter] = sLine[Counter];
         Counter++;
      }

      // Consume the point (and subsequent digits) only if digits follow...
      if( sLine[Counter] == '.' && isdigit( sLine[Counter + 1] ) )
      {
         sReturn[Counter] = '.';
         Counter++;
         sReturn[Counter] = sLine[Counter];
         Counter++;
         while( isdigit( sLine[Counter] ) || sLine[Counter] == '\\' )
         {
            sReturn[Counter] = sLine[Counter];
            Counter++;
         }
      }

      // Either way we are done.
      sReturn[Counter] = '\0';
      goto Done;
   }
   else if( sLine[0] == '.' && isdigit( sLine[1] ) )
   {
      sReturn[0] = '.';
      sReturn[1] = sLine[1];
      Counter = 2;
      while( isdigit( sLine[Counter] ) )
      {
         sReturn[Counter] = sLine[Counter];
         Counter++;
      }

      sReturn[Counter] = '\0';
      goto Done;
   }
   else if( sLine[0] == '.' )
   {
      if( nLen >= 5 && sLine[4] == '.' )
      {
         if( toupper( sLine[1] ) == 'A' && toupper( sLine[2] ) == 'N' && toupper( sLine[3] ) == 'D' )
         {
            sReturn[0] = '.';
            sReturn[1] = 'A';
            sReturn[2] = 'N';
            sReturn[3] = 'D';
            sReturn[4] = '.';
            sReturn[5] = '\0';

            goto Done;
         }
         else if( toupper( sLine[1] ) == 'N' && toupper( sLine[2] ) == 'O' && toupper( sLine[3] ) == 'T' )
         {
            sReturn[0] = '!';
            sReturn[1] = '\0';

            /* Skip the unaccounted letters ( .NOT. <-> ! ) */
            sLine += 4;

            goto Done;
         }
      }

      if( nLen >= 4 && sLine[3] == '.' && toupper( sLine[1] ) == 'O' && toupper( sLine[2] ) == 'R' )
      {
         sReturn[0] = '.';
         sReturn[1] = 'O';
         sReturn[2] = 'R';
         sReturn[3] = '.';
         sReturn[4] = '\0';

         goto Done;
      }

      if( nLen >= 3 && sLine[2] == '.' )
      {
         if( toupper( sLine[1] ) == 'T' )
         {
            sReturn[0] = '.';
            sReturn[1] = 'T';
            sReturn[2] = '.';
            sReturn[3] = '\0';

            goto Done;
         }
         else if( toupper( sLine[1] ) == 'F' )
         {
            sReturn[0] = '.';
            sReturn[1] = 'F';
            sReturn[2] = '.';
            sReturn[3] = '\0';

            goto Done;
         }
      }

      sReturn[0] = '.';
      sReturn[1] = '\0';

      goto Done;
   }
   else if( sLine[0] == '"' )
   {
      pTmp = strchr( sLine + 1, '"' );
      if( pTmp == NULL )
      {
         sReturn[0] = '"';
         sReturn[1] = '\0';
      }
      else
      {
         strncpy( sReturn, sLine, ( pTmp - sLine ) + 1 );
         sReturn[( pTmp - sLine ) + 1] = '\0';
      }

      goto Done;
   }
   else if( sLine[0] == '\'' )
   {
      pTmp = strchr( sLine + 1, '\'' );
      if( pTmp == NULL )
      {
         sReturn[0] = '\'';
         sReturn[1] = '\0';
      }
      else
      {
         strncpy( sReturn, sLine, ( pTmp - sLine ) + 1 );
         sReturn[( pTmp - sLine ) + 1] = '\0';

         if( strchr( sReturn, '"' ) == NULL )
         {
            sReturn[0] = '"';
            sReturn[( pTmp - sLine )] = '"';
         }
      }

      goto Done;
   }
   else if( sLine[0] == '[' )
   {
      if( s_bArrayPrefix )
      {
         sReturn[0] = '[';
         sReturn[1] = '\0';
      }
      else
      {
         pTmp = strchr( sLine + 1, ']' );
         if( pTmp == NULL )
         {
            sReturn[0] = '[';
            sReturn[1] = '\0';
         }
         else
         {
            strncpy( sReturn, sLine, ( pTmp - sLine ) + 1 );
            sReturn[( pTmp - sLine ) + 1] = '\0';

            if( strchr( sReturn, '"' ) == NULL )
            {
               sReturn[0] = '"';
               sReturn[( pTmp - sLine )] = '"';
            }
            else if( strchr( sReturn, '\'' ) == NULL )
            {
               sReturn[0] = '\'';
               sReturn[( pTmp - sLine )] = '\'';
            }
         }
      }

      goto Done;
   }
   else if( sLine[0] == '\\' )
   {
      sReturn[0] = '\\';
      sReturn[1] = sLine[1];
      sReturn[2] = '\0';

      goto Done;
   }
   else if ( strchr( "+-*/:=^!&()[]{}@,|<>#%?$~", sLine[0] ) )
   {
      sReturn[0] = sLine[0];
      sReturn[1] = '\0';

      goto Done;
   }
   else
   {
      printf( "\nUnexpected case: %s\n", sLine );
      getchar();
      sReturn[0] = sLine[0];
      sReturn[1] = '\0';
   }

 Done:

   sLine += ( nLen = strlen( sReturn ) );

   if( ! lDontRecord )
   {
      if( sReturn[0] == '.' && nLen > 1 && sReturn[nLen - 1] == '.' )
      {
         s_bArrayPrefix = FALSE;
      }
      else
      {
         s_bArrayPrefix = ( isalnum( sReturn[0] ) || strchr( "])}._", sReturn[0] ) );
      }
   }

   while( sLine[0] == ' ' )
   {
      sReturn[nLen] = sLine[0];
      sLine++; nLen++;
   }
   sReturn[nLen] = '\0';

   if( ISBYREF( 1 ) )
   {
      if( sLine[0] == '\0' )
      {
         hb_itemPutC( pLine, NULL );
      }
      else
      {
         hb_itemPutCPtr( pLine, hb_strdup( sLine ), strlen( sLine ) );
      }
      //printf( "\nToken: '%s' value: '%s'\n", sReturn, pLine->item.asString.value );
   }
   else
   {
      //printf( "\nToken: '%s' ***value: '%s'\n", sReturn, pLine->item.asString.value );
   }

   //printf( "\nToken: '%s'\n", sReturn );

   hb_retclen( sReturn, nLen );
}

//----------------------------------------------------------------------------//
static HB_FUNC( NEXTIDENTIFIER )
{
   PHB_ITEM pLine    = hb_param( 1, HB_IT_STRING );
   PHB_ITEM pSkipped = hb_param( 2, HB_IT_ANY );
   char *sLine;
   char cChar, cLastChar = ' ';
   size_t nAt, nLen;
   int nStart = -1;

   if( pLine == NULL || pLine->item.asString.length == 0 )
   {
      hb_ret();
   }

   sLine = pLine->item.asString.value;
   nLen  = pLine->item.asString.length;

   for( nAt = 0; nAt < nLen; nAt++ )
   {
       cChar = sLine[nAt];

       if( strchr( " ,([{|^*/+-=!#<>:&$", cChar ) )
       {
          if( nStart >= 0 )
          {
             break;
          }
          continue; // No need to record cLastChar
       }
       else if( strchr( ")]}", cChar ) )
       {
          if( nStart >= 0 )
          {
             break;
          }
       }
       else if( strchr( "\"'", cChar ) )
       {
          while( ( nAt < nLen ) && ( sLine[++nAt] != cChar ) );

          continue; // No need to record cLastChar
       }
       else if( cChar == '[' )
       {
          if( ! ( isalnum( cLastChar ) || strchr( "])}_.", cLastChar ) ) )
          {
             while( nAt < nLen && sLine[++nAt] != ']' );
          }
          cLastChar = ']';

          continue; // Recorded cLastChar
       }
       else if( cChar == '.' )
       {
          if( nStart >= 0 )
          {
             break;
          }
          else if( toupper( sLine[nAt + 1] ) == 'T' && sLine[nAt + 2] == '.' )
          {
             nAt += 2;
             continue;
          }
          else if( toupper( sLine[nAt + 1] ) == 'F' && sLine[nAt + 2] == '.' )
          {
             nAt += 2;
             continue;
          }
          else if( toupper( sLine[nAt + 1] ) == 'O' && toupper( sLine[nAt + 2] ) == 'R' && sLine[nAt + 3] == '.' )
          {
             nAt += 3;
             continue;
          }
          else if( toupper( sLine[nAt + 1] ) == 'A' && toupper( sLine[nAt + 2] ) == 'N' && toupper( sLine[nAt + 3] ) == 'D' && sLine[nAt + 4] == '.' )
          {
             nAt += 4;
             continue;
          }
          else if( toupper( sLine[nAt + 1] ) == 'N' && toupper( sLine[nAt + 2] ) == 'O' && toupper( sLine[nAt + 3] ) == 'T' && sLine[nAt + 4] == '.' )
          {
             nAt += 4;
             continue;
          }
       }
       else if( nStart == -1 && ( isalpha( cChar ) || cChar == '_' ) )
       {
          nStart = nAt;
       }

       cLastChar = cChar;
    }

    if( ISBYREF( 2 ) )
    {
       if( nStart <= 0 )
       {
          hb_itemPutC( pSkipped, NULL );
          //printf( "\nNot Skipped: \n" );
       }
       else
       {
          hb_itemPutCL( pSkipped, sLine, nStart );
          //printf( "\nSkipped: '%s'\n", pSkipped->item.asString.value );
       }
    }

    if( nStart >= 0 )
    {
       char *sIdentifier = (char *) hb_xgrab( ( nAt - nStart ) + 1 );

       strncpy( sIdentifier, sLine + nStart, ( nAt - nStart ) );
       sIdentifier[nAt - nStart] = '\0';

       //printf( "\nLine: '%s' nStart: %i nAt: %i sIdentifier: '%s'\n", sLine, nStart, nAt, sIdentifier );

       if( ISBYREF( 1 ) )
       {
          hb_itemPutCPtr( pLine, hb_strdup( sLine + nAt ), strlen( sLine + nAt ) );
       }

       //printf( "\nIdentifier: '%s'\n", sIdentifier );

       #ifdef __XHARBOUR__
          hb_retcAdopt( sIdentifier );
       #else
          hb_retc( sIdentifier );
          hb_xfree( sIdentifier );
       #endif
    }
    else
    {
       hb_ret();
    }
}

//----------------------------------------------------------------------------//
HB_FUNC( EXTRACTLEADINGWS )
{
   PHB_ITEM pLine = hb_param( 1, HB_IT_STRING );
   size_t iLen, i = 0;
   char *pTmp;

   if( pLine == NULL )
   {
      hb_retclen( "", 0 );
      return;
   }

   iLen = pLine->item.asString.length;

   while( pLine->item.asString.value[i] == ' ' )
   {
      i++;
   }

   if( i > 0 )
   {
      if( HB_IS_BYREF( hb_stackItemFromBase( 1 ) ) )
      {
         hb_itemPutCPtr( pLine, hb_strdup( pLine->item.asString.value + i ), iLen - i );
      }
   }

   pTmp = ( char * ) hb_xgrab( i + 1 );
   memset( pTmp, ' ', i );

   if( HB_IS_BYREF( hb_stackItemFromBase( 2 ) ) )
   {
      PHB_ITEM pWS = hb_itemUnRef( hb_stackItemFromBase( 2 ) );
      hb_itemPutCL( pWS, pTmp, i );
   }

   #ifdef __XHARBOUR__
      hb_retclenAdopt( pTmp, i );
   #else
      hb_retclen( pTmp, i );
      hb_xfree( pTmp );
   #endif
}

//----------------------------------------------------------------------------//
HB_FUNC( DROPTRAILINGWS )
{
   PHB_ITEM pLine = hb_param( 1, HB_IT_STRING );
   char *pString;
   size_t iLen, i;

   if( pLine == NULL )
   {
      hb_retclen( "", 0 );
      return;
   }

   pString = hb_itemGetC( pLine );
   iLen    = hb_itemGetCLen( pLine );

   i = iLen - 1;

   while( pString[i] == ' ' )
   {
      i--;
   }

   if( ++i < iLen )
   {
      pString[i] = '\0';
   }

   if( HB_IS_BYREF( hb_stackItemFromBase( 1 ) ) )
   {
      hb_itemPutCL( pLine, pString, i );
   }

   if( HB_IS_BYREF( hb_stackItemFromBase( 2 ) ) )
   {
      PHB_ITEM pWS = hb_itemUnRef( hb_stackItemFromBase( 2 ) );
      char *pTmp = ( char * ) hb_xgrab( iLen - i + 1 );

      memset( pTmp, ' ', iLen - i );
      hb_itemPutCPtr( pWS, pTmp, iLen - i );
   }

   #ifdef __XHARBOUR__
      hb_retclenAdopt( pString, i );
   #else
      hb_retclen( pString, i );
      hb_xfree( pString );
   #endif
}

//----------------------------------------------------------------------------//
HB_FUNC( DROPEXTRATRAILINGWS )
{
   PHB_ITEM pLine = hb_param( 1, HB_IT_STRING );
   char *pString;
   size_t iLen, i;

   if( pLine == NULL )
   {
      hb_retclen( "", 0 );
      return;
   }

   pString = hb_itemGetC( pLine );
   iLen    = hb_itemGetCLen( pLine );

   i = iLen - 1;

   while( i > 1 && pString[i] == ' ' && pString[i - 1] == ' ' )
   {
      i--;
   }

   if( ++i < iLen )
   {
      pString[i] = '\0';
   }

   if( HB_IS_BYREF( hb_stackItemFromBase( 1 ) ) )
   {
      hb_itemPutCL( pLine, pString, i );
   }

   #ifdef __XHARBOUR__
      hb_retclenAdopt( pString, i );
   #else
      hb_retclen( pString, i );
      hb_xfree( pString );
   #endif
}

#pragma ENDDUMP

#endif

//----------------------------------------------------------------------------//
#ifdef WIN

   #pragma BEGINDUMP

   #include <Windows.h>

   HB_FUNC( MESSAGEBOX )
   {
       hb_retni( MessageBox( ( HWND ) hb_parnl( 1 ), hb_parc( 2 ), hb_parc( 3 ), hb_parni( 4 ) ) );
   }

   #pragma ENDDUMP

#endif

//----------------------------------------------------------------------------//
STATIC FUNCTION InitFWRules()

   /* Defines */
   aAdd( aDefRules, { '_FIVEWIN_CH' ,  , .T. } )
   aAdd( aDefRules, { 'FWCOPYRIGHT' ,  , .T. } )
   aAdd( aDefRules, { 'FWVERSION' ,  , .T. } )
   aAdd( aDefRules, { 'FWDESCRIPTION' ,  , .T. } )
   aAdd( aDefRules, { 'Browse' ,  , .T. } )
   aAdd( aDefRules, { '_DIALOG_CH' ,  , .T. } )
   aAdd( aDefRules, { '_FONT_CH' ,  , .T. } )
   aAdd( aDefRules, { 'LF_HEIGHT' ,  , .T. } )
   aAdd( aDefRules, { 'LF_WIDTH' ,  , .T. } )
   aAdd( aDefRules, { 'LF_ESCAPEMENT' ,  , .T. } )
   aAdd( aDefRules, { 'LF_ORIENTATION' ,  , .T. } )
   aAdd( aDefRules, { 'LF_WEIGHT' ,  , .T. } )
   aAdd( aDefRules, { 'LF_ITALIC' ,  , .T. } )
   aAdd( aDefRules, { 'LF_UNDERLINE' ,  , .T. } )
   aAdd( aDefRules, { 'LF_STRIKEOUT' ,  , .T. } )
   aAdd( aDefRules, { 'LF_CHARSET' ,  , .T. } )
   aAdd( aDefRules, { 'LF_OUTPRECISION' ,  , .T. } )
   aAdd( aDefRules, { 'LF_CLIPPRECISION' ,  , .T. } )
   aAdd( aDefRules, { 'LF_QUALITY' ,  , .T. } )
   aAdd( aDefRules, { 'LF_PITCHANDFAMILY' ,  , .T. } )
   aAdd( aDefRules, { 'LF_FACENAME' ,  , .T. } )
   aAdd( aDefRules, { '_INI_CH' ,  , .T. } )
   aAdd( aDefRules, { '_MENU_CH' ,  , .T. } )
   aAdd( aDefRules, { '_PRINT_CH' ,  , .T. } )
   aAdd( aDefRules, { '_COLORS_CH' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_BLACK' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_BLUE' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_GREEN' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_CYAN' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_RED' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_MAGENTA' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_BROWN' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_HGRAY' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_LIGHTGRAY' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_GRAY' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_HBLUE' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_HGREEN' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_HCYAN' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_HRED' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_HMAGENTA' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_YELLOW' ,  , .T. } )
   aAdd( aDefRules, { 'CLR_WHITE' ,  , .T. } )
   aAdd( aDefRules, { '_DLL_CH' ,  , .T. } )
   aAdd( aDefRules, { '_C_TYPES' ,  , .T. } )
   aAdd( aDefRules, { 'VOID' ,  , .T. } )
   aAdd( aDefRules, { 'BYTE' ,  , .T. } )
   aAdd( aDefRules, { 'CHAR' ,  , .T. } )
   aAdd( aDefRules, { 'WORD' ,  , .T. } )
   aAdd( aDefRules, { '_INT' ,  , .T. } )
   aAdd( aDefRules, { 'BOOL' ,  , .T. } )
   aAdd( aDefRules, { 'HDC' ,  , .T. } )
   aAdd( aDefRules, { 'LONG' ,  , .T. } )
   aAdd( aDefRules, { 'STRING' ,  , .T. } )
   aAdd( aDefRules, { 'LPSTR' ,  , .T. } )
   aAdd( aDefRules, { 'PTR' ,  , .T. } )
   aAdd( aDefRules, { '_DOUBLE' ,  , .T. } )
   aAdd( aDefRules, { 'DWORD' ,  , .T. } )
   aAdd( aDefRules, { '_FOLDER_CH' ,  , .T. } )
   aAdd( aDefRules, { '_OBJECTS_CH' ,  , .T. } )
   aAdd( aDefRules, { '_ODBC_CH' ,  , .T. } )
   aAdd( aDefRules, { '_DDE_CH' ,  , .T. } )
   aAdd( aDefRules, { 'WM_DDE_FIRST' ,  , .T. } )
   aAdd( aDefRules, { 'WM_DDE_INITIATE' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'WM_DDE_TERMINATE' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'WM_DDE_ADVISE' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'WM_DDE_UNADVISE' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'WM_DDE_ACK' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'WM_DDE_DATA' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'WM_DDE_REQUEST' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'WM_DDE_POKE' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'WM_DDE_EXECUTE' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'WM_DDE_LAST' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { '_VIDEO_CH' ,  , .T. } )
   aAdd( aDefRules, { 'VK_LBUTTON' ,  , .T. } )
   aAdd( aDefRules, { 'VK_RBUTTON' ,  , .T. } )
   aAdd( aDefRules, { 'VK_CANCEL' ,  , .T. } )
   aAdd( aDefRules, { 'VK_MBUTTON' ,  , .T. } )
   aAdd( aDefRules, { 'VK_BACK' ,  , .T. } )
   aAdd( aDefRules, { 'VK_TAB' ,  , .T. } )
   aAdd( aDefRules, { 'VK_CLEAR' ,  , .T. } )
   aAdd( aDefRules, { 'VK_RETURN' ,  , .T. } )
   aAdd( aDefRules, { 'VK_SHIFT' ,  , .T. } )
   aAdd( aDefRules, { 'VK_CONTROL' ,  , .T. } )
   aAdd( aDefRules, { 'VK_MENU' ,  , .T. } )
   aAdd( aDefRules, { 'VK_PAUSE' ,  , .T. } )
   aAdd( aDefRules, { 'VK_CAPITAL' ,  , .T. } )
   aAdd( aDefRules, { 'VK_ESCAPE' ,  , .T. } )
   aAdd( aDefRules, { 'VK_SPACE' ,  , .T. } )
   aAdd( aDefRules, { 'VK_PRIOR' ,  , .T. } )
   aAdd( aDefRules, { 'VK_NEXT' ,  , .T. } )
   aAdd( aDefRules, { 'VK_END' ,  , .T. } )
   aAdd( aDefRules, { 'VK_HOME' ,  , .T. } )
   aAdd( aDefRules, { 'VK_LEFT' ,  , .T. } )
   aAdd( aDefRules, { 'VK_UP' ,  , .T. } )
   aAdd( aDefRules, { 'VK_RIGHT' ,  , .T. } )
   aAdd( aDefRules, { 'VK_DOWN' ,  , .T. } )
   aAdd( aDefRules, { 'VK_SELECT' ,  , .T. } )
   aAdd( aDefRules, { 'VK_PRINT' ,  , .T. } )
   aAdd( aDefRules, { 'VK_EXECUTE' ,  , .T. } )
   aAdd( aDefRules, { 'VK_SNAPSHOT' ,  , .T. } )
   aAdd( aDefRules, { 'VK_INSERT' ,  , .T. } )
   aAdd( aDefRules, { 'VK_DELETE' ,  , .T. } )
   aAdd( aDefRules, { 'VK_HELP' ,  , .T. } )
   aAdd( aDefRules, { 'VK_NUMPAD0' ,  , .T. } )
   aAdd( aDefRules, { 'VK_NUMPAD1' ,  , .T. } )
   aAdd( aDefRules, { 'VK_NUMPAD2' ,  , .T. } )
   aAdd( aDefRules, { 'VK_NUMPAD3' ,  , .T. } )
   aAdd( aDefRules, { 'VK_NUMPAD4' ,  , .T. } )
   aAdd( aDefRules, { 'VK_NUMPAD5' ,  , .T. } )
   aAdd( aDefRules, { 'VK_NUMPAD6' ,  , .T. } )
   aAdd( aDefRules, { 'VK_NUMPAD7' ,  , .T. } )
   aAdd( aDefRules, { 'VK_NUMPAD8' ,  , .T. } )
   aAdd( aDefRules, { 'VK_NUMPAD9' ,  , .T. } )
   aAdd( aDefRules, { 'VK_MULTIPLY' ,  , .T. } )
   aAdd( aDefRules, { 'VK_ADD' ,  , .T. } )
   aAdd( aDefRules, { 'VK_SEPARATOR' ,  , .T. } )
   aAdd( aDefRules, { 'VK_SUBTRACT' ,  , .T. } )
   aAdd( aDefRules, { 'VK_DECIMAL' ,  , .T. } )
   aAdd( aDefRules, { 'VK_DIVIDE' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F1' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F2' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F3' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F4' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F5' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F6' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F7' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F8' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F9' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F10' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F11' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F12' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F13' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F14' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F15' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F16' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F17' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F18' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F19' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F20' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F21' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F22' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F23' ,  , .T. } )
   aAdd( aDefRules, { 'VK_F24' ,  , .T. } )
   aAdd( aDefRules, { 'VK_NUMLOCK' ,  , .T. } )
   aAdd( aDefRules, { 'VK_SCROLL' ,  , .T. } )
   aAdd( aDefRules, { 'ACC_NORMAL' ,  , .T. } )
   aAdd( aDefRules, { 'ACC_SHIFT' ,  , .T. } )
   aAdd( aDefRules, { 'ACC_CONTROL' ,  , .T. } )
   aAdd( aDefRules, { 'ACC_ALT' ,  , .T. } )
   aAdd( aDefRules, { '_TREE_CH' ,  , .T. } )
   aAdd( aDefRules, { '_WINAPI_CH' ,  , .T. } )
   aAdd( aDefRules, { 'FM_CLICK' ,  , .T. } )
   aAdd( aDefRules, { 'FM_SCROLLUP' ,  , .T. } )
   aAdd( aDefRules, { 'FM_SCROLLDOWN' ,  , .T. } )
   aAdd( aDefRules, { 'FM_SCROLLPGUP' ,  , .T. } )
   aAdd( aDefRules, { 'FM_SCROLLPGDN' ,  , .T. } )
   aAdd( aDefRules, { 'FM_CHANGE' ,  , .T. } )
   aAdd( aDefRules, { 'FM_COLOR' ,  , .T. } )
   aAdd( aDefRules, { 'FM_MEASURE' ,  , .T. } )
   aAdd( aDefRules, { 'FM_DRAW' ,  , .T. } )
   aAdd( aDefRules, { 'FM_LOSTFOCUS' ,  , .T. } )
   aAdd( aDefRules, { 'FM_THUMBPOS' ,  , .T. } )
   aAdd( aDefRules, { 'FM_CLOSEAREA' ,  , .T. } )
   aAdd( aDefRules, { 'FM_VBXEVENT' ,  , .T. } )
   aAdd( aDefRules, { 'FM_HELPF1' ,  , .T. } )
   aAdd( aDefRules, { 'FM_THUMBTRACK' ,  , .T. } )
   aAdd( aDefRules, { 'FM_DROPOVER' ,  , .T. } )
   aAdd( aDefRules, { 'FM_CHANGEFOCUS' ,  , .T. } )
   aAdd( aDefRules, { 'WM_ASYNCSELECT' ,  , .T. } )
   aAdd( aDefRules, { 'FM_CLOSEUP' ,  , .T. } )
   aAdd( aDefRules, { 'WM_TASKBAR' ,  , .T. } )
   aAdd( aDefRules, { 'IDOK' ,  , .T. } )
   aAdd( aDefRules, { 'ID_OK' ,  , .T. } )
   aAdd( aDefRules, { 'IDCANCEL' ,  , .T. } )
   aAdd( aDefRules, { 'BN_CLICKED' ,  , .T. } )
   aAdd( aDefRules, { 'CS_VREDRAW' ,  , .T. } )
   aAdd( aDefRules, { 'CS_HREDRAW' ,  , .T. } )
   aAdd( aDefRules, { 'CS_GLOBALCLASS' ,  , .T. } )
   aAdd( aDefRules, { 'CS_OWNDC' ,  , .T. } )
   aAdd( aDefRules, { 'CS_CLASSDC' ,  , .T. } )
   aAdd( aDefRules, { 'CS_PARENTDC' ,  , .T. } )
   aAdd( aDefRules, { 'CS_BYTEALIGNCLIENT' ,  , .T. } )
   aAdd( aDefRules, { 'CS_BYTEALIGNWINDOW' ,  , .T. } )
   aAdd( aDefRules, { 'WS_OVERLAPPED' ,  , .T. } )
   aAdd( aDefRules, { 'WS_POPUP' ,  , .T. } )
   aAdd( aDefRules, { 'WS_CHILD' ,  , .T. } )
   aAdd( aDefRules, { 'WS_CLIPSIBLINGS' ,  , .T. } )
   aAdd( aDefRules, { 'WS_CLIPCHILDREN' ,  , .T. } )
   aAdd( aDefRules, { 'WS_VISIBLE' ,  , .T. } )
   aAdd( aDefRules, { 'WS_DISABLED' ,  , .T. } )
   aAdd( aDefRules, { 'WS_MINIMIZE' ,  , .T. } )
   aAdd( aDefRules, { 'WS_MAXIMIZE' ,  , .T. } )
   aAdd( aDefRules, { 'WS_CAPTION' ,  , .T. } )
   aAdd( aDefRules, { 'WS_BORDER' ,  , .T. } )
   aAdd( aDefRules, { 'WS_DLGFRAME' ,  , .T. } )
   aAdd( aDefRules, { 'WS_VSCROLL' ,  , .T. } )
   aAdd( aDefRules, { 'WS_HSCROLL' ,  , .T. } )
   aAdd( aDefRules, { 'WS_SYSMENU' ,  , .T. } )
   aAdd( aDefRules, { 'WS_THICKFRAME' ,  , .T. } )
   aAdd( aDefRules, { 'WS_MINIMIZEBOX' ,  , .T. } )
   aAdd( aDefRules, { 'WS_MAXIMIZEBOX' ,  , .T. } )
   aAdd( aDefRules, { 'WS_GROUP' ,  , .T. } )
   aAdd( aDefRules, { 'WS_TABSTOP' ,  , .T. } )
   aAdd( aDefRules, { 'WS_OVERLAPPEDWINDOW' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'WS_POPUPWINDOW' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'WS_CHILDWINDOW' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'ES_LEFT' ,  , .T. } )
   aAdd( aDefRules, { 'ES_RIGHT' ,  , .T. } )
   aAdd( aDefRules, { 'ES_MULTILINE' ,  , .T. } )
   aAdd( aDefRules, { 'ES_AUTOHSCROLL' ,  , .T. } )
   aAdd( aDefRules, { 'ES_READONLY' ,  , .T. } )
   aAdd( aDefRules, { 'ES_WANTRETURN' ,  , .T. } )
   aAdd( aDefRules, { 'WM_NULL' ,  , .T. } )
   aAdd( aDefRules, { 'WM_DESTROY' ,  , .T. } )
   aAdd( aDefRules, { 'WM_MOVE' ,  , .T. } )
   aAdd( aDefRules, { 'WM_SIZE' ,  , .T. } )
   aAdd( aDefRules, { 'WM_SETFOCUS' ,  , .T. } )
   aAdd( aDefRules, { 'WM_KILLFOCUS' ,  , .T. } )
   aAdd( aDefRules, { 'WM_PAINT' ,  , .T. } )
   aAdd( aDefRules, { 'WM_CLOSE' ,  , .T. } )
   aAdd( aDefRules, { 'WM_QUERYENDSESSION' ,  , .T. } )
   aAdd( aDefRules, { 'WM_QUIT' ,  , .T. } )
   aAdd( aDefRules, { 'WM_SYSCOLORCHANGE' ,  , .T. } )
   aAdd( aDefRules, { 'WM_ENDSESSION' ,  , .T. } )
   aAdd( aDefRules, { 'WM_SYSTEMERROR' ,  , .T. } )
   aAdd( aDefRules, { 'WM_WININICHANGE' ,  , .T. } )
   aAdd( aDefRules, { 'WM_DEVMODECHANGE' ,  , .T. } )
   aAdd( aDefRules, { 'WM_FONTCHANGE' ,  , .T. } )
   aAdd( aDefRules, { 'WM_TIMECHANGE' ,  , .T. } )
   aAdd( aDefRules, { 'WM_SPOOLERSTATUS' ,  , .T. } )
   aAdd( aDefRules, { 'WM_COMPACTING' ,  , .T. } )
   aAdd( aDefRules, { 'WM_GETDLGCODE' ,  , .T. } )
   aAdd( aDefRules, { 'WM_CHAR' ,  , .T. } )
   aAdd( aDefRules, { 'WM_COMMAND' ,  , .T. } )
   aAdd( aDefRules, { 'WM_MOUSEMOVE' ,  , .T. } )
   aAdd( aDefRules, { 'WM_LBUTTONDOWN' ,  , .T. } )
   aAdd( aDefRules, { 'WM_LBUTTONUP' ,  , .T. } )
   aAdd( aDefRules, { 'WM_RBUTTONDOWN' ,  , .T. } )
   aAdd( aDefRules, { 'WM_RBUTTONUP' ,  , .T. } )
   aAdd( aDefRules, { 'WM_KEYDOWN' ,  , .T. } )
   aAdd( aDefRules, { 'WM_KEYUP' ,  , .T. } )
   aAdd( aDefRules, { 'WM_INITDIALOG' ,  , .T. } )
   aAdd( aDefRules, { 'WM_TIMER' ,  , .T. } )
   aAdd( aDefRules, { 'WM_HSCROLL' ,  , .T. } )
   aAdd( aDefRules, { 'WM_VSCROLL' ,  , .T. } )
   aAdd( aDefRules, { 'WM_QUERYNEWPALETTE' ,  , .T. } )
   aAdd( aDefRules, { 'WM_PALETTEISCHANGING' ,  , .T. } )
   aAdd( aDefRules, { 'WM_PALETTECHANGED' ,  , .T. } )
   aAdd( aDefRules, { 'WM_USER' ,  , .T. } )
   aAdd( aDefRules, { 'DS_SYSMODAL' ,  , .T. } )
   aAdd( aDefRules, { 'DS_MODALFRAME' ,  , .T. } )
   aAdd( aDefRules, { 'DLGC_WANTARROWS' ,  , .T. } )
   aAdd( aDefRules, { 'DLGC_WANTTAB' ,  , .T. } )
   aAdd( aDefRules, { 'DLGC_WANTALLKEYS' ,  , .T. } )
   aAdd( aDefRules, { 'DLGC_WANTCHARS' ,  , .T. } )
   aAdd( aDefRules, { 'LBS_NOTIFY' ,  , .T. } )
   aAdd( aDefRules, { 'LBS_SORT' ,  , .T. } )
   aAdd( aDefRules, { 'LBS_OWNERDRAWFIXED' ,  , .T. } )
   aAdd( aDefRules, { 'LBS_USETABSTOPS' ,  , .T. } )
   aAdd( aDefRules, { 'LBS_NOINTEGRALHEIGHT' ,  , .T. } )
   aAdd( aDefRules, { 'LBS_WANTKEYBOARDINPUT' ,  , .T. } )
   aAdd( aDefRules, { 'LBS_DISABLENOSCROLL' ,  , .T. } )
   aAdd( aDefRules, { 'LBS_STANDARD' ,  , .T. } )
   aAdd( aDefRules, { 'CBS_SIMPLE' ,  , .T. } )
   aAdd( aDefRules, { 'CBS_DROPDOWN' ,  , .T. } )
   aAdd( aDefRules, { 'CBS_DROPDOWNLIST' ,  , .T. } )
   aAdd( aDefRules, { 'CBS_OWNERDRAWFIXED' ,  , .T. } )
   aAdd( aDefRules, { 'CBS_AUTOHSCROLL' ,  , .T. } )
   aAdd( aDefRules, { 'CBS_OEMCONVERT' ,  , .T. } )
   aAdd( aDefRules, { 'CBS_SORT' ,  , .T. } )
   aAdd( aDefRules, { 'CBS_DISABLENOSCROLL' ,  , .T. } )
   aAdd( aDefRules, { 'SB_LINEUP' ,  , .T. } )
   aAdd( aDefRules, { 'SB_LINELEFT' ,  , .T. } )
   aAdd( aDefRules, { 'SB_LINEDOWN' ,  , .T. } )
   aAdd( aDefRules, { 'SB_LINERIGHT' ,  , .T. } )
   aAdd( aDefRules, { 'SB_PAGEUP' ,  , .T. } )
   aAdd( aDefRules, { 'SB_PAGELEFT' ,  , .T. } )
   aAdd( aDefRules, { 'SB_PAGEDOWN' ,  , .T. } )
   aAdd( aDefRules, { 'SB_PAGERIGHT' ,  , .T. } )
   aAdd( aDefRules, { 'SB_THUMBPOSITION' ,  , .T. } )
   aAdd( aDefRules, { 'SB_THUMBTRACK' ,  , .T. } )
   aAdd( aDefRules, { 'SB_TOP' ,  , .T. } )
   aAdd( aDefRules, { 'SB_LEFT' ,  , .T. } )
   aAdd( aDefRules, { 'SB_BOTTOM' ,  , .T. } )
   aAdd( aDefRules, { 'SB_RIGHT' ,  , .T. } )
   aAdd( aDefRules, { 'SB_ENDSCROLL' ,  , .T. } )
   aAdd( aDefRules, { 'SBS_HORZ' ,  , .T. } )
   aAdd( aDefRules, { 'SBS_VERT' ,  , .T. } )
   aAdd( aDefRules, { 'BS_PUSHBUTTON' ,  , .T. } )
   aAdd( aDefRules, { 'BS_DEFPUSHBUTTON' ,  , .T. } )
   aAdd( aDefRules, { 'BS_CHECKBOX' ,  , .T. } )
   aAdd( aDefRules, { 'BS_AUTOCHECKBOX' ,  , .T. } )
   aAdd( aDefRules, { 'BS_GROUPBOX' ,  , .T. } )
   aAdd( aDefRules, { 'BS_AUTORADIOBUTTON' ,  , .T. } )
   aAdd( aDefRules, { 'PS_SOLID' ,  , .T. } )
   aAdd( aDefRules, { 'PS_DASH' ,  , .T. } )
   aAdd( aDefRules, { 'PS_DOT' ,  , .T. } )
   aAdd( aDefRules, { 'PS_DASHDOT' ,  , .T. } )
   aAdd( aDefRules, { 'PS_DASHDOTDOT' ,  , .T. } )
   aAdd( aDefRules, { 'PS_NULL' ,  , .T. } )
   aAdd( aDefRules, { 'PS_INSIDEFRAME' ,  , .T. } )
   aAdd( aDefRules, { 'SS_BLACKRECT' ,  , .T. } )
   aAdd( aDefRules, { 'SS_WHITERECT' ,  , .T. } )
   aAdd( aDefRules, { 'SS_WHITEFRAME' ,  , .T. } )
   aAdd( aDefRules, { 'SS_LEFT' ,  , .T. } )
   aAdd( aDefRules, { 'SS_SIMPLE' ,  , .T. } )
   aAdd( aDefRules, { 'DLGINIT' ,  , .T. } )
   aAdd( aDefRules, { 'FN_UNZIP' ,  , .T. } )
   aAdd( aDefRules, { 'Set3dLook' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aDefRules, { 'CRLF' ,  , .T. } )
   aAdd( aDefRules, { 'bSETGET' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )

   /* Translates */
   aAdd( aTransRules, { 'RGB' , { {    1,   0, '(', '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, ',', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aTransRules, { 'NOREF' , { {    0,   0, '(', NIL, NIL }, {    0,   1, '@', NIL, NIL }, {    1,   0, NIL, '<', NIL }, {    0,   0, ')', NIL, NIL } } , .F. } )
   aAdd( aTransRules, { 'DLL32' ,  , .F. } )
   aAdd( aTransRules, { '_PARM_BLOCK_10_' , { {    1,   0, '(', '<', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )

   /* Commands */
   aAdd( aCommRules, { 'SET' , { {    1,   0, NIL, ':', { '_3DLOOK', '3DLOOK', 'LOOK3D', 'LOOK 3D', '3D LOOK' } }, {    2,   0, NIL, ':', { 'ON', 'OFF', '&' } } } , .T. } )
   aAdd( aCommRules, { 'SET' , { {    0,   0, 'RESOURCES', NIL, NIL }, {    1,   0, 'TO', '<', NIL }, { 1002,   1, ',', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'SET' , { {    0,   0, 'RESOURCES', NIL, NIL }, {    0,   0, 'TO', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { 'SET' , { {    0,   0, 'HELPFILE', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'SET' , { {    0,   0, 'HELP', NIL, NIL }, {    0,   0, 'TOPIC', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    1,   0, NIL, '<', NIL }, { 1002,   1, 'AS', ':', { 'CHARACTER', 'NUMERIC', 'LOGICAL', 'DATE' } }, {    3,   1, NIL, ':', { 'RESOURCE', 'RESNAME', 'NAME' } }, {    4,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, 'DIALOG', '<', NIL }, {    2,   1, NIL, ':', { 'NAME', 'RESNAME', 'RESOURCE' } }, {    3,  -1, NIL, '<', NIL }, {    4,   1, 'TITLE', '<', NIL }, {    5,   1, 'FROM', '<', NIL }, {    6,  -1, ',', '<', NIL }, {    7,  -1, 'TO', '<', NIL }, {    8,  -1, ',', '<', NIL }, {    9,   1, 'SIZE', '<', NIL }, {   10,  -1, ',', '<', NIL }, {   11,   1, NIL, ':', { 'LIBRARY', 'DLL' } }, {   12,  -1, NIL, '<', NIL }, {   13,   1, NIL, ':', { 'VBX' } }, {   14,   1, 'STYLE', '<', NIL }, {   15,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   16,  -1, NIL, '<', NIL }, {   17,   2, ',', '<', NIL }, {   18,   1, 'BRUSH', '<', NIL }, {   19,   1, NIL, ':', { 'WINDOW', 'DIALOG', 'OF' } }, {   20,  -1, NIL, '<', NIL }, {   21,   1, NIL, ':', { 'PIXEL' } }, {   22,   1, 'ICON', '<', NIL }, {   23,   1, 'FONT', '<', NIL }, {   24,   1, NIL, ':', { 'HELP', 'HELPID' } }, {   25,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ACTIVATE' , { {    1,   0, 'DIALOG', '<', NIL }, {    2,   1, NIL, ':', { 'CENTER', 'CENTERED' } }, { 1003,   1, NIL, ':', { 'NOWAIT', 'NOMODAL' } }, { 1004,   1, 'WHEN', '<', NIL }, { 1005,   1, 'VALID', '<', NIL }, {    0,   1, 'ON', NIL, NIL }, { 1000,   2, 'LEFT', NIL, NIL }, { 1006,  -1, 'CLICK', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1007,  -1, 'INIT', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1008,  -1, 'MOVE', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1009,  -1, 'PAINT', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1000,  -1, 'RIGHT', NIL, NIL }, { 1010,  -1, 'CLICK', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, 'FONT', '<', NIL }, {    2,   1, 'NAME', '<', NIL }, {    3,   1, 'SIZE', '<', NIL }, {    4,  -1, ',', '<', NIL }, { 1005,   1, NIL, ':', { 'FROM USER' } }, { 1006,   1, NIL, ':', { 'BOLD' } }, { 1007,   1, NIL, ':', { 'ITALIC' } }, { 1008,   1, NIL, ':', { 'UNDERLINE' } }, { 1009,   1, 'WEIGHT', '<', NIL }, { 1010,   1, 'OF', '<', NIL }, { 1011,   1, 'NESCAPEMENT', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ACTIVATE' , { {    1,   0, 'FONT', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEACTIVATE' , { {    1,   0, 'FONT', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'SET' , { {    0,   0, 'FONT', NIL, NIL }, {    1,   1, 'OF', '<', NIL }, {    2,   1, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'INI' , { {    1,   0, NIL, '<', NIL }, {    2,   1, NIL, ':', { 'FILENAME', 'FILE', 'DISK' } }, {    3,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'GET' , { {    1,   0, NIL, '<', NIL }, {    2,   1, 'SECTION', '<', NIL }, {    3,   1, 'ENTRY', '<', NIL }, {    4,   1, 'DEFAULT', '<', NIL }, {    5,   1, NIL, ':', { 'OF', 'INI' } }, {    6,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'SET' , { {    1,   1, 'SECTION', '<', NIL }, {    2,   1, 'ENTRY', '<', NIL }, {    3,   1, 'TO', '<', NIL }, {    4,   1, NIL, ':', { 'OF', 'INI' } }, {    5,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ENDINI' ,  , .T. } )
   aAdd( aCommRules, { 'MENU' , { { 1001,   1, NIL, '<', { 'POPUP' } }, {    2,   1, NIL, ':', { 'POPUP' } } } , .T. } )
   aAdd( aCommRules, { 'MENUITEM' , { { 1001,   1, NIL, '<', { 'MESSAGE', 'CHECK', 'CHECKED', 'MARK', 'ENABLED', 'DISABLED', 'FILE', 'FILENAME', 'DISK', 'RESOURCE', 'RESNAME', 'NAME', 'ACTION', 'BLOCK', 'OF', 'MENU', 'SYSMENU', 'ACCELERATOR', 'HELP', 'HELP ID', 'HELPID', 'WHEN', 'BREAK' } }, {    0,  -1, 'PROMPT', NIL, NIL }, {    2,   1, NIL, '<', { 'MESSAGE', 'CHECK', 'CHECKED', 'MARK', 'ENABLED', 'DISABLED', 'FILE', 'FILENAME', 'DISK', 'RESOURCE', 'RESNAME', 'NAME', 'ACTION', 'BLOCK', 'OF', 'MENU', 'SYSMENU', 'ACCELERATOR', 'HELP', 'HELP ID', 'HELPID', 'WHEN', 'BREAK' } }, {    3,   1, 'MESSAGE', '<', NIL }, {    4,   1, NIL, ':', { 'CHECK', 'CHECKED', 'MARK' } }, { 1005,   1, NIL, ':', { 'ENABLED', 'DISABLED' } }, {    6,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    7,  -1, NIL, '<', NIL }, {    8,   1, NIL, ':', { 'RESOURCE', 'RESNAME', 'NAME' } }, {    9,  -1, NIL, '<', NIL }, { 1010,   1, 'ACTION', 'A', NIL }, {   11,   1, 'BLOCK', '<', NIL }, {   12,   1, NIL, ':', { 'OF', 'MENU', 'SYSMENU' } }, {   13,  -1, NIL, '<', NIL }, {   14,   1, 'ACCELERATOR', '<', NIL }, {   15,  -1, ',', '<', NIL }, {   16,   1, NIL, ':', { 'HELP' } }, {   17,   1, NIL, ':', { 'HELP ID', 'HELPID' } }, {   18,  -1, NIL, '<', NIL }, { 1019,   1, 'WHEN', '<', NIL }, {   20,   1, NIL, ':', { 'BREAK' } } } , .T. } )
   aAdd( aCommRules, { 'MRU' , { {    1,   0, NIL, '<', NIL }, {    2,   1, NIL, ':', { 'INI', 'ININAME', 'FILENAME', 'NAME', 'DISK' } }, {    3,  -1, NIL, '<', NIL }, {    4,   1, 'SECTION', '<', NIL }, {    5,   1, NIL, ':', { 'SIZE', 'ITEMS' } }, {    6,  -1, NIL, '<', NIL }, {    7,   1, 'MESSAGE', '<', NIL }, { 1008,   1, 'ACTION', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'SEPARATOR' , { { 1001,   1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ENDMENU' ,  , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, 'MENU', '<', NIL }, {    2,   1, NIL, ':', { 'RESOURCE', 'NAME', 'RESNAME' } }, {    3,  -1, NIL, '<', NIL }, {    4,   1, NIL, ':', { 'POPUP' } } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'MENUITEM', NIL, NIL }, { 1001,   1, NIL, '<', { 'ID', 'ACTION', 'BLOCK', 'MESSAGE', 'CHECK', 'CHECKED', 'MARK', 'ENABLED', 'DISABLED', 'FILE', 'FILENAME', 'DISK', 'RESOURCE', 'RESNAME', 'NAME', 'ACCELERATOR', 'HELP ID', 'HELPID', 'WHEN' } }, {    0,  -1, 'PROMPT', NIL, NIL }, {    2,   1, NIL, '<', { 'ID', 'ACTION', 'BLOCK', 'MESSAGE', 'CHECK', 'CHECKED', 'MARK', 'ENABLED', 'DISABLED', 'FILE', 'FILENAME', 'DISK', 'RESOURCE', 'RESNAME', 'NAME', 'ACCELERATOR', 'HELP ID', 'HELPID', 'WHEN' } }, {    3,   1, 'ID', '<', NIL }, {    4,  -1, NIL, ':', { 'OF', 'MENU' } }, {    5,  -1, NIL, '<', NIL }, {    6,   1, 'ACTION', '<', NIL }, {    7,   1, 'BLOCK', '<', NIL }, {    8,   1, 'MESSAGE', '<', NIL }, {    9,   1, NIL, ':', { 'CHECK', 'CHECKED', 'MARK' } }, { 1010,   1, NIL, ':', { 'ENABLED', 'DISABLED' } }, {   11,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {   12,  -1, NIL, '<', NIL }, {   13,   1, NIL, ':', { 'RESOURCE', 'RESNAME', 'NAME' } }, {   14,  -1, NIL, '<', NIL }, {   15,   1, 'ACCELERATOR', '<', NIL }, {   16,  -1, ',', '<', NIL }, {   17,   1, NIL, ':', { 'HELP ID', 'HELPID' } }, {   18,  -1, NIL, '<', NIL }, { 1019,   1, 'WHEN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, 'MENU', '<', NIL }, {    2,   0, 'OF', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'SET' , { {    0,   0, 'MENU', NIL, NIL }, {    1,   0, 'OF', '<', NIL }, {    2,   0, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ACTIVATE' , { {    1,   0, NIL, ':', { 'POPUP', 'MENU' } }, {    2,   0, NIL, '<', NIL }, {    3,   1, 'AT', '<', NIL }, {    4,  -1, ',', '<', NIL }, {    5,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    6,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'SYSMENU', NIL, NIL }, { 1001,   1, NIL, '<', { 'OF', 'WINDOW', 'DIALOG' } }, {    2,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    3,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ENDSYSMENU' ,  , .T. } )
   aAdd( aCommRules, { 'PRINT' , { { 1001,   1, NIL, '<', { 'NAME', 'TITLE', 'DOC', 'FROM USER', 'PREVIEW', 'TO' } }, { 1002,   1, NIL, ':', { 'NAME', 'TITLE', 'DOC' } }, { 1003,  -1, NIL, '<', NIL }, {    4,   1, NIL, ':', { 'FROM USER' } }, {    5,   1, NIL, ':', { 'PREVIEW' } }, {    6,   2, NIL, ':', { 'MODAL' } }, {    7,   1, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'PRINTER' , { { 1001,   1, NIL, '<', { 'NAME', 'DOC', 'FROM USER', 'PREVIEW', 'TO' } }, { 1002,   1, NIL, ':', { 'NAME', 'DOC' } }, { 1003,  -1, NIL, '<', NIL }, {    4,   1, NIL, ':', { 'FROM USER' } }, {    5,   1, NIL, ':', { 'PREVIEW' } }, {    6,   2, NIL, ':', { 'MODAL' } }, {    7,   1, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'PAGE' ,  , .T. } )
   aAdd( aCommRules, { 'ENDPAGE' ,  , .T. } )
   aAdd( aCommRules, { 'ENDPRINT' ,  , .T. } )
   aAdd( aCommRules, { 'ENDPRINTER' ,  , .T. } )
   aAdd( aCommRules, { 'DLL' , { { 1001,   1, NIL, ':', { 'STATIC' } }, {    2,   0, 'FUNCTION', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1003,   1, NIL, '<', { ',', ')' } }, { 1004,  -1, 'AS', '<', NIL }, { 1005,   1, ',', '<', NIL }, { 1006,  -1, 'AS', '<', NIL }, {    0,   0, ')', NIL, NIL }, {    7,   0, 'AS', '<', NIL }, { 1008,   1, NIL, ':', { 'PASCAL' } }, { 1009,   1, 'FROM', '<', NIL }, {   10,   0, 'LIB', '*', NIL } } , .T. } )
   aAdd( aCommRules, { 'DLL32' , { { 1001,   1, NIL, ':', { 'STATIC' } }, {    2,   0, 'FUNCTION', '<', NIL }, {    0,   0, '(', NIL, NIL }, { 1003,   1, NIL, '<', { ',', ')' } }, { 1004,  -1, 'AS', '<', NIL }, { 1005,   1, ',', '<', NIL }, { 1006,  -1, 'AS', '<', NIL }, {    0,   0, ')', NIL, NIL }, {    7,   0, 'AS', '<', NIL }, { 1008,   1, NIL, ':', { 'PASCAL' } }, { 1009,   1, 'FROM', '<', NIL }, {   10,   0, 'LIB', '*', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'FOLDER', NIL, NIL }, { 1003,   1, NIL, '<', { 'OF', 'WINDOW', 'DIALOG', 'PROMPT', 'PROMPTS', 'ITEMS', 'DIALOG', 'DIALOGS', 'PAGE', 'PAGES', 'PIXEL', 'DESIGN', 'COLOR', 'COLORS', 'OPTION', 'SIZE', 'MESSAGE', 'ADJUST', 'FONT' } }, {    4,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    5,  -1, NIL, '<', NIL }, { 1006,   1, NIL, ':', { 'PROMPT', 'PROMPTS', 'ITEMS' } }, { 1007,  -1, NIL, 'A', NIL }, {    8,   1, NIL, ':', { 'DIALOG', 'DIALOGS', 'PAGE', 'PAGES' } }, {    9,  -1, NIL, '<', NIL }, { 1010,   2, ',', '<', NIL }, {   11,   1, NIL, ':', { 'PIXEL' } }, {   12,   1, NIL, ':', { 'DESIGN' } }, {   13,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   14,  -1, NIL, '<', NIL }, {   15,   2, ',', '<', NIL }, {   16,   1, 'OPTION', '<', NIL }, {   17,   1, 'SIZE', '<', NIL }, {   18,  -1, ',', '<', NIL }, {   19,   1, 'MESSAGE', '<', NIL }, {   20,   1, NIL, ':', { 'ADJUST' } }, {   21,   1, 'FONT', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'FOLDER', NIL, NIL }, { 1001,   1, NIL, '<', { 'ID', 'OF', 'WINDOW', 'DIALOG', 'PROMPT', 'PROMPTS', 'ITEMS', 'DIALOG', 'DIALOGS', 'PAGE', 'PAGES', 'COLOR', 'COLORS', 'OPTION', 'ON', 'ADJUST' } }, {    2,   1, 'ID', '<', NIL }, {    3,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    4,  -1, NIL, '<', NIL }, { 1005,   1, NIL, ':', { 'PROMPT', 'PROMPTS', 'ITEMS' } }, { 1006,  -1, NIL, 'A', NIL }, {    7,   1, NIL, ':', { 'DIALOG', 'DIALOGS', 'PAGE', 'PAGES' } }, {    8,  -1, NIL, '<', NIL }, { 1009,   2, ',', '<', NIL }, {   10,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   11,  -1, NIL, '<', NIL }, {   12,   2, ',', '<', NIL }, {   13,   1, 'OPTION', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1014,  -1, 'CHANGE', '<', NIL }, {   15,   1, NIL, ':', { 'ADJUST' } } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'TABS', NIL, NIL }, { 1003,   1, NIL, '<', { 'OF', 'WINDOW', 'DIALOG', 'PROMPT', 'PROMPTS', 'ITEMS', 'ACTION', 'EXECUTE', 'ON CHANGE', 'PIXEL', 'DESIGN', 'COLOR', 'COLORS', 'OPTION', 'SIZE', 'MESSAGE' } }, {    4,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    5,  -1, NIL, '<', NIL }, { 1006,   1, NIL, ':', { 'PROMPT', 'PROMPTS', 'ITEMS' } }, { 1007,  -1, NIL, 'A', NIL }, { 1008,   1, NIL, ':', { 'ACTION', 'EXECUTE', 'ON CHANGE' } }, { 1009,  -1, NIL, '<', NIL }, {   10,   1, NIL, ':', { 'PIXEL' } }, {   11,   1, NIL, ':', { 'DESIGN' } }, {   12,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   13,  -1, NIL, '<', NIL }, {   14,   2, ',', '<', NIL }, {   15,   1, 'OPTION', '<', NIL }, {   16,   1, 'SIZE', '<', NIL }, {   17,  -1, ',', '<', NIL }, {   18,   1, 'MESSAGE', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'TABS', NIL, NIL }, { 1001,   1, NIL, '<', { 'ID', 'OF', 'WINDOW', 'DIALOG', 'PROMPT', 'PROMPTS', 'ITEMS', 'ACTION', 'EXECUTE', 'COLOR', 'COLORS', 'OPTION' } }, {    2,   1, 'ID', '<', NIL }, {    3,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    4,  -1, NIL, '<', NIL }, { 1005,   1, NIL, ':', { 'PROMPT', 'PROMPTS', 'ITEMS' } }, { 1006,  -1, NIL, 'A', NIL }, { 1007,   1, NIL, ':', { 'ACTION', 'EXECUTE' } }, { 1008,  -1, NIL, '<', NIL }, {    9,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   10,  -1, NIL, '<', NIL }, {   11,   2, ',', '<', NIL }, {   12,   1, 'OPTION', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    1,   0, 'PAGES', '<', NIL }, {    2,   1, 'ID', '<', NIL }, {    3,   1, 'OF', '<', NIL }, { 1004,   1, 'DIALOGS', 'A', NIL }, {    5,   1, 'OPTION', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1006,  -1, 'CHANGE', '<', NIL }, {    7,   1, 'FONT', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, 'ODBC', '<', NIL }, {    2,   1, 'NAME', '<', NIL }, {    3,   1, 'USER', '<', NIL }, {    4,   1, 'PASSWORD', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ODBC' , { {    1,   0, NIL, '<', NIL }, {    2,   0, NIL, ':', { 'SQL', 'EXECUTE' } }, {    3,   0, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, NIL, ':', { 'DDE', 'LINK' } }, {    2,   0, NIL, '<', NIL }, {    3,   1, 'SERVICE', '<', NIL }, {    4,   1, 'TOPIC', '<', NIL }, {    5,   1, 'ITEM', '<', NIL }, { 1006,   1, 'ACTION', '<', NIL }, { 1007,   1, 'VALID', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ACTIVATE' , { {    1,   0, NIL, ':', { 'DDE', 'LINK' } }, {    2,   0, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    0,   0, 'VIDEO', NIL, NIL }, { 1001,   1, NIL, '<', { 'FILE', 'FILENAME', 'DISK', 'OF', 'WINDOW', 'DIALOG' } }, {    2,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    3,  -1, NIL, '<', NIL }, {    4,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    5,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ACTIVATE' , { {    1,   0, 'VIDEO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'PLAY' , { {    1,   0, 'VIDEO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'VIDEO', NIL, NIL }, { 1003,   1, NIL, '<', { 'SIZE', 'FILE', 'FILENAME', 'DISK', 'OF', 'WINDOW', 'DIALOG', 'NOBORDER' } }, {    4,   1, 'SIZE', '<', NIL }, {    5,  -1, ',', '<', NIL }, {    6,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    7,  -1, NIL, '<', NIL }, {    8,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    9,  -1, NIL, '<', NIL }, {   10,   1, NIL, ':', { 'NOBORDER' } } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'VIDEO', NIL, NIL }, { 1001,   1, NIL, '<', { 'ID', 'OF', 'WINDOW', 'DIALOG', 'WHEN', 'VALID', 'FILE', 'FILENAME', 'DISK' } }, {    2,   1, 'ID', '<', NIL }, {    3,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    4,  -1, NIL, '<', NIL }, {    5,   1, 'WHEN', '<', NIL }, {    6,   1, 'VALID', '<', NIL }, {    7,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    8,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'TREE' , { { 1001,   1, NIL, '<', { 'BITMAPS' } }, {    2,   1, 'BITMAPS', '<', NIL }, {    3,  -1, ',', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'TREEITEM' , { { 1001,   1, NIL, '<', NIL }, {    0,  -1, 'PROMPT', NIL, NIL }, {    2,   0, NIL, '<', NIL }, {    3,   1, 'RESOURCE', '<', NIL }, {    4,   2, ',', '<', NIL }, {    5,   1, 'FILENAME', '<', NIL }, {    6,   2, ',', '<', NIL }, {    7,   1, NIL, ':', { 'OPENED', 'OPEN' } } } , .T. } )
   aAdd( aCommRules, { 'ENDTREE' ,  , .T. } )
   aAdd( aCommRules, { 'SET' , { {    1,   0, 'MULTIPLE', ':', { 'ON', 'OFF' } } } , .T. } )
   aAdd( aCommRules, { 'DEFAULT' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ':=', '<', NIL }, { 1003,   1, ',', '<', NIL }, { 1004,  -1, ':=', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DO' ,  , .T. } )
   aAdd( aCommRules, { 'UNTIL' , { {    1,   0, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'SET' , { {    0,   0, 'IDLEACTION', NIL, NIL }, {    1,   0, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DATABASE' , { {    1,   0, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'RELEASE' , { {    1,   0, NIL, '<', NIL }, {    2,   0, NIL, '<', NIL }, { 1003,   1, ',', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    0,   0, 'BRUSH', NIL, NIL }, { 1001,   1, NIL, '<', { 'STYLE', 'COLOR', 'FILE', 'FILENAME', 'DISK', 'RESOURCE', 'NAME', 'RESNAME' } }, { 1002,   1, 'STYLE', '<', NIL }, {    3,   1, 'COLOR', '<', NIL }, {    4,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    5,  -1, NIL, '<', NIL }, {    6,   1, NIL, ':', { 'RESOURCE', 'NAME', 'RESNAME' } }, {    7,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'SET' , { {    0,   0, 'BRUSH', NIL, NIL }, {    1,   1, 'OF', '<', NIL }, {    2,   1, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, 'PEN', '<', NIL }, {    2,   1, 'STYLE', '<', NIL }, {    3,   1, 'WIDTH', '<', NIL }, {    4,   1, 'COLOR', '<', NIL }, {    5,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    6,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ACTIVATE' , { {    1,   0, 'PEN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    0,   0, 'BUTTONBAR', NIL, NIL }, { 1001,   1, NIL, '<', { 'SIZE', 'BUTTONSIZE', 'SIZEBUTTON', '_3D', '3D', '3DLOOK', '_3DLOOK', 'TOP', 'LEFT', 'RIGHT', 'BOTTOM', 'FLOAT', 'OF', 'WINDOW', 'DIALOG', 'CURSOR' } }, {    2,   1, NIL, ':', { 'SIZE', 'BUTTONSIZE', 'SIZEBUTTON' } }, {    3,  -1, NIL, '<', NIL }, {    4,  -1, ',', '<', NIL }, {    5,   1, NIL, ':', { '_3D', '3D', '3DLOOK', '_3DLOOK' } }, { 1006,   1, NIL, ':', { 'TOP', 'LEFT', 'RIGHT', 'BOTTOM', 'FLOAT' } }, {    7,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    8,  -1, NIL, '<', NIL }, {    9,   1, 'CURSOR', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'BUTTONBAR', NIL, NIL }, { 1003,   1, NIL, '<', { 'SIZE', 'BUTTONSIZE', '3D', '3DLOOK', '_3DLOOK', 'TOP', 'LEFT', 'RIGHT', 'BOTTOM', 'FLOAT', 'OF', 'WINDOW', 'DIALOG', 'CURSOR' } }, {    4,   1, 'SIZE', '<', NIL }, {    5,  -1, ',', '<', NIL }, {    6,   1, 'BUTTONSIZE', '<', NIL }, {    7,  -1, ',', '<', NIL }, {    8,   1, NIL, ':', { '3D', '3DLOOK', '_3DLOOK' } }, { 1009,   1, NIL, ':', { 'TOP', 'LEFT', 'RIGHT', 'BOTTOM', 'FLOAT' } }, {   10,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {   11,  -1, NIL, '<', NIL }, {   12,   1, 'CURSOR', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    0,   0, 'BUTTON', NIL, NIL }, { 1001,   1, NIL, '<', { 'OF', 'BUTTONBAR', 'NAME', 'RESNAME', 'RESOURCE', 'FILE', 'FILENAME', 'DISK', 'ACTION', 'EXEC', 'GROUP', 'MESSAGE', 'ADJUST', 'WHEN', 'TOOLTIP', 'PRESSED', 'ON', 'AT', 'PROMPT', 'FONT', 'NOBORDER', 'FLAT', 'MENU' } }, {    2,   1, NIL, ':', { 'OF', 'BUTTONBAR' } }, {    3,  -1, NIL, '<', NIL }, {    4,   1, NIL, ':', { 'NAME', 'RESNAME', 'RESOURCE' } }, {    5,  -1, NIL, '<', NIL }, {    6,   2, ',', '<', NIL }, { 1007,   3, ',', '<', NIL }, {    8,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    9,  -1, NIL, '<', NIL }, {   10,   2, ',', '<', NIL }, { 1011,   3, ',', '<', NIL }, { 1012,   1, NIL, ':', { 'ACTION', 'EXEC' } }, { 1013,  -1, NIL, 'A', NIL }, {   14,   1, NIL, ':', { 'GROUP' } }, {   15,   1, 'MESSAGE', '<', NIL }, {   16,   1, NIL, ':', { 'ADJUST' } }, {   17,   1, 'WHEN', '<', NIL }, {   18,   1, 'TOOLTIP', '<', NIL }, {   19,   1, NIL, ':', { 'PRESSED' } }, { 1000,   1, 'ON', NIL, NIL }, { 1020,  -1, 'DROP', '<', NIL }, {   21,   1, 'AT', '<', NIL }, {   22,   1, 'PROMPT', '<', NIL }, {   23,   1, 'FONT', '<', NIL }, { 1024,   1, NIL, ':', { 'NOBORDER', 'FLAT' } }, { 1025,   1, 'MENU', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'BTNBMP', NIL, NIL }, { 1001,   1, NIL, '<', { 'ID', 'OF', 'BUTTONBAR', 'NAME', 'RESNAME', 'RESOURCE', 'FILE', 'FILENAME', 'DISK', 'ACTION', 'EXEC', 'ON CLICK', 'MESSAGE', 'ADJUST', 'WHEN', 'UPDATE', 'TOOLTIP', 'PROMPT', 'FONT', 'NOBORDER' } }, {    2,   1, 'ID', '<', NIL }, {    3,   1, NIL, ':', { 'OF', 'BUTTONBAR' } }, {    4,  -1, NIL, '<', NIL }, {    5,   1, NIL, ':', { 'NAME', 'RESNAME', 'RESOURCE' } }, {    6,  -1, NIL, '<', NIL }, {    7,   2, ',', '<', NIL }, { 1008,   3, ',', '<', NIL }, {    9,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {   10,  -1, NIL, '<', NIL }, {   11,   2, ',', '<', NIL }, { 1012,   3, ',', '<', NIL }, { 1013,   1, NIL, ':', { 'ACTION', 'EXEC', 'ON CLICK' } }, { 1014,  -1, NIL, 'A', NIL }, {   15,   1, 'MESSAGE', '<', NIL }, {   16,   1, NIL, ':', { 'ADJUST' } }, {   17,   1, 'WHEN', '<', NIL }, {   18,   1, NIL, ':', { 'UPDATE' } }, {   19,   1, 'TOOLTIP', '<', NIL }, {   20,   1, 'PROMPT', '<', NIL }, {   21,   1, 'FONT', '<', NIL }, { 1022,   1, NIL, ':', { 'NOBORDER' } } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'BTNBMP', NIL, NIL }, { 1003,   1, NIL, '<', { 'NAME', 'RESNAME', 'RESOURCE', 'FILE', 'FILENAME', 'DISK', 'SIZE', 'ACTION', 'OF', 'WINDOW', 'DIALOG', 'MESSAGE', 'WHEN', 'ADJUST', 'UPDATE', 'PROMPT', 'FONT', 'NOBORDER' } }, {    4,   1, NIL, ':', { 'NAME', 'RESNAME', 'RESOURCE' } }, {    5,  -1, NIL, '<', NIL }, {    6,   2, ',', '<', NIL }, { 1007,   3, ',', '<', NIL }, {    8,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    9,  -1, NIL, '<', NIL }, {   10,   2, ',', '<', NIL }, { 1011,   3, ',', '<', NIL }, {   12,   1, 'SIZE', '<', NIL }, {   13,  -1, ',', '<', NIL }, { 1014,   1, 'ACTION', 'A', NIL }, {   15,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {   16,  -1, NIL, '<', NIL }, {   17,   1, 'MESSAGE', '<', NIL }, {   18,   1, 'WHEN', '<', NIL }, {   19,   1, NIL, ':', { 'ADJUST' } }, {   20,   1, NIL, ':', { 'UPDATE' } }, {   21,   1, 'PROMPT', '<', NIL }, {   22,   1, 'FONT', '<', NIL }, {   23,   1, NIL, ':', { 'NOBORDER' } } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'ICON', NIL, NIL }, { 1003,   1, NIL, '<', { 'NAME', 'RESOURCE', 'RESNAME', 'FILE', 'FILENAME', 'DISK', 'BORDER', 'ON', 'OF', 'WINDOW', 'DIALOG', 'UPDATE', 'WHEN', 'COLOR' } }, {    4,   1, NIL, ':', { 'NAME', 'RESOURCE', 'RESNAME' } }, {    5,  -1, NIL, '<', NIL }, {    6,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    7,  -1, NIL, '<', NIL }, {    8,   1, NIL, ':', { 'BORDER' } }, {    0,   1, 'ON', NIL, NIL }, {    9,  -1, 'CLICK', '<', NIL }, {   10,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {   11,  -1, NIL, '<', NIL }, {   12,   1, NIL, ':', { 'UPDATE' } }, {   13,   1, 'WHEN', '<', NIL }, {   14,   1, 'COLOR', '<', NIL }, {   15,   2, ',', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { { 1001,   1, 'ICON', '<', NIL }, {    2,   1, 'ID', '<', NIL }, {    3,   1, NIL, ':', { 'NAME', 'RESOURCE', 'RESNAME' } }, {    4,  -1, NIL, '<', NIL }, {    5,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    6,  -1, NIL, '<', NIL }, {    0,   1, 'ON', NIL, NIL }, {    7,  -1, 'CLICK', '<', NIL }, {    8,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    9,  -1, NIL, '<', NIL }, {   10,   1, NIL, ':', { 'UPDATE' } }, {   11,   1, 'WHEN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, 'ICON', '<', NIL }, {    2,   1, NIL, ':', { 'NAME', 'RESOURCE', 'RESNAME' } }, {    3,  -1, NIL, '<', NIL }, {    4,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    5,  -1, NIL, '<', NIL }, {    6,   1, 'WHEN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'BUTTON', NIL, NIL }, { 1003,   1, NIL, '<', NIL }, {    0,  -1, 'PROMPT', NIL, NIL }, {    4,   0, NIL, '<', NIL }, {    5,   1, 'SIZE', '<', NIL }, {    6,  -1, ',', '<', NIL }, {    7,   1, 'ACTION', '<', NIL }, {    8,   1, NIL, ':', { 'DEFAULT' } }, {    9,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {   10,  -1, NIL, '<', NIL }, {   11,   1, NIL, ':', { 'HELP', 'HELPID', 'HELP ID' } }, {   12,  -1, NIL, '<', NIL }, {   13,   1, 'FONT', '<', NIL }, {   14,   1, NIL, ':', { 'PIXEL' } }, {   15,   1, NIL, ':', { 'DESIGN' } }, {   16,   1, 'MESSAGE', '<', NIL }, {   17,   1, NIL, ':', { 'UPDATE' } }, {   18,   1, 'WHEN', '<', NIL }, {   19,   1, 'VALID', '<', NIL }, {   20,   1, NIL, ':', { 'CANCEL' } } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'BUTTON', NIL, NIL }, { 1001,   1, NIL, '<', { 'ID', 'ACTION', 'HELP', 'HELPID', 'HELP ID', 'MESSAGE', 'UPDATE', 'WHEN', 'VALID', 'PROMPT', 'CANCEL' } }, {    2,   1, 'ID', '<', NIL }, {    3,   2, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    4,  -2, NIL, '<', NIL }, { 1005,   1, 'ACTION', 'A', NIL }, {    6,   1, NIL, ':', { 'HELP', 'HELPID', 'HELP ID' } }, {    7,  -1, NIL, '<', NIL }, {    8,   1, 'MESSAGE', '<', NIL }, {    9,   1, NIL, ':', { 'UPDATE' } }, {   10,   1, 'WHEN', '<', NIL }, {   11,   1, 'VALID', '<', NIL }, {   12,   1, 'PROMPT', '<', NIL }, {   13,   1, NIL, ':', { 'CANCEL' } } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'CHECKBOX', NIL, NIL }, { 1001,   1, NIL, '<', NIL }, {    0,  -1, 'VAR', NIL, NIL }, {    2,   0, NIL, '<', NIL }, {    3,   1, 'ID', '<', NIL }, {    4,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    5,  -1, NIL, '<', NIL }, {    6,   1, NIL, ':', { 'HELPID', 'HELP ID' } }, {    7,  -1, NIL, '<', NIL }, { 1008,   1, NIL, ':', { 'ON CLICK', 'ON CHANGE' } }, { 1009,  -1, NIL, '<', NIL }, {   10,   1, 'VALID', '<', NIL }, {   11,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   12,  -1, NIL, '<', NIL }, {   13,   2, ',', '<', NIL }, {   14,   1, 'MESSAGE', '<', NIL }, {   15,   1, NIL, ':', { 'UPDATE' } }, {   16,   1, 'WHEN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'CHECKBOX', NIL, NIL }, { 1003,   1, NIL, '<', { 'PROMPT', 'OF', 'WINDOW', 'DIALOG', 'SIZE', 'HELPID', 'HELP ID', 'FONT', 'ON CLICK', 'ON CHANGE', 'VALID', 'COLOR', 'COLORS', 'DESIGN', 'PIXEL', 'MESSAGE', 'UPDATE', 'WHEN' } }, {    0,  -1, 'VAR', NIL, NIL }, { 1004,   1, NIL, '<', { 'PROMPT', 'OF', 'WINDOW', 'DIALOG', 'SIZE', 'HELPID', 'HELP ID', 'FONT', 'ON CLICK', 'ON CHANGE', 'VALID', 'COLOR', 'COLORS', 'DESIGN', 'PIXEL', 'MESSAGE', 'UPDATE', 'WHEN' } }, {    5,   1, 'PROMPT', '<', NIL }, {    6,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    7,  -1, NIL, '<', NIL }, {    8,   1, 'SIZE', '<', NIL }, {    9,  -1, ',', '<', NIL }, {   10,   1, NIL, ':', { 'HELPID', 'HELP ID' } }, {   11,  -1, NIL, '<', NIL }, {   12,   1, 'FONT', '<', NIL }, { 1013,   1, NIL, ':', { 'ON CLICK', 'ON CHANGE' } }, { 1014,  -1, NIL, '<', NIL }, {   15,   1, 'VALID', '<', NIL }, {   16,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   17,  -1, NIL, '<', NIL }, {   18,   2, ',', '<', NIL }, {   19,   1, NIL, ':', { 'DESIGN' } }, {   20,   1, NIL, ':', { 'PIXEL' } }, {   21,   1, 'MESSAGE', '<', NIL }, {   22,   1, NIL, ':', { 'UPDATE' } }, {   23,   1, 'WHEN', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'COMBOBOX', NIL, NIL }, { 1003,   1, NIL, '<', NIL }, {    0,  -1, 'VAR', NIL, NIL }, {    4,   0, NIL, '<', NIL }, {    5,   1, NIL, ':', { 'PROMPTS', 'ITEMS' } }, {    6,  -1, NIL, '<', NIL }, {    7,   1, 'SIZE', '<', NIL }, {    8,  -1, ',', '<', NIL }, {    9,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {   10,  -1, NIL, '<', NIL }, {   11,   1, NIL, ':', { 'HELPID', 'HELP ID' } }, {   12,  -1, NIL, '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1013,  -1, 'CHANGE', '<', NIL }, {   14,   1, 'VALID', '<', NIL }, {   15,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   16,  -1, NIL, '<', NIL }, {   17,   2, ',', '<', NIL }, {   18,   1, NIL, ':', { 'PIXEL' } }, {   19,   1, 'FONT', '<', NIL }, {   20,   1, NIL, ':', { 'UPDATE' } }, {   21,   1, 'MESSAGE', '<', NIL }, {   22,   1, 'WHEN', '<', NIL }, {   23,   1, NIL, ':', { 'DESIGN' } }, {   24,   1, 'BITMAPS', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1025,  -1, 'DRAWITEM', '<', NIL }, {   26,   1, 'STYLE', '<', NIL }, {   27,   1, 'PICTURE', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1000,  -1, 'EDIT', NIL, NIL }, { 1028,  -1, 'CHANGE', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'COMBOBOX', NIL, NIL }, { 1001,   1, NIL, '<', NIL }, {    0,  -1, 'VAR', NIL, NIL }, {    2,   0, NIL, '<', NIL }, {    3,   1, NIL, ':', { 'PROMPTS', 'ITEMS' } }, {    4,  -1, NIL, '<', NIL }, {    5,   1, 'ID', '<', NIL }, {    6,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    7,  -1, NIL, '<', NIL }, {    8,   1, NIL, ':', { 'HELPID', 'HELP ID' } }, {    9,  -1, NIL, '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1010,  -1, 'CHANGE', '<', NIL }, {   11,   1, 'VALID', '<', NIL }, {   12,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   13,  -1, NIL, '<', NIL }, {   14,   2, ',', '<', NIL }, {   15,   1, NIL, ':', { 'UPDATE' } }, {   16,   1, 'MESSAGE', '<', NIL }, {   17,   1, 'WHEN', '<', NIL }, {   18,   1, 'BITMAPS', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1019,  -1, 'DRAWITEM', '<', NIL }, {   20,   1, 'STYLE', '<', NIL }, {   21,   1, 'PICTURE', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1000,  -1, 'EDIT', NIL, NIL }, { 1022,  -1, 'CHANGE', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'LISTBOX', NIL, NIL }, { 1001,   1, NIL, '<', NIL }, {    0,  -1, 'VAR', NIL, NIL }, {    2,   0, NIL, '<', NIL }, {    3,   1, NIL, ':', { 'PROMPTS', 'ITEMS' } }, {    4,  -1, NIL, '<', NIL }, {    5,   1, NIL, ':', { 'FILES', 'FILESPEC' } }, {    6,  -1, NIL, '<', NIL }, {    7,   1, 'ID', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1008,  -1, 'CHANGE', 'A', NIL }, {    0,   1, 'ON', NIL, NIL }, {    0,   2, 'LEFT', NIL, NIL }, {    9,  -1, 'DBLCLICK', '<', NIL }, {   10,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {   11,  -1, NIL, '<', NIL }, {   12,   1, NIL, ':', { 'HELPID', 'HELP ID' } }, {   13,  -1, NIL, '<', NIL }, {   14,   1, 'VALID', '<', NIL }, {   15,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   16,  -1, NIL, '<', NIL }, {   17,   2, ',', '<', NIL }, {   18,   1, 'MESSAGE', '<', NIL }, {   19,   1, NIL, ':', { 'UPDATE' } }, {   20,   1, 'WHEN', '<', NIL }, {   21,   1, 'BITMAPS', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1022,  -1, 'DRAWITEM', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'LISTBOX', NIL, NIL }, { 1003,   1, NIL, '<', NIL }, {    0,  -1, 'VAR', NIL, NIL }, {    4,   0, NIL, '<', NIL }, {    5,   1, NIL, ':', { 'PROMPTS', 'ITEMS' } }, {    6,  -1, NIL, '<', NIL }, {    7,   1, 'SIZE', '<', NIL }, {    8,  -1, ',', '<', NIL }, {    0,   1, 'ON', NIL, NIL }, {    9,  -1, 'CHANGE', '<', NIL }, {    0,   1, 'ON', NIL, NIL }, {    0,   2, 'LEFT', NIL, NIL }, {   10,  -1, 'DBLCLICK', '<', NIL }, {   11,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {   12,  -1, NIL, '<', NIL }, {   13,   1, 'VALID', '<', NIL }, {   14,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   15,  -1, NIL, '<', NIL }, {   16,   2, ',', '<', NIL }, {   17,   1, NIL, ':', { 'PIXEL' } }, {   18,   1, NIL, ':', { 'DESIGN' } }, {   19,   1, 'FONT', '<', NIL }, {   20,   1, 'MESSAGE', '<', NIL }, {   21,   1, NIL, ':', { 'UPDATE' } }, {   22,   1, 'WHEN', '<', NIL }, {   23,   1, 'BITMAPS', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1024,  -1, 'DRAWITEM', '<', NIL }, {   25,   1, NIL, ':', { 'MULTI', 'MULTIPLE', 'MULTISEL' } }, {   26,   1, NIL, ':', { 'SORT' } } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'LISTBOX', NIL, NIL }, { 1001,   1, NIL, '<', { 'FIELDS' } }, {    0,   0, 'FIELDS', NIL, NIL }, { 1002,   1, NIL, 'A', { 'ALIAS', 'ID', 'OF', 'DIALOG', 'FIELDSIZES', 'SIZES', 'COLSIZES', 'HEAD', 'HEADER', 'HEADERS', 'TITLE', 'SELECT', 'ON', 'ON', 'ON', 'ON', 'FONT', 'CURSOR', 'COLOR', 'COLORS', 'MESSAGE', 'UPDATE', 'WHEN', 'VALID', 'ACTION' } }, {    3,   1, 'ALIAS', '<', NIL }, {    4,   1, 'ID', '<', NIL }, {    5,   1, NIL, ':', { 'OF', 'DIALOG' } }, {    6,  -1, NIL, '<', NIL }, { 1007,   1, NIL, ':', { 'FIELDSIZES', 'SIZES', 'COLSIZES' } }, { 1008,  -1, NIL, 'A', NIL }, { 1009,   1, NIL, ':', { 'HEAD', 'HEADER', 'HEADERS', 'TITLE' } }, { 1010,  -1, NIL, 'A', NIL }, {   11,   1, 'SELECT', '<', NIL }, {   12,  -1, 'FOR', '<', NIL }, {   13,   2, 'TO', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1014,  -1, 'CHANGE', '<', NIL }, {    0,   1, 'ON', NIL, NIL }, { 1000,   2, 'LEFT', NIL, NIL }, { 1015,  -1, 'CLICK', '<', NIL }, {    0,   1, 'ON', NIL, NIL }, { 1000,   2, 'LEFT', NIL, NIL }, { 1016,  -1, 'DBLCLICK', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1000,  -1, 'RIGHT', NIL, NIL }, { 1017,  -1, 'CLICK', '<', NIL }, {   18,   1, 'FONT', '<', NIL }, {   19,   1, 'CURSOR', '<', NIL }, {   20,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   21,  -1, NIL, '<', NIL }, {   22,   2, ',', '<', NIL }, {   23,   1, 'MESSAGE', '<', NIL }, {   24,   1, NIL, ':', { 'UPDATE' } }, {   25,   1, 'WHEN', '<', NIL }, {   26,   1, 'VALID', '<', NIL }, { 1027,   1, 'ACTION', 'A', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'LISTBOX', NIL, NIL }, { 1003,   1, NIL, '<', { 'FIELDS' } }, {    0,   0, 'FIELDS', NIL, NIL }, { 1004,   1, NIL, 'A', { 'ALIAS', 'FIELDSIZES', 'SIZES', 'COLSIZES', 'HEAD', 'HEADER', 'HEADERS', 'TITLE', 'SIZE', 'OF', 'DIALOG', 'SELECT', 'ON', 'ON', 'ON', 'ON', 'FONT', 'CURSOR', 'COLOR', 'COLORS', 'MESSAGE', 'UPDATE', 'PIXEL', 'WHEN', 'DESIGN', 'VALID', 'ACTION' } }, {    5,   1, 'ALIAS', '<', NIL }, { 1006,   1, NIL, ':', { 'FIELDSIZES', 'SIZES', 'COLSIZES' } }, { 1007,  -1, NIL, 'A', NIL }, { 1008,   1, NIL, ':', { 'HEAD', 'HEADER', 'HEADERS', 'TITLE' } }, { 1009,  -1, NIL, 'A', NIL }, {   10,   1, 'SIZE', '<', NIL }, {   11,  -1, ',', '<', NIL }, {   12,   1, NIL, ':', { 'OF', 'DIALOG' } }, {   13,  -1, NIL, '<', NIL }, {   14,   1, 'SELECT', '<', NIL }, {   15,  -1, 'FOR', '<', NIL }, {   16,   2, 'TO', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1017,  -1, 'CHANGE', '<', NIL }, {    0,   1, 'ON', NIL, NIL }, {    0,   2, 'LEFT', NIL, NIL }, {   18,  -1, 'CLICK', '<', NIL }, {    0,   1, 'ON', NIL, NIL }, { 1000,   2, 'LEFT', NIL, NIL }, { 1019,  -1, 'DBLCLICK', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1000,  -1, 'RIGHT', NIL, NIL }, { 1020,  -1, 'CLICK', '<', NIL }, {   21,   1, 'FONT', '<', NIL }, {   22,   1, 'CURSOR', '<', NIL }, {   23,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   24,  -1, NIL, '<', NIL }, {   25,   2, ',', '<', NIL }, {   26,   1, 'MESSAGE', '<', NIL }, {   27,   1, NIL, ':', { 'UPDATE' } }, {   28,   1, NIL, ':', { 'PIXEL' } }, {   29,   1, 'WHEN', '<', NIL }, {   30,   1, NIL, ':', { 'DESIGN' } }, {   31,   1, 'VALID', '<', NIL }, { 1032,   1, 'ACTION', 'A', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'RADIO', NIL, NIL }, { 1003,   1, NIL, '<', { 'PROMPT', 'ITEMS', 'OF', 'WINDOW', 'DIALOG', 'HELPID', 'HELP ID', 'ON CLICK', 'ON CHANGE', 'COLOR', 'MESSAGE', 'UPDATE', 'WHEN', 'SIZE', 'VALID', 'DESIGN', '3D', '_3D', 'PIXEL' } }, {    0,  -1, 'VAR', NIL, NIL }, { 1004,   1, NIL, '<', { 'PROMPT', 'ITEMS', 'OF', 'WINDOW', 'DIALOG', 'HELPID', 'HELP ID', 'ON CLICK', 'ON CHANGE', 'COLOR', 'MESSAGE', 'UPDATE', 'WHEN', 'SIZE', 'VALID', 'DESIGN', '3D', '_3D', 'PIXEL' } }, {    5,   1, NIL, ':', { 'PROMPT', 'ITEMS' } }, {    6,  -1, NIL, 'A', NIL }, {    7,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    8,  -1, NIL, '<', NIL }, { 1009,   1, NIL, ':', { 'HELPID', 'HELP ID' } }, { 1010,  -1, NIL, 'A', NIL }, {   11,   1, NIL, ':', { 'ON CLICK', 'ON CHANGE' } }, {   12,  -1, NIL, '<', NIL }, {   13,   1, 'COLOR', '<', NIL }, {   14,   2, ',', '<', NIL }, {   15,   1, 'MESSAGE', '<', NIL }, {   16,   1, NIL, ':', { 'UPDATE' } }, {   17,   1, 'WHEN', '<', NIL }, {   18,   1, 'SIZE', '<', NIL }, {   19,  -1, ',', '<', NIL }, {   20,   1, 'VALID', '<', NIL }, {   21,   1, NIL, ':', { 'DESIGN' } }, {   22,   1, NIL, ':', { '3D', '_3D' } }, {   23,   1, NIL, ':', { 'PIXEL' } } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'RADIO', NIL, NIL }, { 1001,   1, NIL, '<', { 'ID', 'OF', 'WINDOW', 'DIALOG', 'HELPID', 'HELP ID', 'ON CHANGE', 'ON CLICK', 'COLOR', 'MESSAGE', 'UPDATE', 'WHEN', 'VALID' } }, {    0,  -1, 'VAR', NIL, NIL }, { 1002,   1, NIL, '<', { 'ID', 'OF', 'WINDOW', 'DIALOG', 'HELPID', 'HELP ID', 'ON CHANGE', 'ON CLICK', 'COLOR', 'MESSAGE', 'UPDATE', 'WHEN', 'VALID' } }, {    3,   1, 'ID', 'A', NIL }, {    4,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    5,  -1, NIL, '<', NIL }, { 1006,   1, NIL, ':', { 'HELPID', 'HELP ID' } }, { 1007,  -1, NIL, 'A', NIL }, {    8,   1, NIL, ':', { 'ON CHANGE', 'ON CLICK' } }, {    9,  -1, NIL, '<', NIL }, {   10,   1, 'COLOR', '<', NIL }, {   11,   2, ',', '<', NIL }, {   12,   1, 'MESSAGE', '<', NIL }, {   13,   1, NIL, ':', { 'UPDATE' } }, {   14,   1, 'WHEN', '<', NIL }, {   15,   1, 'VALID', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'BITMAP', NIL, NIL }, { 1003,   1, NIL, '<', { 'NAME', 'RESNAME', 'RESOURCE', 'FILENAME', 'FILE', 'DISK', 'NOBORDER', 'NO BORDER', 'SIZE', 'OF', 'WINDOW', 'DIALOG', 'ON CLICK', 'ON LEFT CLICK', 'ON RIGHT CLICK', 'SCROLL', 'ADJUST', 'CURSOR', 'PIXEL', 'MESSAGE', 'UPDATE', 'WHEN', 'VALID', 'DESIGN' } }, {    4,   1, NIL, ':', { 'NAME', 'RESNAME', 'RESOURCE' } }, {    5,  -1, NIL, '<', NIL }, {    6,   1, NIL, ':', { 'FILENAME', 'FILE', 'DISK' } }, {    7,  -1, NIL, '<', NIL }, {    8,   1, NIL, ':', { 'NOBORDER', 'NO BORDER' } }, {    9,   1, 'SIZE', '<', NIL }, {   10,  -1, ',', '<', NIL }, {   11,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {   12,  -1, NIL, '<', NIL }, { 1013,   1, NIL, ':', { 'ON CLICK', 'ON LEFT CLICK' } }, { 1014,  -1, NIL, '<', NIL }, { 1015,   1, NIL, ':', { 'ON RIGHT CLICK' } }, { 1016,  -1, NIL, '<', NIL }, {   17,   1, NIL, ':', { 'SCROLL' } }, {   18,   1, NIL, ':', { 'ADJUST' } }, {   19,   1, 'CURSOR', '<', NIL }, {   20,   1, NIL, ':', { 'PIXEL' } }, {   21,   1, 'MESSAGE', '<', NIL }, {   22,   1, NIL, ':', { 'UPDATE' } }, {   23,   1, 'WHEN', '<', NIL }, {   24,   1, 'VALID', '<', NIL }, {   25,   1, NIL, ':', { 'DESIGN' } } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'IMAGE', NIL, NIL }, { 1003,   1, NIL, '<', { 'NAME', 'RESNAME', 'RESOURCE', 'FILENAME', 'FILE', 'DISK', 'NOBORDER', 'NO BORDER', 'SIZE', 'OF', 'WINDOW', 'DIALOG', 'ON CLICK', 'ON LEFT CLICK', 'ON RIGHT CLICK', 'SCROLL', 'ADJUST', 'CURSOR', 'PIXEL', 'MESSAGE', 'UPDATE', 'WHEN', 'VALID', 'DESIGN' } }, {    4,   1, NIL, ':', { 'NAME', 'RESNAME', 'RESOURCE' } }, {    5,  -1, NIL, '<', NIL }, {    6,   1, NIL, ':', { 'FILENAME', 'FILE', 'DISK' } }, {    7,  -1, NIL, '<', NIL }, {    8,   1, NIL, ':', { 'NOBORDER', 'NO BORDER' } }, {    9,   1, 'SIZE', '<', NIL }, {   10,  -1, ',', '<', NIL }, {   11,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {   12,  -1, NIL, '<', NIL }, { 1013,   1, NIL, ':', { 'ON CLICK', 'ON LEFT CLICK' } }, { 1014,  -1, NIL, '<', NIL }, { 1015,   1, NIL, ':', { 'ON RIGHT CLICK' } }, { 1016,  -1, NIL, '<', NIL }, {   17,   1, NIL, ':', { 'SCROLL' } }, {   18,   1, NIL, ':', { 'ADJUST' } }, {   19,   1, 'CURSOR', '<', NIL }, {   20,   1, NIL, ':', { 'PIXEL' } }, {   21,   1, 'MESSAGE', '<', NIL }, {   22,   1, NIL, ':', { 'UPDATE' } }, {   23,   1, 'WHEN', '<', NIL }, {   24,   1, 'VALID', '<', NIL }, {   25,   1, NIL, ':', { 'DESIGN' } } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'BITMAP', NIL, NIL }, { 1001,   1, NIL, '<', { 'ID', 'OF', 'WINDOW', 'DIALOG', 'NAME', 'RESNAME', 'RESOURCE', 'FILE', 'FILENAME', 'DISK', 'ON CLICK', 'ON LEFT CLICK', 'ON RIGHT CLICK', 'SCROLL', 'ADJUST', 'CURSOR', 'MESSAGE', 'UPDATE', 'WHEN', 'VALID', 'TRANSPAREN' } }, {    2,   1, 'ID', '<', NIL }, {    3,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    4,  -1, NIL, '<', NIL }, {    5,   1, NIL, ':', { 'NAME', 'RESNAME', 'RESOURCE' } }, {    6,  -1, NIL, '<', NIL }, {    7,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    8,  -1, NIL, '<', NIL }, { 1009,   1, NIL, ':', { 'ON CLICK', 'ON LEFT CLICK' } }, { 1010,  -1, NIL, '<', NIL }, { 1011,   1, NIL, ':', { 'ON RIGHT CLICK' } }, { 1012,  -1, NIL, '<', NIL }, {   13,   1, NIL, ':', { 'SCROLL' } }, {   14,   1, NIL, ':', { 'ADJUST' } }, {   15,   1, 'CURSOR', '<', NIL }, {   16,   1, 'MESSAGE', '<', NIL }, {   17,   1, NIL, ':', { 'UPDATE' } }, {   18,   1, 'WHEN', '<', NIL }, {   19,   1, 'VALID', '<', NIL }, {   20,   1, NIL, ':', { 'TRANSPAREN' } } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    0,   0, 'BITMAP', NIL, NIL }, { 1001,   1, NIL, '<', { 'RESOURCE', 'NAME', 'RESNAME', 'FILE', 'FILENAME', 'DISK', 'OF', 'WINDOW', 'DIALOG' } }, {    2,   1, NIL, ':', { 'RESOURCE', 'NAME', 'RESNAME' } }, {    3,  -1, NIL, '<', NIL }, {    4,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    5,  -1, NIL, '<', NIL }, {    6,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    7,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'SAY', NIL, NIL }, { 1001,   1, NIL, '<', { 'PROMPT', 'VAR', 'PICTURE', 'ID', 'OF', 'WINDOW', 'DIALOG', 'COLOR', 'COLORS', 'UPDATE', 'FONT' } }, {    2,   1, NIL, ':', { 'PROMPT', 'VAR' } }, {    3,  -1, NIL, '<', NIL }, {    4,   1, 'PICTURE', '<', NIL }, {    5,   1, 'ID', '<', NIL }, {    6,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    7,  -1, NIL, '<', NIL }, {    8,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {    9,  -1, NIL, '<', NIL }, {   10,   2, ',', '<', NIL }, {   11,   1, NIL, ':', { 'UPDATE' } }, {   12,   1, 'FONT', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'SAY', NIL, NIL }, { 1003,   1, NIL, '<', NIL }, {    4,  -1, NIL, ':', { 'PROMPT', 'VAR' } }, {    5,   0, NIL, '<', NIL }, { 1006,   1, 'PICTURE', '<', NIL }, { 1007,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, { 1008,  -1, NIL, '<', NIL }, {    9,   1, 'FONT', '<', NIL }, {   10,   1, NIL, ':', { 'CENTERED', 'CENTER' } }, {   11,   1, NIL, ':', { 'RIGHT' } }, {   12,   1, NIL, ':', { 'BORDER' } }, {   13,   1, NIL, ':', { 'PIXEL', 'PIXELS' } }, {   14,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   15,  -1, NIL, '<', NIL }, {   16,   2, ',', '<', NIL }, {   17,   1, 'SIZE', '<', NIL }, {   18,  -1, ',', '<', NIL }, {   19,   1, NIL, ':', { 'DESIGN' } }, {   20,   1, NIL, ':', { 'UPDATE' } }, {   21,   1, NIL, ':', { 'SHADED', 'SHADOW' } }, {   22,   1, NIL, ':', { 'BOX' } }, {   23,   1, NIL, ':', { 'RAISED' } } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'GET', NIL, NIL }, { 1001,   1, NIL, '<', NIL }, {    0,  -1, 'VAR', NIL, NIL }, {    2,   0, NIL, '<', NIL }, {    3,   1, NIL, ':', { 'MULTILINE', 'MEMO', 'TEXT' } }, {    4,   1, 'ID', '<', NIL }, {    5,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    6,  -1, NIL, '<', NIL }, {    7,   1, NIL, ':', { 'HELPID', 'HELP ID' } }, {    8,  -1, NIL, '<', NIL }, {    9,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   10,  -1, NIL, '<', NIL }, {   11,   2, ',', '<', NIL }, {   12,   1, 'FONT', '<', NIL }, {   13,   1, 'CURSOR', '<', NIL }, {   14,   1, 'MESSAGE', '<', NIL }, {   15,   1, NIL, ':', { 'UPDATE' } }, {   16,   1, 'WHEN', '<', NIL }, {   17,   1, NIL, ':', { 'READONLY', 'NO MODIFY' } }, {   18,   1, 'VALID', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1019,  -1, 'CHANGE', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'GET', NIL, NIL }, { 1001,   1, NIL, '<', NIL }, {    0,  -1, 'VAR', NIL, NIL }, {    2,   0, NIL, '<', NIL }, {    3,   1, 'ID', '<', NIL }, {    4,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    5,  -1, NIL, '<', NIL }, {    6,   1, NIL, ':', { 'HELPID', 'HELP ID' } }, {    7,  -1, NIL, '<', NIL }, {    8,   1, 'VALID', '<', NIL }, {    9,   1, 'PICTURE', '<', NIL }, {   10,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   11,  -1, NIL, '<', NIL }, {   12,   2, ',', '<', NIL }, {   13,   1, 'FONT', '<', NIL }, {   14,   1, 'CURSOR', '<', NIL }, {   15,   1, 'MESSAGE', '<', NIL }, {   16,   1, NIL, ':', { 'UPDATE' } }, {   17,   1, 'WHEN', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1018,  -1, 'CHANGE', '<', NIL }, {   19,   1, NIL, ':', { 'READONLY', 'NO MODIFY' } }, {   20,   1, NIL, ':', { 'SPINNER' } }, {    0,   2, 'ON', NIL, NIL }, {   21,  -2, 'UP', '<', NIL }, {    0,   2, 'ON', NIL, NIL }, {   22,  -2, 'DOWN', '<', NIL }, {   23,   2, 'MIN', '<', NIL }, {   24,   2, 'MAX', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'GET', NIL, NIL }, { 1003,   1, NIL, '<', NIL }, {    0,  -1, 'VAR', NIL, NIL }, {    4,   0, NIL, '<', NIL }, { 1005,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, { 1006,  -1, NIL, '<', NIL }, {    7,   1, NIL, ':', { 'MULTILINE', 'MEMO', 'TEXT' } }, {    8,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {    9,  -1, NIL, '<', NIL }, {   10,   2, ',', '<', NIL }, {   11,   1, 'SIZE', '<', NIL }, {   12,  -1, ',', '<', NIL }, {   13,   1, 'FONT', '<', NIL }, {   14,   1, NIL, ':', { 'HSCROLL' } }, {   15,   1, 'CURSOR', '<', NIL }, {   16,   1, NIL, ':', { 'PIXEL' } }, {   17,   1, 'MESSAGE', '<', NIL }, {   18,   1, NIL, ':', { 'UPDATE' } }, {   19,   1, 'WHEN', '<', NIL }, {   20,   1, NIL, ':', { 'CENTER', 'CENTERED' } }, {   21,   1, NIL, ':', { 'RIGHT' } }, {   22,   1, NIL, ':', { 'READONLY', 'NO MODIFY' } }, {   23,   1, 'VALID', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1024,  -1, 'CHANGE', '<', NIL }, {   25,   1, NIL, ':', { 'DESIGN' } }, { 1026,   1, NIL, ':', { 'NO BORDER', 'NOBORDER' } }, { 1027,   1, NIL, ':', { 'NO VSCROLL' } } } , .F. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'GET', NIL, NIL }, { 1003,   1, NIL, '<', NIL }, {    0,  -1, 'VAR', NIL, NIL }, {    4,   0, NIL, '<', NIL }, { 1005,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, { 1006,  -1, NIL, '<', NIL }, {    7,   1, 'PICTURE', '<', NIL }, {    8,   1, 'VALID', '<', NIL }, {    9,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   10,  -1, NIL, '<', NIL }, {   11,   2, ',', '<', NIL }, {   12,   1, 'SIZE', '<', NIL }, {   13,  -1, ',', '<', NIL }, {   14,   1, 'FONT', '<', NIL }, {   15,   1, NIL, ':', { 'DESIGN' } }, {   16,   1, 'CURSOR', '<', NIL }, {   17,   1, NIL, ':', { 'PIXEL' } }, {   18,   1, 'MESSAGE', '<', NIL }, {   19,   1, NIL, ':', { 'UPDATE' } }, {   20,   1, 'WHEN', '<', NIL }, {   21,   1, NIL, ':', { 'CENTER', 'CENTERED' } }, {   22,   1, NIL, ':', { 'RIGHT' } }, { 1000,   1, 'ON', NIL, NIL }, { 1023,  -1, 'CHANGE', '<', NIL }, {   24,   1, NIL, ':', { 'READONLY', 'NO MODIFY' } }, {   25,   1, NIL, ':', { 'PASSWORD' } }, { 1026,   1, NIL, ':', { 'NO BORDER', 'NOBORDER' } }, {   27,   1, NIL, ':', { 'HELPID', 'HELP ID' } }, {   28,  -1, NIL, '<', NIL } } , .F. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'GET', NIL, NIL }, { 1003,   1, NIL, '<', NIL }, {    0,  -1, 'VAR', NIL, NIL }, {    4,   0, NIL, '<', NIL }, { 1005,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, { 1006,  -1, NIL, '<', NIL }, {    7,   1, 'PICTURE', '<', NIL }, {    8,   1, 'VALID', '<', NIL }, {    9,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   10,  -1, NIL, '<', NIL }, {   11,   2, ',', '<', NIL }, {   12,   1, 'SIZE', '<', NIL }, {   13,  -1, ',', '<', NIL }, {   14,   1, 'FONT', '<', NIL }, {   15,   1, NIL, ':', { 'DESIGN' } }, {   16,   1, 'CURSOR', '<', NIL }, {   17,   1, NIL, ':', { 'PIXEL' } }, {   18,   1, 'MESSAGE', '<', NIL }, {   19,   1, NIL, ':', { 'UPDATE' } }, {   20,   1, 'WHEN', '<', NIL }, {   21,   1, NIL, ':', { 'CENTER', 'CENTERED' } }, {   22,   1, NIL, ':', { 'RIGHT' } }, { 1000,   1, 'ON', NIL, NIL }, { 1023,  -1, 'CHANGE', '<', NIL }, {   24,   1, NIL, ':', { 'READONLY', 'NO MODIFY' } }, {   25,   1, NIL, ':', { 'HELPID', 'HELP ID' } }, {   26,  -1, NIL, '<', NIL }, {   27,   1, NIL, ':', { 'SPINNER' } }, {    0,   2, 'ON', NIL, NIL }, {   28,  -2, 'UP', '<', NIL }, {    0,   2, 'ON', NIL, NIL }, {   29,  -2, 'DOWN', '<', NIL }, {   30,   2, 'MIN', '<', NIL }, {   31,   2, 'MAX', '<', NIL } } , .F. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'SCROLLBAR', NIL, NIL }, { 1003,   1, NIL, '<', { 'HORIZONTAL', 'VERTICAL', 'RANGE', 'PAGESTEP', 'SIZE', 'UP', 'ON UP', 'DOWN', 'ON DOWN', 'PAGEUP', 'ON PAGEUP', 'PAGEDOWN', 'ON PAGEDOWN', 'ON THUMBPOS', 'PIXEL', 'COLOR', 'COLORS', 'OF', 'MESSAGE', 'UPDATE', 'WHEN', 'VALID', 'DESIGN' } }, {    4,   1, NIL, ':', { 'HORIZONTAL' } }, { 1005,   1, NIL, ':', { 'VERTICAL' } }, {    6,   1, 'RANGE', '<', NIL }, {    7,  -1, ',', '<', NIL }, {    8,   1, 'PAGESTEP', '<', NIL }, {    9,   1, 'SIZE', '<', NIL }, {   10,  -1, ',', '<', NIL }, { 1011,   1, NIL, ':', { 'UP', 'ON UP' } }, { 1012,  -1, NIL, '<', NIL }, { 1013,   1, NIL, ':', { 'DOWN', 'ON DOWN' } }, { 1014,  -1, NIL, '<', NIL }, { 1015,   1, NIL, ':', { 'PAGEUP', 'ON PAGEUP' } }, { 1016,  -1, NIL, '<', NIL }, { 1017,   1, NIL, ':', { 'PAGEDOWN', 'ON PAGEDOWN' } }, { 1018,  -1, NIL, '<', NIL }, { 1019,   1, NIL, ':', { 'ON THUMBPOS' } }, { 1020,  -1, NIL, '<', NIL }, { 1021,   1, NIL, ':', { 'PIXEL' } }, {   22,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   23,  -1, NIL, '<', NIL }, {   24,   2, ',', '<', NIL }, {   25,   1, 'OF', '<', NIL }, {   26,   1, 'MESSAGE', '<', NIL }, {   27,   1, NIL, ':', { 'UPDATE' } }, {   28,   1, 'WHEN', '<', NIL }, {   29,   1, 'VALID', '<', NIL }, {   30,   1, NIL, ':', { 'DESIGN' } } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    0,   0, 'SCROLLBAR', NIL, NIL }, { 1001,   1, NIL, '<', { 'HORIZONTAL', 'VERTICAL', 'RANGE', 'PAGESTEP', 'UP', 'ON UP', 'DOWN', 'ON DOWN', 'PAGEUP', 'ON PAGEUP', 'PAGEDOWN', 'ON PAGEDOWN', 'ON THUMBPOS', 'COLOR', 'COLORS', 'OF', 'WINDOW', 'DIALOG', 'MESSAGE', 'UPDATE', 'WHEN', 'VALID' } }, {    2,   1, NIL, ':', { 'HORIZONTAL' } }, { 1003,   1, NIL, ':', { 'VERTICAL' } }, {    4,   1, 'RANGE', '<', NIL }, {    5,  -1, ',', '<', NIL }, {    6,   1, 'PAGESTEP', '<', NIL }, { 1007,   1, NIL, ':', { 'UP', 'ON UP' } }, { 1008,  -1, NIL, '<', NIL }, { 1009,   1, NIL, ':', { 'DOWN', 'ON DOWN' } }, { 1010,  -1, NIL, '<', NIL }, { 1011,   1, NIL, ':', { 'PAGEUP', 'ON PAGEUP' } }, { 1012,  -1, NIL, '<', NIL }, { 1013,   1, NIL, ':', { 'PAGEDOWN', 'ON PAGEDOWN' } }, { 1014,  -1, NIL, '<', NIL }, { 1015,   1, NIL, ':', { 'ON THUMBPOS' } }, { 1016,  -1, NIL, '<', NIL }, {   17,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   18,  -1, NIL, '<', NIL }, {   19,   2, ',', '<', NIL }, {   20,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {   21,  -1, NIL, '<', NIL }, {   22,   1, 'MESSAGE', '<', NIL }, {   23,   1, NIL, ':', { 'UPDATE' } }, {   24,   1, 'WHEN', '<', NIL }, {   25,   1, 'VALID', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'SCROLLBAR', NIL, NIL }, { 1001,   1, NIL, '<', { 'ID', 'RANGE', 'PAGESTEP', 'UP', 'ON UP', 'ON LEFT', 'DOWN', 'ON DOWN', 'ON RIGHT', 'PAGEUP', 'ON PAGEUP', 'PAGEDOWN', 'ON PAGEDOWN', 'ON THUMBPOS', 'COLOR', 'COLORS', 'OF', 'MESSAGE', 'UPDATE', 'WHEN', 'VALID' } }, {    2,   1, 'ID', '<', NIL }, {    3,   1, 'RANGE', '<', NIL }, {    4,  -1, ',', '<', NIL }, {    5,   1, 'PAGESTEP', '<', NIL }, { 1006,   1, NIL, ':', { 'UP', 'ON UP', 'ON LEFT' } }, { 1007,  -1, NIL, '<', NIL }, { 1008,   1, NIL, ':', { 'DOWN', 'ON DOWN', 'ON RIGHT' } }, { 1009,  -1, NIL, '<', NIL }, { 1010,   1, NIL, ':', { 'PAGEUP', 'ON PAGEUP' } }, { 1011,  -1, NIL, '<', NIL }, { 1012,   1, NIL, ':', { 'PAGEDOWN', 'ON PAGEDOWN' } }, { 1013,  -1, NIL, '<', NIL }, { 1014,   1, NIL, ':', { 'ON THUMBPOS' } }, { 1015,  -1, NIL, '<', NIL }, {   16,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   17,  -1, NIL, '<', NIL }, {   18,   2, ',', '<', NIL }, {   19,   1, 'OF', '<', NIL }, {   20,   1, 'MESSAGE', '<', NIL }, {   21,   1, NIL, ':', { 'UPDATE' } }, {   22,   1, 'WHEN', '<', NIL }, {   23,   1, 'VALID', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, { 1003,   1, 'GROUP', '<', NIL }, {    4,   0, 'TO', '<', NIL }, {    5,   0, ',', '<', NIL }, {    6,   1, NIL, ':', { 'LABEL', 'PROMPT' } }, {    7,  -1, NIL, '<', NIL }, {    8,   1, 'OF', '<', NIL }, {    9,   1, 'COLOR', '<', NIL }, {   10,   2, ',', '<', NIL }, {   11,   1, NIL, ':', { 'PIXEL' } }, { 1012,   1, NIL, ':', { 'DESIGN' } }, { 1013,   1, 'FONT', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'GROUP', NIL, NIL }, { 1001,   1, NIL, '<', { 'LABEL', 'PROMPT', 'ID', 'OF', 'WINDOW', 'DIALOG', 'COLOR', 'FONT' } }, {    2,   1, NIL, ':', { 'LABEL', 'PROMPT' } }, {    3,  -1, NIL, '<', NIL }, {    4,   1, 'ID', '<', NIL }, {    5,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    6,  -1, NIL, '<', NIL }, {    7,   1, 'COLOR', '<', NIL }, {    8,   2, ',', '<', NIL }, { 1009,   1, 'FONT', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'METER', NIL, NIL }, { 1003,   1, NIL, '<', NIL }, {    0,  -1, 'VAR', NIL, NIL }, {    4,   0, NIL, '<', NIL }, {    5,   1, 'TOTAL', '<', NIL }, {    6,   1, 'SIZE', '<', NIL }, {    7,  -1, ',', '<', NIL }, {    8,   1, 'OF', '<', NIL }, {    9,   1, NIL, ':', { 'UPDATE' } }, {   10,   1, NIL, ':', { 'PIXEL' } }, {   11,   1, 'FONT', '<', NIL }, {   12,   1, 'PROMPT', '<', NIL }, {   13,   1, NIL, ':', { 'NOPERCENTAGE' } }, {   14,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   15,  -1, NIL, '<', NIL }, {   16,  -1, ',', '<', NIL }, {   17,   1, 'BARCOLOR', '<', NIL }, {   18,  -1, ',', '<', NIL }, {   19,   1, NIL, ':', { 'DESIGN' } } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'METER', NIL, NIL }, { 1001,   1, NIL, '<', NIL }, {    0,  -1, 'VAR', NIL, NIL }, {    2,   0, NIL, '<', NIL }, {    3,   1, 'TOTAL', '<', NIL }, {    4,   1, 'ID', '<', NIL }, {    5,   1, 'OF', '<', NIL }, {    6,   1, NIL, ':', { 'UPDATE' } }, {    7,   1, 'FONT', '<', NIL }, {    8,   1, 'PROMPT', '<', NIL }, {    9,   1, NIL, ':', { 'NOPERCENTAGE' } }, {   10,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   11,  -1, NIL, '<', NIL }, {   12,  -1, ',', '<', NIL }, {   13,   1, 'BARCOLOR', '<', NIL }, {   14,  -1, ',', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'METAFILE', NIL, NIL }, { 1003,   1, NIL, '<', { 'FILE', 'FILENAME', 'DISK', 'OF', 'WINDOW', 'DIALOG', 'SIZE', 'COLOR', 'COLORS' } }, {    4,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    5,  -1, NIL, '<', NIL }, {    6,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    7,  -1, NIL, '<', NIL }, {    8,   1, 'SIZE', '<', NIL }, {    9,  -1, ',', '<', NIL }, {   10,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   11,  -1, NIL, '<', NIL }, {   12,   2, ',', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'METAFILE', NIL, NIL }, { 1001,   1, NIL, '<', { 'ID', 'FILE', 'FILENAME', 'DISK', 'OF', 'WINDOW', 'DIALOG', 'COLOR', 'COLORS' } }, {    2,   1, 'ID', '<', NIL }, {    3,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    4,  -1, NIL, '<', NIL }, {    5,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    6,  -1, NIL, '<', NIL }, {    7,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {    8,  -1, NIL, '<', NIL }, {    9,   2, ',', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, 'CURSOR', '<', NIL }, {    2,   1, NIL, ':', { 'RESOURCE', 'RESNAME', 'NAME' } }, {    3,  -1, NIL, '<', NIL }, { 1004,   1, NIL, ':', { 'ARROW', 'ICON', 'SIZENS', 'SIZEWE', 'SIZENWSE', 'SIZENESW', 'IBEAM', 'CROSS', 'SIZE', 'UPARROW', 'WAIT', 'HAND' } } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    0,   0, 'WINDOW', NIL, NIL }, { 1001,   1, NIL, '<', { 'MDICHILD', 'FROM', 'TITLE', 'BRUSH', 'CURSOR', 'MENU', 'MENUINFO', 'ICON', 'OF', 'VSCROLL', 'VERTICAL SCROLL', 'HSCROLL', 'HORIZONTAL SCROLL', 'COLOR', 'COLORS', 'PIXEL', 'STYLE', 'HELPID', 'HELP ID', 'BORDER', 'NOSYSMENU', 'NO SYSMENU', 'NOCAPTION', 'NO CAPTION', 'NO TITLE', 'NOICONIZE', 'NOMINIMIZE', 'NOZOOM', 'NO ZOOM', 'NOMAXIMIZE', 'NO MAXIMIZE' } }, {    0,   1, 'MDICHILD', NIL, NIL }, {    2,   1, 'FROM', '<', NIL }, {    3,  -1, ',', '<', NIL }, {    4,  -1, 'TO', '<', NIL }, {    5,  -1, ',', '<', NIL }, {    6,   1, 'TITLE', '<', NIL }, {    7,   1, 'BRUSH', '<', NIL }, {    8,   1, 'CURSOR', '<', NIL }, {    9,   1, 'MENU', '<', NIL }, { 1010,   1, 'MENUINFO', '<', NIL }, {   11,   1, 'ICON', '<', NIL }, {   12,   1, 'OF', '<', NIL }, {   13,   1, NIL, ':', { 'VSCROLL', 'VERTICAL SCROLL' } }, {   14,   1, NIL, ':', { 'HSCROLL', 'HORIZONTAL SCROLL' } }, {   15,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   16,  -1, NIL, '<', NIL }, {   17,   2, ',', '<', NIL }, {   18,   1, NIL, ':', { 'PIXEL' } }, {   19,   1, 'STYLE', '<', NIL }, {   20,   1, NIL, ':', { 'HELPID', 'HELP ID' } }, {   21,  -1, NIL, '<', NIL }, { 1022,   1, 'BORDER', ':', { 'NONE', 'SINGLE' } }, {   23,   1, NIL, ':', { 'NOSYSMENU', 'NO SYSMENU' } }, {   24,   1, NIL, ':', { 'NOCAPTION', 'NO CAPTION', 'NO TITLE' } }, {   25,   1, NIL, ':', { 'NOICONIZE', 'NOMINIMIZE' } }, {   26,   1, NIL, ':', { 'NOZOOM', 'NO ZOOM', 'NOMAXIMIZE', 'NO MAXIMIZE' } } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, 'WINDOW', '<', NIL }, {    2,   1, 'FROM', '<', NIL }, {    3,  -1, ',', '<', NIL }, {    4,  -1, 'TO', '<', NIL }, {    5,  -1, ',', '<', NIL }, {    6,   1, 'TITLE', '<', NIL }, {    7,   1, 'STYLE', '<', NIL }, {    8,   1, 'MENU', '<', NIL }, {    9,   1, 'BRUSH', '<', NIL }, {   10,   1, 'ICON', '<', NIL }, {    0,   1, 'MDI', NIL, NIL }, {   11,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   12,  -1, NIL, '<', NIL }, {   13,   2, ',', '<', NIL }, { 1014,   1, NIL, ':', { 'VSCROLL', 'VERTICAL SCROLL' } }, { 1015,   1, NIL, ':', { 'HSCROLL', 'HORIZONTAL SCROLL' } }, {   16,   1, 'MENUINFO', '<', NIL }, { 1000,   2, 'BORDER', NIL, NIL }, { 1017,  -1, NIL, ':', { 'NONE', 'SINGLE' } }, {   18,   1, 'OF', '<', NIL }, { 1019,   1, NIL, ':', { 'PIXEL' } } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, 'WINDOW', '<', NIL }, {    2,   1, 'FROM', '<', NIL }, {    3,  -1, ',', '<', NIL }, {    4,  -1, 'TO', '<', NIL }, {    5,  -1, ',', '<', NIL }, {    6,   2, NIL, ':', { 'PIXEL' } }, {    7,   1, 'TITLE', '<', NIL }, {    8,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {    9,  -1, NIL, '<', NIL }, {   10,   2, ',', '<', NIL }, {   11,   1, 'OF', '<', NIL }, {   12,   1, 'BRUSH', '<', NIL }, {   13,   1, 'CURSOR', '<', NIL }, {   14,   1, 'ICON', '<', NIL }, {   15,   1, 'MENU', '<', NIL }, {   16,   1, 'STYLE', '<', NIL }, { 1017,   1, 'BORDER', ':', { 'NONE', 'SINGLE' } }, {   18,   1, NIL, ':', { 'NOSYSMENU', 'NO SYSMENU' } }, {   19,   1, NIL, ':', { 'NOCAPTION', 'NO CAPTION', 'NO TITLE' } }, {   20,   1, NIL, ':', { 'NOICONIZE', 'NOMINIMIZE' } }, {   21,   1, NIL, ':', { 'NOZOOM', 'NO ZOOM', 'NOMAXIMIZE', 'NO MAXIMIZE' } }, { 1022,   1, NIL, ':', { 'VSCROLL', 'VERTICAL SCROLL' } }, { 1023,   1, NIL, ':', { 'HSCROLL', 'HORIZONTAL SCROLL' } } } , .T. } )
   aAdd( aCommRules, { 'ACTIVATE' , { {    1,   0, 'WINDOW', '<', NIL }, { 1002,   1, NIL, ':', { 'ICONIZED', 'NORMAL', 'MAXIMIZED' } }, {    0,   1, 'ON', NIL, NIL }, { 1000,   2, 'LEFT', NIL, NIL }, { 1003,  -1, 'CLICK', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1004,  -1, 'LBUTTONUP', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1000,  -1, 'RIGHT', NIL, NIL }, { 1005,  -1, 'CLICK', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1006,  -1, 'MOVE', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1007,  -1, 'RESIZE', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1008,  -1, 'PAINT', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1009,  -1, 'KEYDOWN', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1010,  -1, 'INIT', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1011,  -1, 'UP', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1012,  -1, 'DOWN', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1013,  -1, 'PAGEUP', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1014,  -1, 'PAGEDOWN', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1015,  -1, 'LEFT', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1016,  -1, 'RIGHT', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1017,  -1, 'PAGELEFT', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1018,  -1, 'PAGERIGHT', '<', NIL }, { 1000,   1, 'ON', NIL, NIL }, { 1019,  -1, 'DROPFILES', '<', NIL }, { 1020,   1, 'VALID', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'SET' , { {    0,   0, 'MESSAGE', NIL, NIL }, {    1,   1, 'OF', '<', NIL }, {    2,   1, 'TO', '<', NIL }, {    3,   1, NIL, ':', { 'CENTER', 'CENTERED' } }, {    4,   1, NIL, ':', { 'CLOCK', 'TIME' } }, {    5,   1, NIL, ':', { 'DATE' } }, {    6,   1, NIL, ':', { 'KEYBOARD' } }, {    7,   1, 'FONT', '<', NIL }, {    8,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {    9,  -1, NIL, '<', NIL }, {   10,   2, ',', '<', NIL }, { 1011,   1, NIL, ':', { 'NO INSET', 'NOINSET' } } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, NIL, ':', { 'MESSAGE', 'MESSAGE BAR', 'MSGBAR' } }, { 1002,   1, NIL, '<', { 'OF', 'PROMPT', 'TITLE', 'CENTER', 'CENTERED', 'CLOCK', 'TIME', 'DATE', 'KEYBOARD', 'FONT', 'COLOR', 'COLORS', 'NO INSET', 'NOINSET' } }, {    3,   1, 'OF', '<', NIL }, {    4,   1, NIL, ':', { 'PROMPT', 'TITLE' } }, {    5,  -1, NIL, '<', NIL }, {    6,   1, NIL, ':', { 'CENTER', 'CENTERED' } }, {    7,   1, NIL, ':', { 'CLOCK', 'TIME' } }, {    8,   1, NIL, ':', { 'DATE' } }, {    9,   1, NIL, ':', { 'KEYBOARD' } }, {   10,   1, 'FONT', '<', NIL }, {   11,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {   12,  -1, NIL, '<', NIL }, {   13,   2, ',', '<', NIL }, { 1014,   1, NIL, ':', { 'NO INSET', 'NOINSET' } } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    0,   0, 'MSGITEM', NIL, NIL }, { 1001,   1, NIL, '<', { 'OF', 'PROMPT', 'SIZE', 'FONT', 'COLOR', 'COLORS', 'BITMAP', 'BITMAPS', 'ACTION', 'TOOLTIP' } }, {    2,   1, 'OF', '<', NIL }, {    3,   1, 'PROMPT', '<', NIL }, {    4,   1, 'SIZE', '<', NIL }, {    5,   1, 'FONT', '<', NIL }, {    6,   1, NIL, ':', { 'COLOR', 'COLORS' } }, {    7,  -1, NIL, '<', NIL }, {    8,   2, ',', '<', NIL }, { 1009,   1, NIL, ':', { 'BITMAP', 'BITMAPS' } }, { 1010,  -1, NIL, '<', NIL }, { 1011,   2, ',', '<', NIL }, { 1012,   1, 'ACTION', '<', NIL }, { 1013,   1, 'TOOLTIP', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    1,   0, 'CLIPBOARD', '<', NIL }, { 1002,   1, 'FORMAT', ':', { 'TEXT', 'OEMTEXT', 'BITMAP', 'DIF' } }, {    3,   1, 'OF', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ACTIVATE' , { {    1,   0, 'CLIPBOARD', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'DEFINE' , { {    0,   0, 'TIMER', NIL, NIL }, { 1001,   1, NIL, '<', { 'INTERVAL', 'ACTION', 'OF', 'WINDOW', 'DIALOG' } }, {    2,   1, 'INTERVAL', '<', NIL }, { 1003,   1, 'ACTION', 'A', NIL }, {    4,   1, NIL, ':', { 'OF', 'WINDOW', 'DIALOG' } }, {    5,  -1, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ACTIVATE' , { {    1,   0, 'TIMER', '<', NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    0,   0, 'VBX', NIL, NIL }, { 1003,   1, NIL, '<', { 'OF', 'SIZE', 'FILE', 'FILENAME', 'DISK', 'CLASS', 'ON', 'WHEN', 'VALID', 'PIXEL', 'DESIGN' } }, {    4,   1, 'OF', '<', NIL }, {    5,   1, 'SIZE', '<', NIL }, {    6,  -1, ',', '<', NIL }, {    7,   1, NIL, ':', { 'FILE', 'FILENAME', 'DISK' } }, {    8,  -1, NIL, '<', NIL }, {    9,   1, 'CLASS', '<', NIL }, { 1010,   1, 'ON', '<', NIL }, { 1011,  -1, NIL, '<', NIL }, { 1012,   2, 'ON', '<', NIL }, { 1013,  -2, NIL, '<', NIL }, { 1014,   1, 'WHEN', '<', NIL }, { 1015,   1, 'VALID', '<', NIL }, {   16,   1, NIL, ':', { 'PIXEL' } }, {   17,   1, NIL, ':', { 'DESIGN' } } } , .T. } )
   aAdd( aCommRules, { 'REDEFINE' , { {    0,   0, 'VBX', NIL, NIL }, { 1001,   1, NIL, '<', { 'ID', 'OF', 'COLOR', 'ON' } }, {    2,   1, 'ID', '<', NIL }, {    3,   1, 'OF', '<', NIL }, {    4,   1, 'COLOR', '<', NIL }, {    5,   2, ',', '<', NIL }, { 1006,   1, 'ON', '<', NIL }, { 1007,  -1, NIL, '<', NIL }, { 1008,   2, 'ON', '<', NIL }, { 1009,  -2, NIL, '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'OBJECT' , { {    1,   0, NIL, '<', NIL }, {    2,   0, 'AS', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'ENDOBJECT' ,  , .T. } )
   aAdd( aCommRules, { 'CLS' ,  , .T. } )
   aAdd( aCommRules, { 'CLEAR' , { {    0,   0, 'SCREEN', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { '?' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { '??' , { { 1001,   1, NIL, 'A', NIL } } , .F. } )
   aAdd( aCommRules, { 'READ' ,  , .T. } )
   aAdd( aCommRules, { 'SAVE' , { {    0,   0, 'SCREEN', NIL, NIL }, {    1,   1, 'TO', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'RESTORE' , { {    0,   0, 'SCREEN', NIL, NIL }, {    1,   1, 'FROM', '<', NIL } } , .T. } )
   aAdd( aCommRules, { 'SAVESCREEN' , { {    1,   0, '(', '*', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { 'RESTSCREEN' , { {    1,   0, '(', '*', NIL }, {    0,   0, ')', NIL, NIL } } , .T. } )
   aAdd( aCommRules, { '@' , { {    1,   0, NIL, '<', NIL }, {    2,   0, ',', '<', NIL }, {    3,   0, 'PROMPT', '*', NIL } } , .T. } )
   aAdd( aCommRules, { 'MENU' , { {    1,   0, 'TO', '<', NIL } } , .T. } )

RETURN .T.

//--------------------------------------------------------------//
STATIC FUNCTION InitFWResults()

   /* Defines Results*/
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { { {   0, '"(c) FiveTech, 1993-2001"' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '"FWH Pre-release - April 2001"' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '"FiveWin for Harbour"' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WBrowse' } }, { -1} ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '5' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '6' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '7' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '9' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '10' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '11' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '12' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '13' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '14' } }, { -1} ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8388608' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '32768' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8421376' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '128' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8388736' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '32896' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '12632256' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'CLR_HGRAY' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8421504' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '16711680' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '65280' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '16776960' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '255' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '16711935' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '65535' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '16777215' } }, { -1} ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '7' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '7' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '5' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '6' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '7' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '9' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '10' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '11' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '12' } }, { -1} ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { { {   0, '992' } }, { -1} ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '9' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '12' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '13' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '16' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '17' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '18' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '19' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '20' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '27' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '32' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '33' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '34' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '35' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '36' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '37' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '38' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '39' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '40' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '41' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '42' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '43' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '44' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '45' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '46' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '47' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '96' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '97' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '98' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '99' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '100' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '101' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '102' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '103' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '104' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '105' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '106' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '107' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '108' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '109' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '110' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '111' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '112' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '113' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '114' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '115' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '116' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '117' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '118' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '119' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '120' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '121' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '122' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '123' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '124' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '125' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '126' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '127' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '128' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '129' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '130' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '131' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '132' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '133' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '134' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '135' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '144' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '145' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '16' } }, { -1} ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1024' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1025' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1026' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1027' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1028' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1029' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1030' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1031' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1032' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1033' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1034' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1035' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1036' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1037' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1038' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1039' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1040' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1041' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1042' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, 'WM_USER+1043' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '16384' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '32' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '64' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '128' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4096' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8192' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2147483648' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1073741824' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '67108864' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '33554432' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '268435456' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '134217728' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '536870912' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '16777216' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '12582912' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8388608' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4194304' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2097152' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1048576' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '524288' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '262144' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '131072' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '65536' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '131072' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '65536' } }, { -1} ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '128' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2048' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4096' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '5' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '7' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '15' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '16' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '17' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '18' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '21' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '22' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '23' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '26' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '27' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '29' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '30' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '42' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '65' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '135' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '258' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '273' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '512' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '513' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '514' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '516' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '517' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '256' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '257' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '272' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '275' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '276' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '277' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '783' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '784' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '785' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1024' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '128' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '128' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '16' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '128' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '256' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1024' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4096' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '10485763' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '16' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '64' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '128' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '256' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2048' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '5' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '6' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '6' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '7' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '7' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '8' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '7' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '9' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '1' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '2' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '3' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '5' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '6' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '4' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '6' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '9' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '0' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '11' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '240' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '15000' } }, { -1} ,  } )
   aAdd( aDefResults, { , ,  } )
   aAdd( aDefResults, { { {   0, 'Chr(13)+Chr(10)' } }, { -1} ,  } )
   aAdd( aDefResults, { { {   0, '{' }, {   0, '|' }, {   0, 'u' }, {   0, '|' }, {   0, 'If' }, {   0, '(' }, {   0, 'PCount' }, {   0, '(' }, {   0, ')' }, {   0, '==' }, {   0, '0' }, {   0, ',' }, {   0,   1 }, {   0, ',' }, {   0,   1 }, {   0, ':=' }, {   0, 'u' }, {   0, ')' }, {   0, '}' } }, { -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  1, -1,  1, -1, -1, -1, -1} , { NIL }  } )

   /* Translates Results*/
   aAdd( aTransResults, { { {   0, '( ' }, {   0,   1 }, {   0, ' + ( ' }, {   0,   2 }, {   0, ' * 256 ) + ( ' }, {   0,   3 }, {   0, ' * 65536 ) )' } }, { -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aTransResults, { { {   0,   1 } }, {  1} , { NIL }  } )
   aAdd( aTransResults, { { {   0, 'DLL' } }, { -1} ,  } )
   aAdd( aTransResults, { { {   0, '{ |bp1,bp2,bp3,bp4,bp5,bp6,bp7,bp8,bp9,bp10| ' }, {   0,   1 }, {   0, ' }' } }, { -1,  1, -1} , { NIL }  } )

   /* Commands Results*/
   aAdd( aCommResults, { , , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   2, 'SetResources( ' }, {   2,   2 }, {   2, ' ); ' }, {   0, ' SetResources( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'FreeResources()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'SetHelpFile( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'HelpSetTopic( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := LoadValue( ' }, {   0,   4 }, {   0, ', ' }, {   2, 'Upper(' }, {   2,   2 }, {   2, ')' }, {   0, ', ' }, {   0,   1 }, {   0, ' )' } }, {  1, -1,  1, -1, -1,  4, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' = TDialog():New( ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ', ' }, {   0,  25 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':Activate( ' }, {   0,   1 }, {   0, ':bLClicked ' }, {   6, ':= {|nRow,nCol,nFlags|' }, {   6,   6 }, {   6, '}' }, {   0, ', ' }, {   0,   1 }, {   0, ':bMoved    ' }, {   8, ':= ' }, {   8,   8 }, {   0, ', ' }, {   0,   1 }, {   0, ':bPainted  ' }, {   9, ':= {|hDC,cPS|' }, {   9,   9 }, {   9, '}' }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   5, '{|Self|' }, {   5,   5 }, {   5, '}' }, {   0, ', ' }, {   3, '! ' }, {   3,   3 }, {   0, ', ' }, {   7, '{|Self|' }, {   7,   7 }, {   7, '}' }, {   0, ', ' }, {   0,   1 }, {   0, ':bRClicked ' }, {  10, ':= {|nRow,nCol,nFlags|' }, {  10,  10 }, {  10, '}' }, {   0, ', ' }, {   4, '{|Self|' }, {   4,   4 }, {   4, '}' }, {   0, ' )' } }, {  1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  5, -1,  1, -1, -1,  1, -1, -1,  6, -1, -1,  1, -1, -1, -1,  6, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1, -1,  1, -1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TFont():New( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   5,   5 }, {   0, ', ' }, {   6,   6 }, {   0, ',' }, {  11,  11 }, {   0, ',,' }, {   9,   9 }, {   0, ', ' }, {   7,   7 }, {   0, ', ' }, {   8,   8 }, {   0, ',,,,,, ' }, {  10,  10 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':Activate()' } }, {  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':DeActivate()' } }, {  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':SetFont( ' }, {   0,   2 }, {   0, ' )' } }, {  1, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TIni():New( ' }, {   0,   3 }, {   0, ' )' } }, {  1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := ' }, {   0,   6 }, {   0, ':Get( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   1 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   5 }, {   0, ':Set( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { , ,  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' MenuBegin( ' }, {   0,   2 }, {   0, ' )' } }, {  1, -1, -1,  6, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' MenuAddItem( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   5, 'Upper(' }, {   5,   5 }, {   5, ') == "ENABLED" ' }, {   0, ', ' }, {  10, '{|oMenuItem|' }, {  10,  10 }, {  10, '}' }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {  19,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  6, -1, -1,  4, -1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1,  5, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TMru():New( ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   8, '{|cMruItem,oMenuItem|' }, {   8,   8 }, {   8, '}' }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ':=' }, {   0, ' MenuAddItem()' } }, {  1, -1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'MenuEnd()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TMenu():ReDefine( ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, {  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TMenuItem():ReDefine( ' }, {   0,   2 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {  10, 'Upper(' }, {  10,  10 }, {  10, ') == "ENABLED" ' }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,   1 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {  19,  19 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  6, -1, -1,  4, -1, -1,  5, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TMenu():New( .f., ' }, {   0,   2 }, {   0, ' )' } }, {  1, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':SetMenu( ' }, {   0,   2 }, {   0, ' )' } }, {  1, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   2 }, {   0, ':Activate( ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   6 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' :=' }, {   0, ' MenuBegin( .f., .t., ' }, {   0,   3 }, {   0, ' )' } }, {  1, -1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'MenuEnd()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' PrintBegin( ' }, {   3,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   6 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  6, -1,  6, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' PrintBegin( ' }, {   3,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   6 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  6, -1,  6, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'PageBegin()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PageEnd()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PrintEnd()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'PrintEnd()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   1,   1 }, {   0, ' function ' }, {   0,   2 }, {   0, '( ' }, {   3, 'NOREF(' }, {   3,   3 }, {   3, ')' }, {   5, ' ,NOREF(' }, {   5,   5 }, {   5, ')' }, {   0, ' ) ; local hDLL := If( ValType( ' }, {   0,  10 }, {   0, ' ) == "N", ' }, {   0,  10 }, {   0, ', LoadLibrary( ' }, {   0,  10 }, {   0, ' ) ) ; local uResult ; local cFarProc ; if Abs( hDLL ) > 32 ; cFarProc = GetProcAddress( hDLL, If( ' }, {   9, ' Empty( ' }, {   9,   9 }, {   9, ' ) == ' }, {   0, ' .t., ' }, {   0,   2 }, {   0, ', ' }, {   0,   9 }, {   0, ' ), ' }, {   8,   8 }, {   0, ', ' }, {   0,   7 }, {   4, ' ,' }, {   4,   4 }, {   6, ' ,' }, {   6,   6 }, {   0, ' ) ; uResult = CallDLL( cFarProc ' }, {   3, ' ,' }, {   3,   3 }, {   5, ' ,' }, {   5,   5 }, {   0, ' ) ; If( ValType( ' }, {   0,  10 }, {   0, ' ) == "N",, FreeLibrary( hDLL ) ) ; else ; MsgAlert( "Error code: " + LTrim( Str( hDLL ) ) + " loading " + ' }, {   0,  10 }, {   0, ' ) ; end ; return uResult' } }, {  1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  4, -1, -1,  1, -1, -1,  4, -1,  1, -1,  6, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   0, ' function ' }, {   0,   2 }, {   0, '( ' }, {   3, 'NOREF(' }, {   3,   3 }, {   3, ')' }, {   5, ' ,NOREF(' }, {   5,   5 }, {   5, ')' }, {   0, ' ) ; local hDLL := If( ValType( ' }, {   0,  10 }, {   0, ' ) == "N", ' }, {   0,  10 }, {   0, ', LoadLib32( ' }, {   0,  10 }, {   0, ' ) ) ; local uResult ; local cFarProc ; if Abs( hDLL ) <= 32 ; MsgAlert( "Error code: " + LTrim( Str( hDLL ) ) + " loading " + ' }, {   0,  10 }, {   0, ' ) ; else ; cFarProc = GetProc32( hDLL, If( ' }, {   9, ' Empty( ' }, {   9,   9 }, {   9, ' ) == ' }, {   0, ' .t., ' }, {   0,   2 }, {   0, ', ' }, {   0,   9 }, {   0, ' ), ' }, {   8,   8 }, {   0, ', ' }, {   0,   7 }, {   4, ' ,' }, {   4,   4 }, {   6, ' ,' }, {   6,   6 }, {   0, ' ) ; uResult = CallDLL32( cFarProc ' }, {   3, ' ,' }, {   3,   3 }, {   5, ' ,' }, {   5,   5 }, {   0, ' ) ; If( ValType( ' }, {   0,  10 }, {   0, ' ) == "N",, FreeLib32( hDLL ) ) ; end ; return uResult' } }, {  1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  4, -1,  1, -1, -1,  1, -1, -1,  4, -1,  1, -1,  6, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TFolder():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   7, '{' }, {   7,   7 }, {   7, '}' }, {   0, ', {' }, {   0,   9 }, {  10, ' ,' }, {  10,  10 }, {   0, '}, ' }, {   0,   5 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TFolder():ReDefine( ' }, {   0,   2 }, {   0, ', ' }, {   6, '{' }, {   6,   6 }, {   6, '}' }, {   0, ', { ' }, {   0,   8 }, {   9, ' ,' }, {   9,   9 }, {   0, ' }, ' }, {   0,   4 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {  14, '{|nOption,nOldOption| ' }, {  14,  14 }, {  14, '}' }, {   0, ', ' }, {   0,  15 }, {   0, ' )' } }, {  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TTabs():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   7, '{' }, {   7,   7 }, {   7, '}' }, {   0, ', ' }, {   9, '{|nOption|' }, {   9,   9 }, {   9, '}' }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1, -1,  1, -1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TTabs():ReDefine( ' }, {   0,   2 }, {   0, ', ' }, {   6, '{' }, {   6,   6 }, {   6, '}' }, {   0, ', ' }, {   8, '{|nOption|' }, {   8,   8 }, {   8, '}' }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ' )' } }, {  1, -1, -1,  1, -1, -1,  1, -1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TPages():Redefine( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   4, '{' }, {   4,   4 }, {   4, '}' }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   6, 'bSETGET(' }, {   6,   6 }, {   6, ') ' }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TOdbc():New( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':Execute( ' }, {   0,   3 }, {   0, ' )' } }, {  1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   2 }, {   0, ' := TDde():New( ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   6,   6 }, {   0, ', ' }, {   7,   7 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   2 }, {   0, ':Activate()' } }, {  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   0, ' := TMci():New( "avivideo", ' }, {   0,   3 }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':lOpen() ; ' }, {   0,   1 }, {   0, ':Play()' } }, {  1, -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':lOpen() ; ' }, {   0,   1 }, {   0, ':Play()' } }, {  1, -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TVideo():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TVideo():ReDefine( ' }, {   0,   2 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ':=' }, {   0, ' TreeBegin( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' :=' }, {   0, ' _TreeItem( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'TreeEnd()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'SetMultiple( Upper(' }, {   0,   1 }, {   0, ') == "ON" )' } }, { -1,  4, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := If( ' }, {   0,   1 }, {   0, ' == nil, ' }, {   0,   2 }, {   0, ', ' }, {   0,   1 }, {   0, ' ) ; ' }, {   3,   3 }, {   3, ' := If( ' }, {   3,   3 }, {   3, ' == nil, ' }, {   3,   4 }, {   3, ', ' }, {   3,   3 }, {   3, ' ); ' } }, {  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'while .t.' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'if ' }, {   0,   1 }, {   0, '; exit; end; end' } }, { -1,  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'SetIdleAction( ' }, {   0,   1 }, {   0, ' )' } }, { -1,  5, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TDataBase():New()' } }, {  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0,   2 }, {   0, ':End() ; ' }, {   0,   2 }, {   0, ' := nil ' }, {   3, ' ; ' }, {   3,   3 }, {   3, ':End() ; ' }, {   3,   3 }, {   3, ' := nil ' } }, {  1, -1,  1, -1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TBrush():New( ' }, {   2, ' Upper(' }, {   2,   2 }, {   2, ') ' }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, {  1, -1, -1, -1,  4, -1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':SetBrush( ' }, {   0,   2 }, {   0, ' )' } }, {  1, -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TPen():New( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   6 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':Activate()' } }, {  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TBar():New( ' }, {   0,   8 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   6, 'Upper(' }, {   6,   6 }, {   6, ') ' }, {   0, ', ' }, {   0,   9 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  6, -1, -1,  4, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TBar():NewAt( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   9, 'Upper(' }, {   9,   9 }, {   9, ') ' }, {   0, ', ' }, {   0,  12 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1, -1,  4, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TBtnBmp():NewBar( ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {  13, '{|This|' }, {  13,  13 }, {  13, '}' }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {  20, '{||' }, {  20,  20 }, {  20, '}' }, {   0, ', ' }, {  13, "'" }, {  13,  13 }, {  13, "'" }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ', ' }, {   7,   7 }, {   0, ', ' }, {  11,  11 }, {   0, ', ' }, {  24, '!' }, {  24,  24 }, {   0, ', ' }, {  25,  25 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1, -1,  6, -1,  1, -1,  6, -1,  5, -1,  1, -1,  6, -1, -1,  1, -1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TBtnBmp():ReDefine( ' }, {   0,   2 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {  14, '{|Self|' }, {  14,  14 }, {  14, '}' }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   8,   8 }, {   0, ', ' }, {  12,  12 }, {   0, ', ' }, {  22, '!' }, {  22,  22 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1, -1,  1, -1,  6, -1,  5, -1,  6, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TBtnBmp():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {  14, '{|Self|' }, {  14,  14 }, {  14, '}' }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   7,   7 }, {   0, ', ' }, {  11,  11 }, {   0, ', !' }, {   0,  23 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  5, -1,  6, -1,  6, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TIcon():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  5, -1,  1, -1,  6, -1,  5, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TIcon():ReDefine( ' }, {   0,   2 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  11 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  6, -1,  1, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TIcon():New( ,, ' }, {   0,   3 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TButton():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  6, -1,  1, -1,  6, -1,  5, -1,  5, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TButton():ReDefine( ' }, {   0,   2 }, {   0, ', ' }, {   5, '{||' }, {   5,   5 }, {   5, '}' }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ' )' } }, {  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  5, -1,  5, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TCheckBox():ReDefine( ' }, {   0,   3 }, {   0, ', bSETGET(' }, {   0,   2 }, {   0, '), ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   9,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  1, -1,  1, -1,  1, -1,  6, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TCheckBox():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   4, 'bSETGET(' }, {   4,   4 }, {   4, ')' }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {  14,  14 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  1, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1,  6, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TComboBox():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', bSETGET(' }, {   0,   4 }, {   0, '), ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {  13, '{|Self|' }, {  13,  13 }, {  13, '}' }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ', ' }, {   0,  24 }, {   0, ', ' }, {  25, '{|nItem|' }, {  25,  25 }, {  25, '}' }, {   0, ', ' }, {   0,  26 }, {   0, ', ' }, {   0,  27 }, {   0, ', ' }, {  28,  28 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1, -1,  5, -1,  1, -1,  1, -1,  6, -1,  1, -1,  1, -1,  6, -1,  5, -1,  6, -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TComboBox():ReDefine( ' }, {   0,   5 }, {   0, ', bSETGET(' }, {   0,   2 }, {   0, '), ' }, {   0,   4 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {  10, '{|Self|' }, {  10,  10 }, {  10, '}' }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {  19, '{|nItem|' }, {  19,  19 }, {  19, '}' }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {  22,  22 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  5, -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TListBox():ReDefine( ' }, {   0,   7 }, {   0, ', bSETGET(' }, {   0,   2 }, {   0, '), ' }, {   0,   4 }, {   0, ', ' }, {   8, '{||' }, {   8,   8 }, {   8, '}' }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {  22, '{|nItem|' }, {  22,  22 }, {  22, '}' }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  1, -1,  1, -1,  1, -1,  5, -1,  1, -1,  6, -1,  5, -1, -1,  1, -1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TListBox():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', bSETGET(' }, {   0,   4 }, {   0, '), ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ', ' }, {  24, '{|nItem|' }, {  24,  24 }, {  24, '}' }, {   0, ', ' }, {   0,  25 }, {   0, ', ' }, {   0,  26 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  1, -1,  5, -1,  1, -1,  1, -1,  6, -1,  6, -1,  5, -1,  1, -1,  1, -1,  6, -1,  5, -1,  1, -1, -1,  1, -1, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TWBrowse():ReDefine( ' }, {   0,   4 }, {   0, ', ' }, {   2, '{|| { ' }, {   2,   2 }, {   2, ' } }' }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {  10, '{' }, {  10,  10 }, {  10, '}' }, {   0, ', ' }, {   8, '{' }, {   8,   8 }, {   8, '}' }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {  14,  14 }, {   0, ', ' }, {  16, '{|nRow,nCol,nFlags|' }, {  16,  16 }, {  16, '}' }, {   0, ', ' }, {  17, '{|nRow,nCol,nFlags|' }, {  17,  17 }, {  17, '}' }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ', ' }, {   0,  24 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,  25 }, {   0, ', ' }, {   0,  26 }, {   0, ', ' }, {  15, '{|nRow,nCol,nFlags|' }, {  15,  15 }, {  15, '}' }, {   0, ', ' }, {  27, '{' }, {  27,  27 }, {  27, '}' }, {   0, ' )' } }, {  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1, -1,  1, -1, -1,  4, -1,  1, -1,  1, -1,  5, -1, -1,  1, -1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1,  5, -1,  5, -1, -1,  1, -1, -1, -1,  5, -1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TWBrowse():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   4, '{|| {' }, {   4,   4 }, {   4, ' } }' }, {   0, ', ' }, {   9, '{' }, {   9,   9 }, {   9, '}' }, {   0, ', ' }, {   7, '{' }, {   7,   7 }, {   7, '}' }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {  17,  17 }, {   0, ', ' }, {  19, '{|nRow,nCol,nFlags|' }, {  19,  19 }, {  19, '}' }, {   0, ', ' }, {  20, '{|nRow,nCol,nFlags|' }, {  20,  20 }, {  20, '}' }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  24 }, {   0, ', ' }, {   0,  25 }, {   0, ', ' }, {   0,  26 }, {   0, ', ' }, {   0,  27 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,  28 }, {   0, ', ' }, {   0,  29 }, {   0, ', ' }, {   0,  30 }, {   0, ', ' }, {   0,  31 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {  32, '{' }, {  32,  32 }, {  32, '}' }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1, -1, -1,  1, -1, -1, -1,  1, -1, -1,  1, -1,  4, -1,  1, -1,  1, -1,  5, -1, -1,  1, -1, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1,  6, -1,  5, -1,  6, -1,  5, -1,  5, -1, -1,  5, -1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TRadMenu():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', {' }, {   0,   6 }, {   0, '}, ' }, {   4, 'bSETGET(' }, {   4,   4 }, {   4, ')' }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {  10, '{' }, {  10,  10 }, {  10, '}' }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  5, -1,  1, -1,  1, -1,  1, -1,  6, -1,  5, -1,  1, -1,  1, -1,  5, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TRadMenu():Redefine( ' }, {   2, ' bSETGET(' }, {   2,   2 }, {   2, ')' }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   7, '{' }, {   7,   7 }, {   7, '}' }, {   0, ', { ' }, {   0,   3 }, {   0, ' }, ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ' )' } }, {  1, -1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1,  5, -1,  1, -1,  1, -1,  1, -1,  6, -1,  5, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TBitmap():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {  14, '{ |nRow,nCol,nKeyFlags| ' }, {  14,  14 }, {  14, ' } ' }, {   0, ', ' }, {  16, '{ |nRow,nCol,nKeyFlags| ' }, {  16,  16 }, {  16, ' } ' }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  24 }, {   0, ', ' }, {   0,  25 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1, -1,  1, -1, -1, -1,  1, -1, -1,  6, -1,  6, -1,  1, -1,  1, -1,  6, -1,  5, -1,  6, -1,  5, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TImage():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {  14, '{ |nRow,nCol,nKeyFlags| ' }, {  14,  14 }, {  14, ' } ' }, {   0, ', ' }, {  16, '{ |nRow,nCol,nKeyFlags| ' }, {  16,  16 }, {  16, ' } ' }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  24 }, {   0, ', ' }, {   0,  25 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1, -1,  1, -1, -1, -1,  1, -1, -1,  6, -1,  6, -1,  1, -1,  1, -1,  6, -1,  5, -1,  6, -1,  5, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TBitmap():ReDefine( ' }, {   0,   2 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {  10, '{ |nRow,nCol,nKeyFlags| ' }, {  10,  10 }, {  10, ' }' }, {   0, ', ' }, {  12, '{ |nRow,nCol,nKeyFlags| ' }, {  12,  12 }, {  12, ' }' }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1, -1,  1, -1, -1, -1,  1, -1, -1,  6, -1,  6, -1,  1, -1,  1, -1,  6, -1,  5, -1,  5, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TBitmap():Define( ' }, {   0,   3 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TSay():ReDefine( ' }, {   0,   5 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  5, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TSay():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   8,   8 }, {   0, ', ' }, {   6,   6 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  5, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TMultiGet():ReDefine( ' }, {   0,   4 }, {   0, ', bSETGET(' }, {   0,   2 }, {   0, '), ' }, {   0,   6 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {  19, '{|nKey, nFlags, Self| ' }, {  19,  19 }, {  19, '}' }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  5, -1,  6, -1,  5, -1, -1,  1, -1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TGet():ReDefine( ' }, {   0,   3 }, {   0, ', bSETGET(' }, {   0,   2 }, {   0, '), ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {  18, '{|nKey,nFlags,Self| ' }, {  18,  18 }, {  18, ' }' }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ', ' }, {   0,  24 }, {   0, ')' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  5, -1, -1,  1, -1, -1,  6, -1,  6, -1,  5, -1,  5, -1,  5, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TMultiGet():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', bSETGET(' }, {   0,   4 }, {   0, '), ' }, {   6,   6 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ', ' }, {  24, '{|nKey, nFlags, Self| ' }, {  24,  24 }, {  24, '}' }, {   0, ', ' }, {   0,  25 }, {   0, ', ' }, {  26,  26 }, {   0, ', ' }, {  27,  27 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1,  6, -1,  5, -1,  6, -1,  6, -1,  6, -1,  5, -1, -1,  1, -1, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TGet():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', bSETGET(' }, {   0,   4 }, {   0, '), ' }, {   6,   6 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {  23, '{|nKey, nFlags, Self| ' }, {  23,  23 }, {  23, '}' }, {   0, ', ' }, {   0,  24 }, {   0, ', ' }, {   0,  25 }, {   0, ', ' }, {  26,  26 }, {   0, ', ' }, {   0,  28 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1,  6, -1,  1, -1,  6, -1,  5, -1,  6, -1,  6, -1, -1,  1, -1, -1,  6, -1,  6, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TGet():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', bSETGET(' }, {   0,   4 }, {   0, '), ' }, {   6,   6 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {  23, '{|nKey, nFlags, Self| ' }, {  23,  23 }, {  23, '}' }, {   0, ', ' }, {   0,  24 }, {   0, ', .f., .f., ' }, {   0,  26 }, {   0, ', ' }, {   0,  27 }, {   0, ', ' }, {   0,  28 }, {   0, ', ' }, {   0,  29 }, {   0, ', ' }, {   0,  30 }, {   0, ', ' }, {   0,  31 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1,  6, -1,  1, -1,  6, -1,  5, -1,  6, -1,  6, -1, -1,  1, -1, -1,  6, -1,  1, -1,  6, -1,  5, -1,  5, -1,  5, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TScrollBar():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', (.not.' }, {   0,   4 }, {   0, ') ' }, {   5, '.or. ' }, {   5,   5 }, {   0, ', ' }, {   0,  25 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ' , ' }, {  12,  12 }, {   0, ', ' }, {  14,  14 }, {   0, ', ' }, {  16,  16 }, {   0, ', ' }, {  18,  18 }, {   0, ', ' }, {  20, '{|nPos| ' }, {  20,  20 }, {  20, ' }' }, {   0, ', ' }, {  21,  21 }, {   0, ', ' }, {   0,  23 }, {   0, ', ' }, {   0,  24 }, {   0, ', ' }, {   0,  26 }, {   0, ', ' }, {   0,  27 }, {   0, ', ' }, {   0,  28 }, {   0, ', ' }, {   0,  29 }, {   0, ', ' }, {   0,  30 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1, -1,  6, -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  5, -1,  5, -1, -1,  1, -1, -1,  6, -1,  1, -1,  1, -1,  1, -1,  6, -1,  5, -1,  5, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TScrollBar():WinNew( ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', (.not.' }, {   0,   2 }, {   0, ') ' }, {   3, '.or. ' }, {   3,   3 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   8,   8 }, {   0, ', ' }, {  10,  10 }, {   0, ', ' }, {  12,  12 }, {   0, ', ' }, {  14,  14 }, {   0, ', ' }, {  16, '{|nPos| ' }, {  16,  16 }, {  16, ' }' }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ', ' }, {   0,  24 }, {   0, ', ' }, {   0,  25 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  6, -1, -1,  6, -1,  1, -1,  5, -1,  5, -1,  5, -1,  5, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  5, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TScrollBar():Redefine( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   7,   7 }, {   0, ', ' }, {   9,   9 }, {   0, ', ' }, {  11,  11 }, {   0, ', ' }, {  13,  13 }, {   0, ', ' }, {  15, '{|nPos| ' }, {  15,  15 }, {  15, ' }' }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  20 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {   0,  22 }, {   0, ', ' }, {   0,  23 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  5, -1,  5, -1,  5, -1, -1,  1, -1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  5, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TGroup():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {  12,  12 }, {   0, ', ' }, {  13,  13 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TGroup():ReDefine( ' }, {   0,   4 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   9,   9 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TMeter():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', bSETGET(' }, {   0,   4 }, {   0, '), ' }, {   0,   5 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  19 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1,  1, -1,  6, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TMeter():ReDefine( ' }, {   0,   4 }, {   0, ', bSETGET(' }, {   0,   2 }, {   0, '), ' }, {   0,   3 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  14 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1,  1, -1,  6, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TMetaFile():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  12 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TMetaFile():Redefine( ' }, {   0,   2 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TCursor():New( ' }, {   0,   3 }, {   0, ', ' }, {   4, 'Upper(' }, {   4,   4 }, {   4, ') ' }, {   0, ' )' } }, {  1, -1,  1, -1, -1,  4, -1, -1} , { NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TMdiChild():New( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,  19 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  21 }, {   0, ', ' }, {  22, 'Upper(' }, {  22,  22 }, {  22, ')' }, {   0, ', !' }, {   0,  23 }, {   0, ', !' }, {   0,  24 }, {   0, ', !' }, {   0,  25 }, {   0, ', !' }, {   0,  26 }, {   0, ', ' }, {  10,  10 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1, -1,  4, -1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TMdiFrame():New( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {  14,  14 }, {   0, ', ' }, {  15,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {  17, 'Upper(' }, {  17,  17 }, {  17, ')' }, {   0, ', ' }, {   0,  18 }, {   0, ', ' }, {  19,  19 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1, -1,  4, -1, -1,  1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TWindow():New( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  15 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  14 }, {   0, ', ' }, {   0,  11 }, {   0, ', ' }, {  22,  22 }, {   0, ', ' }, {  23,  23 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {  17, 'Upper(' }, {  17,  17 }, {  17, ')' }, {   0, ', !' }, {   0,  18 }, {   0, ', !' }, {   0,  19 }, {   0, ', !' }, {   0,  20 }, {   0, ', !' }, {   0,  21 }, {   0, ', ' }, {   0,   6 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  1, -1,  1, -1,  1, -1, -1,  4, -1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':Activate( ' }, {   2, 'Upper(' }, {   2,   2 }, {   2, ') ' }, {   0, ', ' }, {   0,   1 }, {   0, ':bLClicked ' }, {   3, ':= { |nRow,nCol,nKeyFlags| ' }, {   3,   3 }, {   3, ' } ' }, {   0, ', ' }, {   0,   1 }, {   0, ':bRClicked ' }, {   5, ':= { |nRow,nCol,nKeyFlags| ' }, {   5,   5 }, {   5, ' } ' }, {   0, ', ' }, {   0,   1 }, {   0, ':bMoved    ' }, {   6, ':= ' }, {   6,   6 }, {   0, ', ' }, {   0,   1 }, {   0, ':bResized  ' }, {   7, ':= ' }, {   7,   7 }, {   0, ', ' }, {   0,   1 }, {   0, ':bPainted  ' }, {   8, ':= { | hDC, cPS | ' }, {   8,   8 }, {   8, ' } ' }, {   0, ', ' }, {   0,   1 }, {   0, ':bKeyDown  ' }, {   9, ':= { | nKey | ' }, {   9,   9 }, {   9, ' } ' }, {   0, ', ' }, {   0,   1 }, {   0, ':bInit     ' }, {  10, ':= { | Self | ' }, {  10,  10 }, {  10, ' } ' }, {   0, ', ' }, {  11,  11 }, {   0, ', ' }, {  12,  12 }, {   0, ', ' }, {  13,  13 }, {   0, ', ' }, {  14,  14 }, {   0, ', ' }, {  15,  15 }, {   0, ', ' }, {  16,  16 }, {   0, ', ' }, {  17,  17 }, {   0, ', ' }, {  18,  18 }, {   0, ', ' }, {  20,  20 }, {   0, ', ' }, {  19, '{|nRow,nCol,aFiles|' }, {  19,  19 }, {  19, '}' }, {   0, ', ' }, {   0,   1 }, {   0, ':bLButtonUp ' }, {   4, ':= ' }, {   4,   4 }, {   0, ' )' } }, {  1, -1, -1,  4, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  5, -1,  1, -1, -1,  5, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1, -1,  5, -1,  5, -1,  5, -1,  5, -1,  5, -1,  5, -1,  5, -1,  5, -1,  5, -1, -1,  1, -1, -1,  1, -1, -1,  5, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':oMsgBar := TMsgBar():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {  11, '!' }, {  11,  11 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  1, -1,  1, -1,  1, -1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   2,   2 }, {   2, ':=' }, {   0,   3 }, {   0, ':oMsgBar := TMsgBar():New( ' }, {   0,   3 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', ' }, {   0,  12 }, {   0, ', ' }, {   0,  13 }, {   0, ', ' }, {   0,  10 }, {   0, ', ' }, {  14, '!' }, {  14,  14 }, {   0, ' )' } }, {  1, -1,  1, -1,  1, -1,  1, -1,  6, -1,  6, -1,  6, -1,  6, -1,  1, -1,  1, -1,  1, -1, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ':=' }, {   0, ' TMsgItem():New( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   7 }, {   0, ', ' }, {   0,   8 }, {   0, ', .t., ' }, {  12,  12 }, {   0, ', ' }, {  10,  10 }, {   0, ', ' }, {  11,  11 }, {   0, ', ' }, {  13,  13 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  5, -1,  1, -1,  1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ' := TClipBoard():New( ' }, {   2, ' Upper(' }, {   2,   2 }, {   2, ')' }, {   0, ', ' }, {   0,   3 }, {   0, ' )' } }, {  1, -1, -1,  4, -1, -1,  1, -1} , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':Open()' } }, {  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TTimer():New( ' }, {   0,   2 }, {   0, ', ' }, {   3, '{||' }, {   3,   3 }, {   3, '}' }, {   0, ', ' }, {   0,   5 }, {   0, ' )' } }, {  1, -1, -1,  1, -1, -1,  1, -1, -1,  1, -1} , { NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0,   1 }, {   0, ':Activate()' } }, {  1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   3,   3 }, {   3, ' := ' }, {   0, ' TVbControl():New( ' }, {   0,   1 }, {   0, ', ' }, {   0,   2 }, {   0, ', ' }, {   0,   5 }, {   0, ', ' }, {   0,   6 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   8 }, {   0, ', ' }, {   0,   9 }, {   0, ', { ' }, {  10,  10 }, {  10, ', _PARM_BLOCK_10_( ' }, {  10,  11 }, {  10, ' ) ' }, {  12, ' ,' }, {  12,  12 }, {  12, ', _PARM_BLOCK_10_( ' }, {  12,  13 }, {  12, ' ) ' }, {   0, ' }, ' }, {  14,  14 }, {   0, ', ' }, {  15,  15 }, {   0, ', ' }, {   0,  16 }, {   0, ', ' }, {   0,  17 }, {   0, ' )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  4, -1,  1, -1, -1,  4, -1,  1, -1, -1,  5, -1,  5, -1,  6, -1,  6, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   1,   1 }, {   1, ' := ' }, {   0, ' TVbControl():ReDefine( ' }, {   0,   2 }, {   0, ', ' }, {   0,   3 }, {   0, ', ' }, {   0,   4 }, {   0, ', ' }, {   0,   5 }, {   0, ', { ' }, {   6,   6 }, {   6, ', _PARM_BLOCK_10_( ' }, {   6,   7 }, {   6, ' ) ' }, {   8, ' ,' }, {   8,   8 }, {   8, ', _PARM_BLOCK_10_( ' }, {   8,   9 }, {   8, ' ) ' }, {   0, ' } )' } }, {  1, -1, -1,  1, -1,  1, -1,  1, -1,  1, -1,  4, -1,  1, -1, -1,  4, -1,  1, -1, -1} , { NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'Self := SetObject( Self, { || ' }, {   0,   2 }, {   0, '():New() } )' } }, { -1,  1, -1} , { NIL, NIL }  } )
   aAdd( aCommResults, { { {   0, 'Self := EndObject()' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'InvalidateRect( GetActiveWindow(), 0, .t. )' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'InvalidateRect( GetActiveWindow(), 0, .t. )' } }, { -1} ,  } )
   aAdd( aCommResults, { { {   0, 'WQout( ' }, {   1, '{ ' }, {   1,   1 }, {   1, ' } ' }, {   0, ' )' } }, { -1, -1,  1, -1, -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'WQout( ' }, {   1, '{ ' }, {   1,   1 }, {   1, ' } ' }, {   0, ' )' } }, { -1, -1,  1, -1, -1} , { NIL }  } )
   aAdd( aCommResults, { , ,  } )
   aAdd( aCommResults, { , , { NIL }  } )
   aAdd( aCommResults, { , , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'MsgAlert( OemToAnsi( "SaveScreen() not available in FiveWin" ) )' } }, { -1} , { NIL }  } )
   aAdd( aCommResults, { { {   0, 'MsgAlert( OemToAnsi( "RestScreen() not available in FiveWin" ) )' } }, { -1} , { NIL }  } )
   aAdd( aCommResults, { , , { NIL, NIL, NIL }  } )
   aAdd( aCommResults, { , , { NIL }  } )

RETURN .T.

//--------------------------------------------------------------//
