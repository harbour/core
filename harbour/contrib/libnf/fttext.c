/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Nanforum Toolkit simulation of text file handlers
 * Ideas by Brice de Ganahl and Steve Larsen
 * Total rework using xHarbour array implementation by Andi Jahja
 *
 * Copyright 2005 Andi Jahja <andijahja@xharbour.com>
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

#include "hbapi.h"
#include "hbapifs.h"
#include "hbapiitm.h"
#include "hbstack.h"
#include "hbvmpub.h"
#include "hbpcode.h"
#include "hbinit.h"

#define __PRG_SOURCE__ __FILE__

HB_FUNC( FT_FUSE );
HB_FUNC( FT_FSEEK );
HB_FUNC( FT_FRSEEK );
HB_FUNC( FT_FNEW );
HB_FUNC( FT_FARRAY );
HB_FUNC( FT_FACTIVE );
HB_FUNC( FT_FBUFFERSIZE );
HB_FUNC( FT_FSETNEWLINE );
HB_FUNC( FT_FSELECT );
HB_FUNC( FT_FFLUSH );
HB_FUNC( FT_FWRITEENABLE );
HB_FUNC( FT_FFILENAME );
HB_FUNC( FT_FALIAS );
HB_FUNC( FT_FCHANGED );
HB_FUNC( FT_FINSERT );
HB_FUNC( FT_FDELETE );
HB_FUNC( FT_FRECALL );
HB_FUNC( FT_FAPPEND );
HB_FUNC( FT_FLASTREC );
HB_FUNC( FT_FRECNO );
HB_FUNC( FT_FGOTO );
HB_FUNC( FT_FSKIP );
HB_FUNC( FT_FBOF );
HB_FUNC( FT_FEOF );
HB_FUNC( FT_FGOTOP );
HB_FUNC( FT_FGOBOTTOM );
HB_FUNC( FT_FWRITELN );
HB_FUNC( FT_FREADLN );
HB_FUNC( FT_FREADLN_EX );
HB_FUNC( FT_FDELETED );
HB_FUNC( FT_FCLOSE );
HB_FUNC( FT_FCLOSEALL );

HB_FUNC_EXTERN( ASCAN );
HB_FUNC_EXTERN( HB_RASCAN );

HB_FUNC_INIT( FT_FINIT );
HB_FUNC_EXIT( FT_FEXIT );

#undef HB_PRG_PCODE_VER
#define HB_PRG_PCODE_VER HB_PCODE_VER

HB_INIT_SYMBOLS_BEGIN( hb_vm_SymbolInit_FTEXT )
{ "FT_FUSE",   {HB_FS_PUBLIC}, {HB_FUNCNAME( FT_FUSE )}, NULL },
{ "ASCAN",     {HB_FS_PUBLIC}, {HB_FUNCNAME( ASCAN )}, NULL },
{ "HB_RASCAN", {HB_FS_PUBLIC}, {HB_FUNCNAME( HB_RASCAN )}, NULL },
{ "FT_FINIT$", {HB_FS_INIT},   {HB_INIT_FUNCNAME( FT_FINIT )}, NULL },
{ "FT_FEXIT$", {HB_FS_EXIT},   {HB_EXIT_FUNCNAME( FT_FEXIT )}, NULL }
HB_INIT_SYMBOLS_END( hb_vm_SymbolInit_FTEXT )

#if defined(HB_PRAGMA_STARTUP)
   #pragma startup hb_vm_SymbolInit_FTEXT
#elif defined(HB_MSC_STARTUP)
   #if _MSC_VER >= 1010
      #pragma data_seg( ".CRT$XIY" )
      #pragma comment( linker, "/Merge:.CRT=.data" )
   #else
      #pragma data_seg( "XIY" )
   #endif
   static HB_$INITSYM hb_vm_auto_SymbolInit_FTEXT = hb_vm_SymbolInit_FTEXT;
   #pragma data_seg()
#endif

typedef struct _FT_FFILE
{
   LONG     nCurrent    ;
   FILE     *fHandle    ;
   char     szFileName[_POSIX_PATH_MAX]        ;
   char     szAlias   [HB_SYMBOL_NAME_LEN + 1] ;
   int      iArea       ;
   BOOL     bChange     ;
   BOOL     bActive     ;
   BOOL     bWrite      ;
   PHB_ITEM pOrigin      ;
   PHB_ITEM pArray       ;
   struct _FT_FFILE * pNext;
} FT_FFILE, * PFT_FFILE;

static LONG nCurrent = 0;
static PFT_FFILE pCurFile = NULL;
static PFT_FFILE pFT = NULL;
static int iSelect = 0;
static ULONG uBuffSize = 0;
static char *szNewLine;

static BOOL ft_fread ( FILE *, char * );
static PFT_FFILE ft_fseekAlias ( int );
static PFT_FFILE ft_fseekArea ( char * );
static BOOL ft_fseekActive( void );
#ifdef __LINE_COUNT__
static ULONG ft_flinecount ( FILE * );
#endif
#define DELETION_MARK ""
#define MAX_READ 4096

//------------------------------------------------------------------------------
HB_FUNC( FT_FUSE )
{
   #ifndef FO_WRITE
      #define FO_WRITE      1
   #endif
   #ifndef FO_READWRITE
      #define FO_READWRITE  2
   #endif

   PHB_ITEM pInFile = hb_param( 1, HB_IT_STRING  );

   if( pInFile && hb_itemGetCLen( pInFile ) > 0 && iSelect > 0 )
   {
      PFT_FFILE pTemp;
      BOOL bNewFile = FALSE;
      PHB_ITEM  pArray = hb_itemNew( NULL );
      PFT_FFILE pLast;
      PHB_ITEM  pTmp = hb_itemNew( NULL );
      BOOL bWriteEnable = FALSE;
      char szmode[3];
      int iMode = ISNUM(2) ? hb_parni(2) : 0 ;
      FILE *inFile;

      pTemp = ft_fseekAlias( iSelect );

      // Area already used
      if ( pTemp && pTemp->bActive)
      {
         hb_retl( FALSE );
         return;
      }

      if( iMode & ( FO_WRITE | FO_READWRITE ) )
      {
         bWriteEnable = TRUE;
         szmode[0] = 'r';
         szmode[1] = '+';
         szmode[2] =  0 ;
      }
      else
      {
         szmode[0] = 'r';
         szmode[1] = 0  ;
         szmode[2] = 0  ;
      }

      inFile = fopen( hb_parcx(1), szmode );

      if( inFile )
      {
         PHB_FNAME ft_FileName;
         PHB_ITEM pClone;
         char *string = ( char *) hb_xgrab( uBuffSize + 1 );
#ifdef __LINE_COUNT__
         ULONG ulLineCount = ft_flinecount ( inFile );
         ULONG ulCount = 0;

         fseek( inFile, 0, SEEK_SET );
         hb_arrayNew( pArray, ulLineCount );
#else
         hb_arrayNew( pArray, 0 );
#endif
         while ( ft_fread ( inFile, string ) )
         {
#ifdef __LINE_COUNT__
            ulCount ++;
            hb_arraySetForward( pArray, ulCount, hb_itemPutC( pTmp, string ) );
#else
            hb_arrayAddForward( pArray, hb_itemPutC( pTmp, string ) );
#endif
         }

         hb_itemRelease(pTmp );
         hb_xfree( string );
         fclose( inFile );

         ft_FileName = hb_fsFNameSplit( hb_itemGetCPtr( pInFile ) );

         nCurrent = hb_arrayLen( pArray ) ? 1 : 0;

         if ( pTemp == NULL )
         {
            bNewFile = TRUE;
            pTemp = (PFT_FFILE) hb_xgrab( sizeof( FT_FFILE ) );
            pTemp->pArray = hb_itemNew(NULL);
            pTemp->pOrigin= hb_itemNew(NULL);

         }
         else
         {
            if( pTemp->pArray )
            {
               hb_itemRelease( pTemp->pArray );
               pTemp->pArray = NULL;
               pTemp->pArray = hb_itemNew(NULL);

            }
            if( pTemp->pOrigin )
            {
               hb_itemRelease( pTemp->pOrigin );
               pTemp->pOrigin = NULL;
               pTemp->pOrigin= hb_itemNew(NULL);
            }
         }


         *(pTemp->szFileName) = 0;
         *(pTemp->szAlias) = 0;

         pTemp->bWrite = bWriteEnable;
         strcpy( pTemp->szFileName, hb_parcx(1) );

         if( ISCHAR(3) && hb_parclen( 3 ) > 0 )
         {
            strcpy( pTemp->szAlias, hb_parcx( 3 ) );
         }
         else
         {
            strcpy( pTemp->szAlias, ft_FileName->szName );
         }

         hb_strupr( pTemp->szAlias );

         pTemp->bChange = FALSE;
         pTemp->fHandle = inFile;
         pTemp->iArea = iSelect;

         pClone = hb_arrayClone( pArray );
         hb_itemCopy( pTemp->pArray , pArray );
         hb_itemCopy( pTemp->pOrigin, pClone );

         hb_itemRelease( pClone );

         pTemp->nCurrent = nCurrent ;
         pTemp->bActive = TRUE;

         pCurFile = pTemp;

         if ( bNewFile )
         {
            pTemp->pNext = NULL;

            if( pFT )
            {
               pLast = pFT;
               while( pLast->pNext )
               {
                  pLast = pLast->pNext;
               }
               pLast->pNext = pTemp;
            }
            else
            {
               pFT = pTemp;
            }
         }

         hb_xfree( ft_FileName );
         hb_itemRelease( pArray );
         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      if( iSelect > 0 )
      {
         HB_FUNCNAME( FT_FCLOSE )();
      }

      if( !ft_fseekActive() )
      {
         HB_FUNCNAME( FT_FCLOSEALL )();
      }

      hb_retl( FALSE );
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FSEEK )
{
   PHB_ITEM pSeek = hb_param( 1, HB_IT_STRING );

   if ( pCurFile && pSeek )
   {
      PHB_ITEM pResult = hb_itemDoC( "ASCAN", 2, pCurFile->pArray, pSeek );

      if( pResult )
      {
         nCurrent = hb_itemGetNL( pResult );

         hb_retl( nCurrent > 0 );

         if( !nCurrent )
         {
            nCurrent = hb_arrayLen( pCurFile->pArray ) + 1;
         }

         pCurFile->nCurrent = nCurrent;

         hb_itemRelease( pResult );

         return;
      }
   }

   hb_retl( FALSE );
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FRSEEK )
{
   PHB_ITEM pSeek = hb_param( 1, HB_IT_STRING );

   if ( pCurFile && pSeek )
   {
      PHB_ITEM pResult = hb_itemDoC( "HB_RASCAN", 2, pCurFile->pArray, pSeek );

      if( pResult )
      {
         nCurrent = hb_itemGetNL( pResult );

         hb_retl( nCurrent > 0 );

         if( !nCurrent )
         {
            nCurrent = hb_arrayLen( pCurFile->pArray ) + 1;
         }

         pCurFile->nCurrent = nCurrent;

         hb_itemRelease( pResult );

         return;
      }
   }

   hb_retl( FALSE );
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FNEW )
{
   PHB_ITEM pNew = hb_param( 1, HB_IT_STRING );

   if( pNew && hb_itemGetCLen( pNew ) > 0 )
   {
      FILE *inFile;
      PHB_ITEM pUse;
      PHB_ITEM  fTmp = hb_itemNew( NULL );
      PHB_ITEM  fMode = hb_itemNew( NULL );

      inFile = fopen( hb_parcx(1), "wb" );

      if(!inFile)
      {
         hb_retl( FALSE );
         return;
      }

      fclose( inFile );

      pUse = hb_itemDoC( "FT_FUSE",
         2,
         hb_itemPutC( fTmp, hb_parcx(1) ),
         hb_itemPutNI( fMode, FO_READWRITE ) );

      hb_itemRelease( fTmp );
      hb_itemRelease( fMode );

      if( pUse )
      {
         hb_itemRelease( hb_itemReturn( pUse ) );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_retl( FALSE );
   }
}

//------------------------------------------------------------------------------
HB_FUNC ( FT_FARRAY )
{
   if( pCurFile )
   {
      hb_itemCopy( hb_stackReturnItem(), hb_parl(1) ? pCurFile->pOrigin : pCurFile->pArray );
   }
}

//------------------------------------------------------------------------------
HB_FUNC ( FT_FACTIVE )
{
   hb_retl( pCurFile ? pCurFile->bActive : FALSE );
}

//------------------------------------------------------------------------------
HB_FUNC ( FT_FBUFFERSIZE )
{
   LONG uNewBuff = ISNUM(1) ? hb_parnl(1) : ( LONG ) uBuffSize;

   hb_retnl( uBuffSize );

   if ( uNewBuff > 0 )
   {
      uBuffSize = uNewBuff;
   }
}

//------------------------------------------------------------------------------
HB_FUNC ( FT_FSETNEWLINE )
{
   hb_retc( szNewLine );

   if( ISCHAR(1) )
   {
      szNewLine = hb_parcx( 1 );
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FSELECT )
{
   PHB_ITEM pSelect = hb_param( 1, HB_IT_ANY );

   hb_retni( iSelect );

   if( pSelect )
   {
      if( ISNUM( 1 ) )
      {
         int iNewSelect = hb_parnl( 1 );
         PFT_FFILE pTmp = ft_fseekAlias( iNewSelect );

         if ( iNewSelect == 0 )
         {
            BOOL bFoundActive = FALSE;

            pTmp = pFT;

            while ( pTmp )
            {
               if ( !pTmp->bActive )
               {
                  bFoundActive = TRUE;
                  iSelect = pTmp->iArea;
                  break;
               }
               pTmp = pTmp->pNext;
            }

            if( !bFoundActive )
            {
               iSelect ++;
            }
         }
         else if ( pTmp == NULL )
         {
            iSelect = iNewSelect;
            pCurFile = NULL;
         }
         else
         {
            iSelect = pTmp->iArea;
            nCurrent = pTmp->nCurrent;
            pCurFile = pTmp->bActive ? pTmp : NULL ;
         }
      }
      else if ( ISCHAR(1) )
      {
         PFT_FFILE pTmp = ft_fseekArea( hb_parcx( 1 ) );

         if( pTmp == NULL )
         {
            if( pFT == NULL )
            {
               iSelect = 1;
            }
            else
            {
               BOOL bFoundActive = FALSE;

               pTmp = pFT;

               while ( pTmp )
               {
                  if ( !pTmp->bActive )
                  {
                     bFoundActive = TRUE;
                     iSelect = pTmp->iArea;
                     break;
                  }
                  pTmp = pTmp->pNext;
               }

               if( !bFoundActive )
               {
                  iSelect ++;
               }
            }
         }
         else
         {
            iSelect = pTmp->iArea;
            nCurrent = pTmp->nCurrent;
            pCurFile = pTmp->bActive ? pTmp : NULL ;
         }
      }
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FFLUSH )
{
   if( pCurFile && pCurFile->bWrite && pCurFile->bChange  )
   {
      FILE *inFile = fopen( pCurFile->szFileName, "wb" );

      if( inFile )
      {
         ULONG lEle;
         ULONG ulSize = hb_arrayLen( pCurFile->pArray );

         for ( lEle = 1; lEle <= ulSize ; lEle ++ )
         {
            char *szContent = hb_arrayGetC( pCurFile->pArray, lEle );
            if ( strcmp( szContent, DELETION_MARK ) )
            {
               fprintf( inFile, "%s%s", szContent, szNewLine );
            }
            hb_xfree( szContent );
         }

         fclose( inFile );
         pCurFile->bChange = FALSE;
         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }
   }
   else
   {
      hb_retl( FALSE );
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FWRITEENABLE )
{
   int iAlias = ISNUM(1) ? hb_parni( 1 ) : iSelect ;
   PFT_FFILE pTmp = ft_fseekAlias( iAlias );

   if( pTmp != NULL && ( pCurFile || pTmp->bActive ) )
   {
      hb_retl( pTmp->bWrite);
   }
   else
   {
      hb_retl( FALSE );
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FFILENAME )
{
   int iAlias = ISNUM(1) ? hb_parni( 1 ) : iSelect ;
   PFT_FFILE pTmp = ft_fseekAlias( iAlias );

   if( pTmp != NULL && ( pCurFile || pTmp->bActive ) )
   {
      hb_retc( pTmp->szFileName );
   }
   else
   {
      hb_retc("");
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FALIAS )
{
   int iAlias = ISNUM(1) ? hb_parni( 1 ) : iSelect ;
   PFT_FFILE pTmp = ft_fseekAlias( iAlias );

   if( pTmp != NULL && ( pCurFile || pTmp->bActive ) )
   {
      hb_retc( pTmp->szAlias );
   }
   else
   {
      hb_retc("");
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FCHANGED )
{
   int iAlias = ISNUM(1) ? hb_parni( 1 ) : iSelect ;
   PFT_FFILE pTmp = ft_fseekAlias( iAlias );

   if( pTmp != NULL && ( pCurFile || pTmp->bActive ) )
   {
      hb_retl( pTmp->bChange );
   }
   else
   {
      hb_retl( FALSE );
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FINSERT )
{
   LONG lInsert = ISNUM(1) ? hb_parnl(1) : 0;

   if( pCurFile && pCurFile->bWrite && lInsert > 0 )
   {
      LONG lAdd;

      for ( lAdd = 1; lAdd <= lInsert; lAdd ++ )
      {
         PHB_ITEM  Tmp = hb_itemNew( NULL );
         hb_arraySize( pCurFile->pArray, hb_arrayLen(pCurFile->pArray)+ 1 );
         hb_arrayIns( pCurFile->pArray, nCurrent + lAdd );
         hb_arraySetForward( pCurFile->pArray, nCurrent + lAdd, hb_itemPutC( Tmp, "" ) );
         hb_itemRelease( Tmp);
      }

      pCurFile->bChange = TRUE;
      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FDELETE )
{
   LONG lDelete = ISNUM( 1 ) ? hb_parnl( 1 ) : nCurrent;
   ULONG ulSize = hb_arrayLen( pCurFile->pArray ) ;
   if( pCurFile && pCurFile->bWrite && lDelete > 0 && (ULONG) lDelete <= ulSize)
   {
      PHB_ITEM  Tmp = hb_itemNew( NULL) ;
      hb_arraySetForward( pCurFile->pArray, lDelete, hb_itemPutC( Tmp, DELETION_MARK ) );
      pCurFile->bChange = TRUE;
      hb_itemRelease( Tmp );
      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FRECALL )
{
   LONG lRecall = ISNUM( 1 ) ? hb_parnl( 1 ) : nCurrent;
   ULONG ulSize = hb_arrayLen( pCurFile->pArray ) ;
   if( pCurFile && pCurFile->bWrite && lRecall > 0 && (ULONG) lRecall <=  ulSize )
   {
      char *szReadLn = hb_arrayGetC( pCurFile->pArray, lRecall );

      if( strcmp( szReadLn, DELETION_MARK ) == 0 )
      {
         PHB_ITEM  Tmp = hb_itemNew(NULL);
         char *szOrigin = hb_arrayGetC( pCurFile->pOrigin, lRecall );
         hb_arraySetForward( pCurFile->pArray, lRecall, hb_itemPutC( Tmp, szOrigin ) );
         hb_itemRelease( Tmp );
         if( szOrigin )
         {
            hb_xfree( szOrigin );
         }
         pCurFile->bChange = TRUE;
         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }

      hb_xfree( szReadLn );
   }
   else
   {
      hb_retl( FALSE );
   }
}
//------------------------------------------------------------------------------
HB_FUNC( FT_FAPPEND )
{
   LONG lAppend = ISNUM(1) ? hb_parnl(1) : 1;

   if( lAppend > 0 && pCurFile && pCurFile->bWrite )
   {
      PHB_ITEM  Tmp = hb_itemNew( NULL );
      LONG lStart;
      char *szAppend = ISCHAR(2) ? hb_parcx(2) : (char*) "";

      for( lStart = 1; lStart <= lAppend ; lStart ++ )
      {
         hb_arrayAddForward( pCurFile->pArray, hb_itemPutC(Tmp, szAppend ) );
         hb_arrayAddForward( pCurFile->pOrigin, hb_itemPutC(Tmp, szAppend ) );
      }

      nCurrent = hb_arrayLen( pCurFile->pArray );
      pCurFile->nCurrent = nCurrent;
      pCurFile->bChange = TRUE;
      hb_itemRelease(Tmp);
      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FLASTREC )
{
   ULONG uRet = ULONG_MAX;

   if( pCurFile )
   {
      uRet = hb_parl(1) ? hb_arrayLen( pCurFile->pOrigin ) : hb_arrayLen( pCurFile->pArray );
   }

   hb_retnl( uRet );
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FRECNO )
{
   hb_retnl( nCurrent );
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FGOTO )
{
   if ( ISNUM(1) )
   {
      LONG lGoto = hb_parnl(1);

      if( lGoto > 0 )
      {
         nCurrent = lGoto;

         if( pCurFile )
         {
            pCurFile->nCurrent = nCurrent;
         }
      }
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FSKIP )
{
   LONG uSkip = 1;

   if ( ISNUM(1) )
   {
      uSkip = hb_parnl( 1 );
   }

   nCurrent += uSkip;

   if ( pCurFile )
   {
      pCurFile->nCurrent = nCurrent;
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FBOF )
{
   hb_retl( nCurrent <= 1 );
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FEOF )
{
   hb_retl ( pCurFile ? (ULONG) nCurrent > hb_arrayLen( pCurFile->pArray ) : TRUE );
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FGOTOP )
{
   nCurrent = 1 ;
   if ( pCurFile )
   {
      pCurFile->nCurrent = 1;
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FGOBOTTOM )
{
   if( pCurFile )
   {
      nCurrent = hb_arrayLen( pCurFile->pArray ) ;
      pCurFile->nCurrent = nCurrent;
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FWRITELN )
{
   int iOldArea = iSelect;
   char * pSz = hb_parc(1);
   LONG lWriteLn = ISNUM(2) ? hb_parnl(2) : nCurrent;
   int iAreaWrite = ISNUM(3) ? hb_parni(3) : iSelect;
   BOOL bChangeArea = FALSE;

   if( iAreaWrite != iSelect )
   {
      pCurFile = ft_fseekAlias( iAreaWrite );
      bChangeArea = TRUE;
   }

   if( pCurFile && pCurFile->bWrite && pCurFile->bActive && pSz && lWriteLn > 0 && (ULONG) lWriteLn <= hb_arrayLen(pCurFile->pArray))
   {
      PHB_ITEM  Tmp = hb_itemNew(NULL);
      hb_arraySetForward( pCurFile->pArray,  lWriteLn, hb_itemPutC( Tmp, pSz ) );
      hb_arraySetForward( pCurFile->pOrigin, lWriteLn, hb_itemPutC( Tmp, pSz ) );
      hb_itemRelease(Tmp);
      pCurFile->bChange = TRUE;
      hb_retl( TRUE );
   }
   else
   {
      hb_retl( FALSE );
   }

   if( bChangeArea )
   {
      iSelect = iOldArea;
      pCurFile = ft_fseekAlias( iSelect );
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FREADLN )
{
   LONG lReadLn = ISNUM(1) ? hb_parnl(1) : nCurrent;
   ULONG ulSize =hb_arrayLen( pCurFile->pArray );
   if( pCurFile && lReadLn > 0 && (ULONG) lReadLn <= ulSize)
   {
      char *szReadLn = hb_parl( 2 ) ? hb_arrayGetC( pCurFile->pOrigin, lReadLn ): hb_arrayGetC( pCurFile->pArray, lReadLn );
      hb_retcAdopt( szReadLn );
   }
   else
   {
      hb_retc("");
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FREADLN_EX )
{
   ULONG ulSize =hb_arrayLen( pCurFile->pArray );
   if( pCurFile && nCurrent > 0 && (ULONG) nCurrent <= ulSize)
   {
      char *szReadLn = hb_arrayGetC( pCurFile->pArray, nCurrent );
      hb_retcAdopt( szReadLn );
      nCurrent ++;
      pCurFile->nCurrent = nCurrent;
   }
   else
   {
      hb_retc("");
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FDELETED )
{
   LONG lQuery = ISNUM(1) ? hb_parnl(1) : nCurrent;
   ULONG ulSize =hb_arrayLen( pCurFile->pArray ) ;
   if( pCurFile && lQuery > 0 && (ULONG) lQuery <= ulSize )
   {
      char *szReadLn = hb_arrayGetC( pCurFile->pArray, lQuery );
      hb_retl( strcmp( szReadLn, DELETION_MARK ) == 0 );
      hb_xfree( szReadLn );
   }
   else
   {
      hb_retl( FALSE );
   }
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FCLOSE )
{
   PFT_FFILE pTmp = NULL;
   int iOldSelect = iSelect;

   if ( ISCHAR( 1 ) )
   {
      pTmp = ft_fseekArea( hb_parcx( 1 ) );
   }
   else if ( ISNUM( 1 ) )
   {
      int iSeek = hb_parni( 1 );
      pTmp = ft_fseekAlias( iSeek );
   }
   else if ( ISNIL( 1 ) )
   {
      pTmp = ft_fseekAlias( iSelect );
   }

   if ( pTmp != NULL )
   {
      if ( pTmp->bActive )
      {
         iSelect = pTmp->iArea;
         pCurFile = pTmp;
         HB_FUNCNAME( FT_FFLUSH )();
         pTmp->bActive = FALSE;
         *(pTmp->szFileName) = 0;
         *(pTmp->szAlias) = 0;
         hb_retl( TRUE );
      }
      else
      {
         hb_retl( FALSE );
      }

      if( !ft_fseekActive() )
      {
         HB_FUNCNAME( FT_FCLOSEALL )();
      }
   }
   else
   {
      hb_retl( FALSE );
   }

   if( iSelect != iOldSelect )
   {
      iSelect = iOldSelect;
   }

   pCurFile = NULL;
}

//------------------------------------------------------------------------------
HB_FUNC_INIT ( FT_FINIT )
{
   if( uBuffSize == 0 )
   {
      uBuffSize = MAX_READ;
   }

   szNewLine = hb_conNewLine();
}

//------------------------------------------------------------------------------
HB_FUNC( FT_FCLOSEALL )
{
   PFT_FFILE pTmp = pFT;

   while( pTmp )
   {
      iSelect = pTmp->iArea;
      pCurFile = pTmp;
      HB_FUNCNAME( FT_FFLUSH )();
      hb_itemRelease( pTmp->pArray );
      hb_itemRelease( pTmp->pOrigin );
       pTmp->pArray =NULL;
       pTmp->pOrigin =NULL;

      pTmp = pTmp->pNext;
      hb_xfree( pFT );
      pFT = pTmp;
   }

   pCurFile = NULL;
}

//------------------------------------------------------------------------------
HB_FUNC_EXIT( FT_FEXIT )
{
  HB_FUNCNAME( FT_FCLOSEALL )();
}

//------------------------------------------------------------------------------
static PFT_FFILE ft_fseekArea( char *szSeek )
{
   PFT_FFILE pTmp;

   if ( pFT && szSeek && *szSeek )
   {
      char *szSelect = (char *) hb_xgrab( hb_parclen( 1 ) + 1 );
      BOOL bFound = FALSE;

      strcpy( szSelect , szSeek );

      hb_strupr( szSelect );

      pTmp = pFT;

      while( pTmp )
      {
         if( strcmp( pTmp->szAlias, szSelect ) == 0 )
         {
            bFound = TRUE;
            break;
         }
         pTmp = pTmp->pNext;
      }

      hb_xfree( szSelect );

      return ( bFound ? pTmp : NULL );
   }
   else
   {
      return ( NULL );
   }

}

//------------------------------------------------------------------------------
static PFT_FFILE ft_fseekAlias( int iSeek )
{
   PFT_FFILE pTmp;

   if ( pFT )
   {
      BOOL bFound = FALSE;

      pTmp = pFT;

      while( pTmp )
      {
         if( pTmp->iArea == iSeek )
         {
            bFound = TRUE;
            break;
         }
         pTmp = pTmp->pNext;
      }

      return ( bFound ? pTmp : NULL );
   }
   else
   {
      return (NULL);
   }
}

//------------------------------------------------------------------------------
static BOOL ft_fseekActive()
{
   BOOL bFound = FALSE;

   if ( pFT )
   {
      PFT_FFILE pTmp = pFT;

      while( pTmp )
      {
         if( pTmp->bActive )
         {
            bFound = TRUE;
            break;
         }
         pTmp = pTmp->pNext;
      }
   }

   return ( bFound );
}

//----------------------------------------------------------------------------//
static BOOL ft_fread ( FILE *stream, char *string )
{
   int ch, cnbr = 0;

   for (;;)
   {
      ch = fgetc ( stream );

      if ( ( ch == '\n' ) || ( ch == EOF ) || ( ch == 26 ) )
      {
         string [cnbr] = '\0';
         return ( ch == '\n' || cnbr );
      }
      else
      {
         if ( (ULONG) cnbr < uBuffSize && ch != '\r' )
         {
            string [cnbr++] = (char) ch;
         }
      }

      if ( (ULONG) cnbr >= uBuffSize )
      {
         string [uBuffSize] = '\0';
         return (TRUE);
      }
   }
}

#ifdef __LINE_COUNT__
//----------------------------------------------------------------------------//
static ULONG ft_flinecount( FILE *inFile )
{
   ULONG ulLineCount = 0;
   int ch;

   while ( ( ch = fgetc ( inFile ) ) != EOF )
   {
      if ( ch == '\n' )
      {
         ulLineCount ++;
      }
   }

   return( ulLineCount );
}
#endif
