/*
 * $Id$
 */

/*
 * runlib.c contains the .HRB object runner system
 *
 * Copyright(C) 1999 by Eddie Runia <eddie@runia.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR
 * PURPOSE.  See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to:
 *
 * The Free Software Foundation, Inc.,
 * 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include "extend.h"
#include "itemapi.h"
#include "errorapi.h"
#include "ctoharb.h"
#include "pcode.h"

/* TODO: Fill the error codes with valid ones (instead of 9999) */
/* TOFIX: Change this assembler hack to something standard and portable */
/* TODO: Change the fopen()/fread()/fclose() calls to hb_fs*() */

/* NOTE: This is the assembler output from : hb_vmExecute( pcode, symbols ).  */

/* #if INTEL32 */

static BYTE prgFunction[] =
{
   0x68, 0x00, 0x00, 0x00, 0x00,  /* push offset pcode               */
   0x68, 0x00, 0x00, 0x00, 0x00,  /* push offset symbols             */
   0xE8, 0x00, 0x00, 0x00, 0x00,  /* call near relative hb_vmExecute */
   0x83, 0xC4, 0x08,              /* add esp, 8                      */
   0xC3                           /* ret near                        */
};

/* #elseif INTEL16 */
/* #elseif MOTOROLA */
/* #elseif ... */
/* #endif */


typedef union
{
   BYTE *   pAsmData;                           /* The assembler bytes      */
   PHB_FUNC pFunPtr;                            /* The (dynamic) harbour
                                                   function                 */
} ASM_CALL, * PASM_CALL;

typedef struct
{
   char *     szName;                           /* Name of the function     */
   PASM_CALL  pAsmCall;                         /* Assembler call           */
   BYTE *     pCode;                            /* P-code                   */
} HB_DYNF, * PHB_DYNF;


#define SYM_NOLINK  0                           /* Symbol does not have to
                                                                  be linked */
#define SYM_FUNC    1                           /* Defined function         */
#define SYM_EXTERN  2                           /* Prev. defined function   */

#define SYM_NOT_FOUND 0xFFFFFFFF                /* Symbol not found.
                                                   FindSymbol               */

static ULONG     hb_hrbFindSymbol( char * szName, PHB_DYNF pDynFunc, ULONG ulLoaded );
static PASM_CALL hb_hrbAsmCreateFun( PHB_SYMB pSymbols, BYTE * pCode ); /* Create a dynamic function*/
static void      hb_hrbAsmPatch( BYTE * pCode, ULONG ulOffset, void * Address );
static void      hb_hrbAsmPatchRelative( BYTE * pCode, ULONG ulOffset, void * Address, ULONG ulNext );

static FILE *    hb_hrbFileOpen( char * szFileName );
static void      hb_hrbFileRead( FILE * file, char * szFileName, char * cBuffer, int iSize, int iCount );
static BYTE      hb_hrbFileReadByte( FILE * file, char * szFileName );
static char *    hb_hrbFileReadId( FILE * file, char * szFileName );
static long      hb_hrbFileReadLong( FILE * file, char * szFileName );
static void      hb_hrbFileClose( FILE * file );

static ULONG     s_ulSymEntry = 0;              /* Link enhancement         */

/*
   __HRBRUN( <cFile> [, xParam1 [, xParamN ] ] ) -> return value.

   This program will get the data from the .HRB file and run the p-code
   contained in it.

   In due time it should also be able to collect the data from the
   binary/executable itself
*/

HARBOUR HB___HRBRUN( void )
{
   if( hb_pcount() >= 1 )
   {
      char * szFileName = hb_parc( 1 );
      FILE * file;

      /* Open as binary */

      while ( ( file = hb_hrbFileOpen( szFileName ) ) == NULL )
      {
         WORD wResult = hb_errRT_BASE_Ext1( EG_OPEN, 9999, NULL, szFileName, 0, EF_CANDEFAULT | EF_CANRETRY );

         if( wResult == E_DEFAULT && wResult == E_BREAK )
            break;
      }

      if( file )
      {
         ULONG ulSymbols;                             /* Number of symbols        */
         ULONG ulFuncs;                               /* Number of functions      */
         ULONG ulSize;                                /* Size of function         */
         ULONG ul, ulPos;

         PHB_SYMB pSymRead;                           /* Symbols read             */
         PHB_DYNF pDynFunc;                           /* Functions read           */
         PHB_DYNS pDynSym;

         PHB_ITEM pRetVal;

         int i;

         ulSymbols = hb_hrbFileReadLong( file, szFileName );
         pSymRead = ( PHB_SYMB ) hb_xgrab( ulSymbols * sizeof( HB_SYMB ) );

         for( ul = 0; ul < ulSymbols; ul++ )       /* Read symbols in .HRB     */
         {
            pSymRead[ ul ].szName  = hb_hrbFileReadId( file, szFileName );
            pSymRead[ ul ].cScope  = hb_hrbFileReadByte( file, szFileName );
            pSymRead[ ul ].pFunPtr = ( PHB_FUNC ) ( ULONG ) hb_hrbFileReadByte( file, szFileName );
            pSymRead[ ul ].pDynSym = NULL;
         }

         ulFuncs = hb_hrbFileReadLong( file, szFileName );            /* Read number of functions */
         pDynFunc = ( PHB_DYNF ) hb_xgrab( ulFuncs * sizeof( HB_DYNF ) );
         for( ul = 0; ul < ulFuncs; ul++ )         /* Read symbols in .HRB     */
         {
            pDynFunc[ ul ].szName = hb_hrbFileReadId( file, szFileName );

            ulSize = hb_hrbFileReadLong( file, szFileName );      /* Read size of function    */
            pDynFunc[ ul ].pCode = ( BYTE * ) hb_xgrab( ulSize );
            hb_hrbFileRead( file, szFileName, pDynFunc[ ul ].pCode, 1, ulSize );
                                                /* Read the block           */

            pDynFunc[ ul ].pAsmCall = hb_hrbAsmCreateFun( pSymRead,
                                                 pDynFunc[ ul ].pCode );
                                                /* Create matching dynamic  */
                                                /* function                 */
         }

         s_ulSymEntry = 0;
         for( ul = 0; ul < ulSymbols; ul++ )    /* Linker                   */
         {
            if( ( ( ULONG ) pSymRead[ ul ].pFunPtr ) == SYM_FUNC )
            {
               ulPos = hb_hrbFindSymbol( pSymRead[ ul ].szName, pDynFunc, ulFuncs );
               if( ulPos != SYM_NOT_FOUND )
               {
                  /* Exists and NOT static ?  */
/*                if(    hb_dynsymFind( pSymRead[ ul ].szName ) &&
                      !( pSymRead[ ul ].cScope & FS_STATIC ) )
                  {
                     hb_errRT_BASE( EG_ARG, 9999, "Duplicate symbol", pSymRead[ ul ].szName );
                     return;
                  }
*/
                  pSymRead[ ul ].pFunPtr = pDynFunc[ ulPos ].pAsmCall->pFunPtr;
               }
               else
                  pSymRead[ ul ].pFunPtr = ( PHB_FUNC ) SYM_EXTERN;
            }
            if( ( ( ULONG ) pSymRead[ ul ].pFunPtr ) == SYM_EXTERN )
            {                                   /* External function        */
               pDynSym = hb_dynsymFind( pSymRead[ ul ].szName );
               if( !pDynSym )
               {
                  hb_errRT_BASE( EG_ARG, 9999, "Unknown or unregistered symbol", pSymRead[ ul ].szName );
                  return;
               }
               pSymRead[ ul ].pFunPtr = pDynSym->pFunPtr;
            }
         }

         hb_vmProcessSymbols( pSymRead, ulSymbols );

         /* Initialize static variables first
          */
         for( ul = 0; ul < ulSymbols; ul++ )    /* Check INIT functions     */
         {
            if( ( pSymRead[ ul ].cScope & FS_INITEXIT ) == FS_INITEXIT )
            {
               /* call (_INITSTATICS) function. This function assigns
                * literal values to static variables only. There is no need
                * to pass any parameters to this function because they
                * cannot be used to initialize static variable.
                */
               pSymRead[ ul ].pFunPtr();
            }
         }
         for( ul = 0; ul < ulSymbols; ul++ )    /* Check INIT functions     */
         {
            if( ( pSymRead[ ul ].cScope & FS_INITEXIT ) == FS_INIT )
            {
               hb_vmPushSymbol( pSymRead + ul );
               hb_vmPushNil();
               for( i = 0; i < ( hb_pcount() - 1 ); i++ )
                  hb_vmPush( hb_param( i + 2, IT_ANY ) );
                                               /* Push other cmdline params*/
               hb_vmDo( hb_pcount() - 1 );            /* Run init function        */
            }
         }

         hb_vmPushSymbol( pSymRead );
         hb_vmPushNil();
         for( i = 0; i < ( hb_pcount() - 1 ); i++ )
            hb_vmPush( hb_param( i + 2, IT_ANY ) );    /* Push other cmdline params*/
         hb_vmDo( hb_pcount() - 1 );                   /* Run the thing !!!        */

         pRetVal = hb_itemNew( NULL );
         hb_itemCopy( pRetVal, &stack.Return );

         for( ul = 0; ul < ulSymbols; ul++ )    /* Check EXIT functions     */
         {
            if( ( pSymRead[ ul ].cScope & FS_INITEXIT ) == FS_EXIT )
            {
               hb_vmPushSymbol( pSymRead + ul );
               hb_vmPushNil();
               hb_vmDo( 0 );                        /* Run exit function        */
               pSymRead[ ul ].cScope = pSymRead[ ul ].cScope & ( ~FS_EXIT );
                                               /* Exit function cannot be
                                                  handled by main in hvm.c */
            }
         }

         for( ul = 0; ul < ulFuncs; ul++ )
         {
            hb_xfree( pDynFunc[ ul ].pAsmCall->pAsmData );
            hb_xfree( pDynFunc[ ul ].pAsmCall           );
            hb_xfree( pDynFunc[ ul ].pCode              );
            hb_xfree( pDynFunc[ ul ].szName             );
         }

         for( ul = 0; ul < ulSymbols; ul++ )
            hb_xfree( pSymRead[ ul ].szName );

         hb_xfree( pDynFunc );
         hb_xfree( pSymRead );
         hb_hrbFileClose( file );

         hb_itemReturn( pRetVal );
         hb_itemRelease( pRetVal );
      }
   }
   else
      hb_errRT_BASE( EG_ARG, 9999, NULL, "__HRBRUN" );
}


static ULONG hb_hrbFindSymbol( char * szName, PHB_DYNF pDynFunc, ULONG ulLoaded )
{
   ULONG ulRet;

   if( ( s_ulSymEntry < ulLoaded ) &&             /* Is it a normal list ?    */
       !strcmp( szName, pDynFunc[ s_ulSymEntry ].szName ) )
      ulRet = s_ulSymEntry++;
   else
   {
      BOOL bFound = FALSE;

      ulRet = 0;
      while( !bFound && ulRet < ulLoaded )
      {
         if( !strcmp( szName, pDynFunc[ ulRet ].szName ) )
            bFound = TRUE;
         else
            ulRet++;
      }
      if( !bFound )
         ulRet = SYM_NOT_FOUND;
   }
   return ulRet;
}


/* ReadId
   Read the next (zero terminated) identifier */
static char * hb_hrbFileReadId( FILE * file, char * szFileName )
{
   char * szTemp;                                /* Temporary buffer         */
   char * szIdx;
   char * szRet;

   BOOL  bCont = TRUE;

   szTemp = ( char * ) hb_xgrab( 256 );
   szIdx  = szTemp;
   do
   {
      hb_hrbFileRead( file, szFileName, szIdx, 1, 1 );
      if( *szIdx )
         szIdx++;
      else
         bCont = FALSE;
   } while( bCont );

   szRet = ( char * ) hb_xgrab( szIdx - szTemp + 1 );
   strcpy( szRet, szTemp );
   hb_xfree( szTemp );

   return szRet;
}


static BYTE hb_hrbFileReadByte( FILE * file, char * szFileName )
{
   BYTE bRet;

   hb_hrbFileRead( file, szFileName, &bRet, 1, 1 );

   return bRet;
}


static long hb_hrbFileReadLong( FILE * file, char * szFileName )
{
   char cLong[ 4 ];                               /* Temporary long           */

   hb_hrbFileRead( file, szFileName, cLong, 4, 1 );

   if( cLong[ 3 ] )                             /* Convert to long if ok    */
   {
      hb_errRT_BASE_Ext1( EG_READ, 9999, NULL, szFileName, 0, EF_NONE );
      return 0;
   }
   else
      return ( ( BYTE ) cLong[ 0 ] )             +
             ( ( BYTE ) cLong[ 1 ] ) * 0x100     +
             ( ( BYTE ) cLong[ 2 ] ) * 0x10000   +
             ( ( BYTE ) cLong[ 3 ] ) * 0x1000000 ;
}


/*  hb_hrbFileRead
    Controlled read from file. If errornous -> Break */
static void hb_hrbFileRead( FILE * file, char * szFileName, char * cBuffer, int iSize, int iCount )
{
   if( iCount != ( int ) fread( cBuffer, iSize, iCount, file ) )
      hb_errRT_BASE_Ext1( EG_READ, 9999, NULL, szFileName, 0, EF_NONE );
}


/*  hb_hrbFileOpen
    Open an .HRB file  */
static FILE * hb_hrbFileOpen( char * szFileName )
{
   return fopen( szFileName, "rb" );
}


/*  hb_hrbFileClose
    Close an .HRB file  */
static void hb_hrbFileClose( FILE * file )
{
   fclose( file );
}


/*
   Create dynamic function.

   This function is needed, since it will allow the existing strategy of
   function pointers to work properly.

   For each Harbour function a little program calling the virtual machine
   should be present (see : *.c)

   Since these programs no longer exists when using this system, they should
   be create dynamically at run-time.

   If a .PRG contains 10 functions, 10 dynamic functions are created which
   are all the same :-) except for 2 pointers.
*/
static PASM_CALL hb_hrbAsmCreateFun( PHB_SYMB pSymbols, BYTE * pCode )
{
   PASM_CALL asmRet = ( PASM_CALL ) hb_xgrab( sizeof( ASM_CALL ) );

   asmRet->pAsmData = ( BYTE * ) hb_xgrab( sizeof( prgFunction ) );
   memcpy( asmRet->pAsmData, prgFunction, sizeof( prgFunction ) );
                                              /* Copy new assembler code in */
/* #if INTEL32 */

   hb_hrbAsmPatch( asmRet->pAsmData, 1, pSymbols );   /* Insert pointer to testsym */
   hb_hrbAsmPatch( asmRet->pAsmData, 6, pCode );      /* Insert pointer to testcode */
   hb_hrbAsmPatchRelative( asmRet->pAsmData, 11, &hb_vmExecute, 15 );
                                      /* Insert pointer to hb_vmExecute() */

/* #elseif INTEL16 */
/* #elseif MOTOROLA */
/* #elseif ... */
/* #endif */
   return asmRet;
}

/* Patch an address of the dynamic function */
static void hb_hrbAsmPatch( BYTE * pCode, ULONG ulOffset, void * Address )
{
/* #if 32 bits and low byte first */

   pCode[ ulOffset     ] = ( ( ULONG ) Address       ) & 0xFF;
   pCode[ ulOffset + 1 ] = ( ( ULONG ) Address >>  8 ) & 0xFF;
   pCode[ ulOffset + 2 ] = ( ( ULONG ) Address >> 16 ) & 0xFF;
   pCode[ ulOffset + 3 ] = ( ( ULONG ) Address >> 24 ) & 0xFF;

/* #elseif 16 bits and low byte first */
/* #elseif 32 bits and high byte first */
/* #elseif ... */
/* #endif */
}


/* Intel specific ?? Patch an address relative to the next instruction */
static void hb_hrbAsmPatchRelative( BYTE * pCode, ULONG ulOffset,
                                    void * Address, ULONG ulNext )
{
/* #if 32 bits and low byte first */
   ULONG ulBase = ( ULONG ) pCode + ulNext;
                                /* Relative to next instruction */
   ULONG ulRelative = ( ULONG ) Address - ulBase;

   pCode[ ulOffset     ] = ( ulRelative       ) & 0xFF;
   pCode[ ulOffset + 1 ] = ( ulRelative >>  8 ) & 0xFF;
   pCode[ ulOffset + 2 ] = ( ulRelative >> 16 ) & 0xFF;
   pCode[ ulOffset + 3 ] = ( ulRelative >> 24 ) & 0xFF;

/* #elseif 16 bits and low byte first */
/* #elseif 32 bits and high byte first */
/* #elseif ... */
/* #endif */
}
