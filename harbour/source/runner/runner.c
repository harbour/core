/*
 * $Id$
 */
/*
 * Runner.c contains the .HRB object system
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

#include <stdio.h>

#include "pcode.h"
#include "errorapi.h"
#include "init.h"

/* #if INTEL32 */
static BYTE prgFunction[] = { 0x68, 0x00, 0x00, 0x00, 0x00,
                              0x68, 0x00, 0x00, 0x00, 0x00,
                              0xE8, 0x00, 0x00, 0x00, 0x00,
                              0x83, 0xC4, 0x08,
                              0xC3 };
     /* push offset pcode
        push offset symbols
        call near relative VirtualMachine
        add esp, 8
        ret near                   */

     /* This is the assembler output from : VirtualMachine(pcode,symbols).  */

/* #elseif INTEL16 */
/* #elseif MOTOROLA */
/* #elseif ... */
/* #endif */


typedef union
{
   PBYTE       pAsmData;                        /* The assembler bytes      */
   HARBOURFUNC pFunPtr;                         /* The (dynamic) harbour
                                                   function                 */
} ASM_CALL, *PASM_CALL;

typedef struct
{
   char      *szName;                           /* Name of the function     */
   PASM_CALL  pAsmCall;                         /* Assembler call           */
   PBYTE      pCode;                            /* P-code                   */
} DYNFUNC, *PDYNFUNC;


#define SYM_NOLINK  0                           /* Symbol does not have to
                                                                  be linked */
#define SYM_FUNC    1                           /* Defined function         */
#define SYM_EXTERN  2                           /* Prev. defined function   */

#define SYM_NOT_FOUND 0xFFFFFFFF                /* Symbol not found.
                                                   FindSymbol               */

static PASM_CALL CreateFun( PSYMBOL, PBYTE );   /* Create a dynamic function*/
       void      Do( WORD );
static ULONG     FindSymbol( char *, PDYNFUNC, ULONG );
       HARBOUR   HB_HB_RUN();
static void      HRB_FileClose( FILE * );
static void      HRB_FileRead ( char *, int, int, FILE * );
static FILE     *HRB_FileOpen ( char * );
       void      Push( PHB_ITEM );
       void      PushNil( void );
       void      PushSymbol( PSYMBOL );
       BYTE      ReadByte( FILE * );
       char     *ReadId  ( FILE * );
       long      ReadLong( FILE * );

/*
 * This file contains the extra exportable functions available to the Harbour
 * program
 */
#include "run_exp.h"

extern void Arrays__InitSymbols( void );
extern void Classes__InitSymbols( void );
extern void Console__InitSymbols( void );
extern void CopyFile__InitSymbols( void );
extern void Dates__InitSymbols( void );
extern void Dates2__InitSymbols( void );
extern void Descend__InitSymbols( void );
extern void Dir__InitSymbols( void );
extern void Environ__InitSymbols( void );
extern void Files__InitSymbols( void );
extern void HardCR__InitSymbols( void );
extern void Math__InitSymbols( void );
extern void Memotran__InitSymbols( void );
extern void Set__InitSymbols( void );
extern void Strings__InitSymbols( void );
extern void Transfrm__InitSymbols( void );

static void InitRunnerTable( void )
{
#include "initsymb.h"                           /* Include default symbols  */
}
#if !defined( __GNUC__ )
   #pragma startup InitRunnerTable
#endif


ULONG ulSymEntry = 0;                           /* Link enhancement         */

/*
   HB_Run( <cFile> )

   This program will get the data from the .HRB file and run the p-code
   contained in it.

   In due time it should also be able to collect the data from the
   binary/executable itself
*/
HARBOUR HB_HB_RUN( void )
{
   char *szFileName;

   FILE *file;

   ULONG ulSymbols;                             /* Number of symbols        */
   ULONG ulFuncs;                               /* Number of functions      */
   ULONG ulSize;                                /* Size of function         */
   ULONG ul, ulPos;

   int i;

   PSYMBOL  pSymRead;                           /* Symbols read             */
   PDYNFUNC pDynFunc;                           /* Functions read           */
   PDYNSYM  pDynSym;

   if( hb_pcount() == 0 )
      printf( "\nPlease give HRB file name\n" );
   else
   {
      szFileName = hb_parc( 1 );
      file = HRB_FileOpen( szFileName );        /* Open as binary           */
      if( file )
      {
         ulSymbols = ReadLong( file );
         pSymRead = ( PSYMBOL )hb_xgrab( ulSymbols * sizeof( SYMBOL ) );

         for( ul=0; ul < ulSymbols; ul++)       /* Read symbols in .HRB     */
         {
            pSymRead[ ul ].szName  = ReadId( file );
            pSymRead[ ul ].cScope  = ReadByte( file );
            pSymRead[ ul ].pFunPtr = ( HARBOURFUNC ) ReadByte( file );
            pSymRead[ ul ].pDynSym = NULL;
         }

         ulFuncs = ReadLong( file );            /* Read number of functions */
         pDynFunc = ( PDYNFUNC ) hb_xgrab( ulFuncs * sizeof( DYNFUNC ) );
         for( ul=0; ul < ulFuncs; ul++)         /* Read symbols in .HRB     */
         {
            pDynFunc[ ul ].szName = ReadId( file );

            ulSize = ReadLong( file ) + 1;      /* Read size of function    */
            pDynFunc[ ul ].pCode = ( PBYTE )hb_xgrab( ulSize );
            HRB_FileRead( pDynFunc[ ul ].pCode, 1, ulSize, file );
                                                /* Read the block           */

            pDynFunc[ ul ].pAsmCall = CreateFun( pSymRead,
                                                 pDynFunc[ ul ].pCode );
                                                /* Create matching dynamic  */
                                                /* function                 */
         }

         ulSymEntry = 0;
         for( ul = 0; ul < ulSymbols; ul++ )    /* Linker                   */
         {
            if( ( (ULONG) pSymRead[ ul ].pFunPtr ) == SYM_FUNC )
            {
               ulPos = FindSymbol( pSymRead[ ul ].szName, pDynFunc, ulFuncs );
               if( ulPos != SYM_NOT_FOUND )
               {
/*                  if(    hb_FindDynSym( pSymRead[ ul ].szName ) &&
                      !( pSymRead[ ul ].cScope & FS_STATIC ) )
                  {                           */  /* Exists and NOT static ?  */
/*                     printf( "\nDuplicate identifier '%s' %i.",
                             pSymRead[ ul ].szName, pSymRead[ul].cScope );
                     exit( 1 );
                  } */
                  pSymRead[ ul ].pFunPtr = pDynFunc[ ulPos ].pAsmCall->pFunPtr;
               }
               else
                  pSymRead[ ul ].pFunPtr = ( HARBOURFUNC ) SYM_EXTERN;
            }
            if( ( (ULONG) pSymRead[ ul ].pFunPtr ) == SYM_EXTERN )
            {                                   /* External function        */
               pDynSym = hb_FindDynSym( pSymRead[ ul ].szName );
               if( !pDynSym )
               {
                  printf( "\nUnknown or unregistered function '%s'.",
                          pSymRead[ ul ].szName );
                  exit( 1 );
               }
               pSymRead[ ul ].pFunPtr = pDynSym->pFunPtr;
            }
         }

         ProcessSymbols( pSymRead, ulSymbols );

         /* Initialize static variables first
          */
         for( ul = 0; ul < ulSymbols; ul++ )    /* Check INIT functions     */
         {
            if( (pSymRead[ ul ].cScope & (FS_INIT|FS_EXIT)) == (FS_INIT|FS_EXIT) )
            {
               if( pSymRead[ ul ].pFunPtr )
                     pSymRead[ ul ].pFunPtr();
            }
         }
         for( ul = 0; ul < ulSymbols; ul++ )    /* Check INIT functions     */
         {
            if( (pSymRead[ ul ].cScope & (FS_INIT|FS_EXIT)) == FS_INIT )
            {
                PushSymbol( pSymRead + ul );
                PushNil();
                for( i = 0; i < (hb_pcount() - 1); i++ )
                   Push( hb_param( i + 2, IT_ANY ) );
                                                /* Push other cmdline params*/
                Do( hb_pcount() - 1 );            /* Run init function        */
            }
         }

         PushSymbol( pSymRead );
         PushNil();
         for( i = 0; i < (hb_pcount() - 1); i++ )
            Push( hb_param( i + 2, IT_ANY ) );    /* Push other cmdline params*/
         Do( hb_pcount() - 1 );                   /* Run the thing !!!        */

         for( ul = 0; ul < ulSymbols; ul++ )    /* Check EXIT functions     */
         {
            if( ( pSymRead[ ul ].cScope & FS_INITEXIT ) == FS_EXIT )
            {
                PushSymbol( pSymRead + ul );
                PushNil();
                Do( 0 );                        /* Run exit function        */
                pSymRead[ ul ].cScope = pSymRead[ ul ].cScope & (~FS_EXIT);
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
         {
            hb_xfree( pSymRead[ ul ].szName );
         }

         hb_xfree( pDynFunc );
         hb_xfree( pSymRead );
         HRB_FileClose( file );
      }
      else
      {
         printf( "\nCannot open %s\n", szFileName );
      }
   }
}


static ULONG FindSymbol( char *szName, PDYNFUNC pDynFunc, ULONG ulLoaded )
{
   ULONG ulRet;
   BYTE  bFound;

   if( ( ulSymEntry < ulLoaded ) &&             /* Is it a normal list ?    */
       !strcmp( szName, pDynFunc[ ulSymEntry ].szName ) )
      ulRet = ulSymEntry++;
   else
   {
      bFound = FALSE;
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
   return( ulRet );
}


/* ReadId
   Read the next (zero terminated) identifier */
char *ReadId( FILE *file )
{
   char *szTemp;                                /* Temporary buffer         */
   char *szIdx;
   char *szRet;

   BYTE  bCont = TRUE;

   szTemp = ( char * )hb_xgrab( 256 );
   szIdx  = szTemp;
   do
   {
      HRB_FileRead( szIdx, 1, 1, file );
      if( *szIdx )
         szIdx++;
      else
         bCont = FALSE;
   } while( bCont );

   szRet = (char *) hb_xgrab( szIdx - szTemp + 1 );
   strcpy( szRet, szTemp );
   hb_xfree( szTemp );

   return( szRet );
}


BYTE ReadByte( FILE *file )
{
   BYTE bRet;

   HRB_FileRead( &bRet, 1, 1, file );
   return( bRet );
}


long ReadLong( FILE *file )
{
   char cLong[4];                               /* Temporary long           */

   HRB_FileRead( cLong, 4, 1, file );

   if( cLong[3] )                               /* Convert to long if ok    */
   {
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Error reading .HRB file");
      hb_errLaunch(pError);
      hb_errRelease(pError);
      return( NULL );
   }
   else
      return( ( (BYTE) cLong[0] )             +
              ( (BYTE) cLong[1] ) * 0x100     +
              ( (BYTE) cLong[2] ) * 0x10000   +
              ( (BYTE) cLong[3] ) * 0x1000000 );
}


/*  HRB_FileRead
    Controlled read from file. If errornous -> Break */
static void HRB_FileRead( char *cBuffer, int iSize, int iCount, FILE *fStream )
{
   if( iCount != (int) fread( cBuffer, iSize, iCount, fStream ) )
   {                                            /* Read error               */
      PHB_ITEM pError = hb_errNew();
      hb_errPutDescription(pError, "Error reading .HRB file");
      hb_errLaunch(pError);
      hb_errRelease(pError);
      exit(1);
   }
}


/*  HRB_FileOpen
    Open an .HRB file  */
static FILE *HRB_FileOpen( char *szFileName )
{
   return( fopen( szFileName, "rb" ) );
}


/*  HRB_FileClose
    Close an .HRB file  */
static void HRB_FileClose( FILE *file )
{
   fclose( file );
}


/* Patch an address of the dynamic function */
static void Patch( PBYTE pCode, ULONG ulOffset, void *Address )
{
/* #if 32 bits and low byte first */

   pCode[ ulOffset     ] = ( (ULONG) Address       ) & 0xFF;
   pCode[ ulOffset + 1 ] = ( (ULONG) Address >>  8 ) & 0xFF;
   pCode[ ulOffset + 2 ] = ( (ULONG) Address >> 16 ) & 0xFF;
   pCode[ ulOffset + 3 ] = ( (ULONG) Address >> 24 ) & 0xFF;

/* #elseif 16 bits and low byte first */
/* #elseif 32 bits and high byte first */
/* #elseif ... */
/* #endif */
}


/* Intel specific ?? Patch an address relative to the next instruction */
static void PatchRelative( PBYTE pCode,   ULONG ulOffset,
                           void *Address, ULONG ulNext )
{
/* #if 32 bits and low byte first */
   ULONG ulBase = (ULONG) pCode + ulNext;
                                /* Relative to next instruction */
   ULONG ulRelative = (ULONG) Address - ulBase;

   pCode[ ulOffset     ] = ( ulRelative       ) & 0xFF;
   pCode[ ulOffset + 1 ] = ( ulRelative >>  8 ) & 0xFF;
   pCode[ ulOffset + 2 ] = ( ulRelative >> 16 ) & 0xFF;
   pCode[ ulOffset + 3 ] = ( ulRelative >> 24 ) & 0xFF;

/* #elseif 16 bits and low byte first */
/* #elseif 32 bits and high byte first */
/* #elseif ... */
/* #endif */
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
static PASM_CALL CreateFun( PSYMBOL pSymbols, PBYTE pCode )
{
   PASM_CALL asmRet = (PASM_CALL) hb_xgrab( sizeof( ASM_CALL ) );

   asmRet->pAsmData = (PBYTE) hb_xgrab( sizeof( prgFunction ) );
   memcpy( asmRet->pAsmData, prgFunction, sizeof( prgFunction ) );
                                              /* Copy new assembler code in */
/* #if INTEL32 */

   Patch( asmRet->pAsmData,  1, pSymbols );   /* Insert pointer to testsym  */
   Patch( asmRet->pAsmData,  6, pCode);       /* Insert pointer to testcode */
   PatchRelative( asmRet->pAsmData, 11, &VirtualMachine, 15 );
                                      /* Insert pointer to VirtualMachine() */

/* #elseif INTEL16 */
/* #elseif MOTOROLA */
/* #elseif ... */
/* #endif */
   return( asmRet );
}

