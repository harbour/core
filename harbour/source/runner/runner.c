#include "pcode.h"

#define FILE _FILE
#include <stdio.h>

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

     /* This is the assembler output from : VirtualMachine(pcode,symbols). */

/* #elseif INTEL16 */
/* #elseif MOTOROLA */
/* #elseif ... */
/* #endif */


typedef union
{
   PBYTE       pAsmData;       /* The assembler bytes            */
   HARBOURFUNC pFunPtr;        /* The (dynamic) harbour function */
} ASM_CALL, *PASM_CALL;

typedef struct
{
   char      *szName;          /* Name of the function         */
   PASM_CALL  pAsmCall;        /* Assembler call               */
   PBYTE      pCode;           /* P-code                       */
} DYNFUNC, *PDYNFUNC;


#define SYM_NOLINK  0              /* Symbol does not have to be linked */
#define SYM_FUNC    1              /* Defined function                  */
#define SYM_EXTERN  2              /* Previously defined function       */

#define SYM_NOT_FOUND 0xFFFFFFFF   /* Symbol not found. FindSymbol      */

PASM_CALL CreateFun( PSYMBOL, PBYTE );   /* Create a dynamic function*/
void      Do( WORD );
ULONG     FindSymbol( char *, PDYNFUNC, ULONG );
HARBOUR   HB_RUN();
void      HRB_FileClose( _FILE * );
void      HRB_FileRead ( char *, int, int, FILE * );
_FILE    *HRB_FileOpen ( char * );
void      Push( PITEM );
void      PushNil( void );
void      PushSymbol( PSYMBOL );
char     *ReadId( FILE * );
long      ReadLong( FILE * );

#undef  FILE
#include "run_exp.h"
#define FILE _FILE

/*
 *
 * This file contains the exportable functions available to the Harbour program
 *
 * Currently being discussed in 'Static initializers'
 *
 * If the discussion has finished, it can be removed from here.
 *
 */

#include <init.h>

ULONG ulSymEntry = 0;                           /* Link enhancement         */

/*
   HB_Run alias TugBoat alias Runner.

   This program will get the data from the .HRB file and run the p-code
   contained in it.

   In due time it should also be able to collect the data from the
   binary/executable itself
*/
HARBOUR HB_RUN( void )                          /* HB_Run( <cFile> )        */
{
   char  cLong[4];                              /* Temporary long           */
   char *szFileName;
   char *szTemp;                                /* Temporary buffer         */
   char *szIdx;

   FILE *file;

   ULONG ulSymbols;                             /* Number of symbols        */
   ULONG ulFuncs;                               /* Number of functions      */
   ULONG ulSize;                                /* Size of function         */
   ULONG ul, ulPos;

   int i;

   BYTE  bCont;

   PSYMBOL  pSymRead;                           /* Symbols read             */
   PDYNFUNC pDynFunc;                           /* Functions read           */
   PDYNSYM  pDynSym;

   if( _pcount() == 0 )
      printf( "\nPlease give HRB file name\n" );
   else
   {
      szFileName = _parc( 1 );
      file = HRB_FileOpen( szFileName );        /* Open as binary           */
      if( file )
      {
         ulSymbols = ReadLong( file );
         pSymRead = _xgrab( ulSymbols * sizeof( SYMBOL ) );

         szTemp   = _xgrab( 256 );              /* Must be enough for now   */

         for( ul=0; ul < ulSymbols; ul++)       /* Read symbols in .HRB     */
         {
            pSymRead[ ul ].szName  = ReadId( file );

            HRB_FileRead( szTemp, 2, 1, file );

            pSymRead[ ul ].cScope  = szTemp[ 0 ];
            pSymRead[ ul ].pFunPtr = (void *) szTemp[ 1 ];
            pSymRead[ ul ].pDynSym = NULL;
         }

         ulFuncs = ReadLong( file );            /* Read number of functions */
         pDynFunc = ( PDYNFUNC ) _xgrab( ulFuncs * sizeof( DYNFUNC ) );
         for( ul=0; ul < ulFuncs; ul++)         /* Read symbols in .HRB     */
         {
            pDynFunc[ ul ].szName = ReadId( file );

            ulSize = ReadLong( file ) + 1;      /* Read size of function    */
            pDynFunc[ ul ].pCode = _xgrab( ulSize );
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
                  pSymRead[ ul ].pFunPtr = pDynFunc[ ulPos ].pAsmCall->pFunPtr;
               else
                  pSymRead[ ul ].pFunPtr = (void *) SYM_EXTERN;
            }
            if( ( (ULONG) pSymRead[ ul ].pFunPtr ) == SYM_EXTERN )
            {                                   /* External function        */
               pDynSym = FindDynSym( pSymRead[ ul ].szName );
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

         for( ul = 0; ul < ulSymbols; ul++ )    /* Check INIT functions     */
         {
            if( pSymRead[ ul ].cScope & FS_INIT )
            {
                PushSymbol( pSymRead + ul );
                PushNil();
                for( i = 0; i < (_pcount() - 1); i++ )
                   Push( _param( i + 2, IT_ANY ) );
                                                /* Push other cmdline params*/
                Do( _pcount() - 1 );            /* Run init function        */
            }
         }

         PushSymbol( pSymRead );
         PushNil();
         for( i = 0; i < (_pcount() - 1); i++ )
            Push( _param( i + 2, IT_ANY ) );    /* Push other cmdline params*/
         Do( _pcount() - 1 );                   /* Run the thing !!!        */

         for( ul = 0; ul < ulSymbols; ul++ )    /* Check EXIT functions     */
         {
            if( pSymRead[ ul ].cScope & FS_EXIT )
            {
                PushSymbol( pSymRead + ul );
                PushNil();
                Do( 0 );                        /* Run exit function        */
                pSymRead[ ul ].cScope = pSymRead[ ul ].cScope & (~FS_EXIT);
                      /* Exit function cannot be handled by main() in hvm.c */
            }
         }

         for( ul = 0; ul < ulFuncs; ul++ )
         {
            _xfree( pDynFunc[ ul ].pAsmCall->pAsmData );
            _xfree( pDynFunc[ ul ].pAsmCall           );
            _xfree( pDynFunc[ ul ].pCode              );
            _xfree( pDynFunc[ ul ].szName             );
         }

         for( ul = 0; ul < ulSymbols; ul++ )
         {
            _xfree( pSymRead[ ul ].szName );
         }

         _xfree( pDynFunc );
         _xfree( szTemp );
         _xfree( pSymRead );
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
   char *szFileName;
   char *szTemp;                                /* Temporary buffer         */
   char *szIdx;
   char *szRet;

   BYTE  bCont = TRUE;

   szTemp = _xgrab( 256 );
   szIdx  = szTemp;
   do
   {
      HRB_FileRead( szIdx, 1, 1, file );
      if( *szIdx )
         szIdx++;
      else
         bCont = FALSE;
   } while( bCont );

   szRet = (char *) _xgrab( szIdx - szTemp + 1 );
   strcpy( szRet, szTemp );
   _xfree( szTemp );

   return( szRet );
}


long ReadLong( FILE *file )
{
   char cLong[4];                               /* Temporary long           */

   HRB_FileRead( cLong, 4, 1, file );           /* Read number of symbols   */

   if( cLong[3] )                               /* Convert to long if ok    */
   {
      PITEM pError = _errNew();
      _errPutDescription(pError, "Error reading .HRB file");
      _errLaunch(pError);
      _errRelease(pError);
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
      PITEM pError = _errNew();
      _errPutDescription(pError, "Error reading .HRB file");
      _errLaunch(pError);
      _errRelease(pError);
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
   PASM_CALL asmRet = (PASM_CALL) _xgrab( sizeof( ASM_CALL ) );

   asmRet->pAsmData = (PBYTE) _xgrab( sizeof( prgFunction ) );
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

