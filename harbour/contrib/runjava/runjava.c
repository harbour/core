/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Harbour Portable Object (.HRB) file runner
 *
 * Copyright 1999 Matteo Baccan <baccan@isanet.it>
 *                Based on runlib.c of Eddie Runia <eddie@runia.com>
 * www - http://www.harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version, with one exception:
 *
 * The exception is that if you link the Harbour Runtime Library (HRL)
 * and/or the Harbour Virtual Machine (HVM) with other files to produce
 * an executable, this does not by itself cause the resulting executable
 * to be covered by the GNU General Public License. Your use of that
 * executable is in no way restricted on account of linking the HRL
 * and/or HVM code into it.
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

#include "hbapi.h"
#include "hbapiitm.h"
#include "hbapierr.h"
#include "hbapifs.h"
#include "hbvm.h"
#include "hbpcode.h"

/* Specific define for java runner */
#include "hbrunj.h"


/* TODO: Separate the loading/unloading and the caller functions,
         this way we could also call a specific function name from the
         .HRB file. This way we have basically reproduced the DLL
         functionality of Blinker.

         hnd := __hrbLoad( "MYHRB.HRB" )
         IF hnd != 0
            __hrbDo( hnd, "MYINITFUNC", par1, par2 )
            funhnd := __hrbGetHnd( hnd, "MYINITFUNC" )
            __hrbDoHnd( funhnd, par1, par2 )
            __hrbUnLoad( hnd )
         ENDIF

         */
/* TODO: Fill the error codes with valid ones (instead of 9999) */
/* TOFIX: Change this assembler hack to something standard and portable */
/* TODO: Change the fopen()/fread()/fclose() calls to hb_fs*() */
/* TOFIX: Fix the memory leak on error. */

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


static void      hb_hrbFileRead( char * file, char * cBuffer, int iSize, int iCount );
static BYTE      hb_hrbFileReadByte( char * file );
static char *    hb_hrbFileReadId( char * file );
static long      hb_hrbFileReadLong( char * file );


static ULONG     s_ulSymEntry = 0;              /* Link enhancement         */

JNIEXPORT jlong JNICALL Java_Harbour_Run( JNIEnv *env,
                                          jclass obj,
                                          jintArray pCode )
{

   ULONG ulSymbols;                             /* Number of symbols        */
   ULONG ulFuncs;                               /* Number of functions      */
   ULONG ulSize;                                /* Size of function         */
   ULONG ul, ulPos;

   PHB_SYMB pSymRead;                           /* Symbols read             */
   PHB_DYNF pDynFunc;                           /* Functions read           */
   PHB_DYNS pDynSym;

   int i;

   jlong bError = FALSE;

   jint* dmm = env->GetIntArrayElements( pCode, 0 );

   int nPos = env->GetArrayLength( pCode );
   char * file = (char*)hb_xgrab( nPos );

   while( nPos>0 ){
      file[nPos-1] = (char)dmm[nPos-1];
      nPos--;
   }

   env->ReleaseIntArrayElements( pCode, dmm, 0 );

   /* initialize internal data structures */
   hb_cmdargInit( __argc, __argv );
   hb_vmInit( FALSE );

   ulSymbols = hb_hrbFileReadLong( file );
   pSymRead = ( PHB_SYMB ) hb_xgrab( ulSymbols * sizeof( HB_SYMB ) );

   for( ul = 0; ul < ulSymbols; ul++ )       /* Read symbols in .HRB     */
   {
      pSymRead[ ul ].szName  = hb_hrbFileReadId( file );
      pSymRead[ ul ].cScope  = hb_hrbFileReadByte( file );
      pSymRead[ ul ].pFunPtr = ( PHB_FUNC ) ( ULONG ) hb_hrbFileReadByte( file );
      pSymRead[ ul ].pDynSym = NULL;
   }

   ulFuncs = hb_hrbFileReadLong( file );            /* Read number of functions */
   pDynFunc = ( PHB_DYNF ) hb_xgrab( ulFuncs * sizeof( HB_DYNF ) );
   for( ul = 0; ul < ulFuncs; ul++ )         /* Read symbols in .HRB     */
   {
      pDynFunc[ ul ].szName = hb_hrbFileReadId( file );

      ulSize = hb_hrbFileReadLong( file );      /* Read size of function    */
      pDynFunc[ ul ].pCode = ( BYTE * ) hb_xgrab( ulSize );
      hb_hrbFileRead( file, ( char * ) pDynFunc[ ul ].pCode, 1, ulSize );
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
/*          if(    hb_dynsymFind( pSymRead[ ul ].szName ) &&
                !( pSymRead[ ul ].cScope & FS_STATIC ) )
            {
               hb_errRT_BASE( EG_ARG, 9999, "Duplicate symbol", pSymRead[ ul ].szName );
               bError = TRUE;
               break;
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
            bError = TRUE;
            break;
         }
         pSymRead[ ul ].pFunPtr = pDynSym->pFunPtr;
      }
   }

   if( ! bError )
   {
      PHB_ITEM pRetVal;

      hb_vmProcessSymbols( pSymRead, ( USHORT ) ulSymbols );

      /* Initialize static variables first
       */
      for( ul = 0; ul < ulSymbols; ul++ )    /* Check INIT functions     */
      {
         if( ( pSymRead[ ul ].cScope & HB_FS_INITEXIT ) == HB_FS_INITEXIT )
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
         if( ( pSymRead[ ul ].cScope & HB_FS_INITEXIT ) == HB_FS_INIT )
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
      hb_itemCopy( pRetVal, &hb_stack.Return );

      for( ul = 0; ul < ulSymbols; ul++ )    /* Check EXIT functions     */
      {
         if( ( pSymRead[ ul ].cScope & HB_FS_INITEXIT ) == HB_FS_EXIT )
         {
            hb_vmPushSymbol( pSymRead + ul );
            hb_vmPushNil();
            hb_vmDo( 0 );                        /* Run exit function        */
            pSymRead[ ul ].cScope = pSymRead[ ul ].cScope & ( ~HB_FS_EXIT );
                                            /* Exit function cannot be
                                               handled by main in hvm.c */
         }
      }

      hb_itemReturn( pRetVal );
      hb_itemRelease( pRetVal );
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

   hb_xfree(file);
   hb_vmQuit();

   return bError;
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
static char * hb_hrbFileReadId( char * file )
{
   char * szTemp;                                /* Temporary buffer         */
   char * szIdx;
   char * szRet;

   BOOL  bCont = TRUE;

   szTemp = ( char * ) hb_xgrab( 256 );
   szIdx  = szTemp;
   do
   {
      hb_hrbFileRead( file, szIdx, 1, 1 );
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


static BYTE hb_hrbFileReadByte( char * file )
{
   BYTE bRet;

   hb_hrbFileRead( file, ( char * ) &bRet, 1, 1 );

   return bRet;
}


static long hb_hrbFileReadLong( char * file )
{
   char cLong[ 4 ];                               /* Temporary long           */

   hb_hrbFileRead( file, cLong, 4, 1 );

   return ( ( BYTE ) cLong[ 0 ] )             +
          ( ( BYTE ) cLong[ 1 ] ) * 0x100     +
          ( ( BYTE ) cLong[ 2 ] ) * 0x10000   +
          ( ( BYTE ) cLong[ 3 ] ) * 0x1000000 ;
}


/*  hb_hrbFileRead
    Controlled read from file. If errornous -> Break */
static void hb_hrbFileRead( char * file, char * cBuffer, int iSize, int iCount )
{
   static long nPos = 0;
   long nChar = iSize*iCount;

   while( nChar>0 ){
      cBuffer[nChar-1] = ((char*)file+nPos)[nChar-1];
	  nChar--;
   }
   nPos += iSize*iCount;
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
   PASM_CALL asmRet;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbAsmCreateFun(%p, %p)", pSymbols, pCode));

   asmRet = ( PASM_CALL ) hb_xgrab( sizeof( ASM_CALL ) );
   asmRet->pAsmData = ( BYTE * ) hb_xgrab( sizeof( prgFunction ) );
   memcpy( asmRet->pAsmData, prgFunction, sizeof( prgFunction ) );
                                              /* Copy new assembler code in */
/* #if INTEL32 */

   hb_hrbAsmPatch( asmRet->pAsmData, 1, pSymbols );   /* Insert pointer to testsym */
   hb_hrbAsmPatch( asmRet->pAsmData, 6, pCode );      /* Insert pointer to testcode */
   hb_hrbAsmPatchRelative( asmRet->pAsmData, 11, hb_vmExecute, 15 );
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
   HB_TRACE(HB_TR_DEBUG, ("hb_hrbAsmPatch(%p, %lu, %p)", pCode, ulOffset, Address));

/* #if 32 bits and low byte first */

   pCode[ ulOffset     ] = ( BYTE ) ( ( ( ULONG ) Address       ) & 0xFF );
   pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ( ULONG ) Address >>  8 ) & 0xFF );
   pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ( ULONG ) Address >> 16 ) & 0xFF );
   pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ( ULONG ) Address >> 24 ) & 0xFF );

/* #elseif 16 bits and low byte first */
/* #elseif 32 bits and high byte first */
/* #elseif ... */
/* #endif */
}


/* Intel specific ?? Patch an address relative to the next instruction */
static void hb_hrbAsmPatchRelative( BYTE * pCode, ULONG ulOffset,
                                    void * Address, ULONG ulNext )
{
   ULONG ulBase;
   ULONG ulRelative;

   HB_TRACE(HB_TR_DEBUG, ("hb_hrbAsmPatchRelative(%p, %lu, %p, %lu)", pCode, ulOffset, Address, ulNext));

/* #if 32 bits and low byte first */
   ulBase = ( ULONG ) pCode + ulNext;
                                /* Relative to next instruction */
   ulRelative = ( ULONG ) Address - ulBase;

   pCode[ ulOffset     ] = ( BYTE ) ( ( ulRelative       ) & 0xFF );
   pCode[ ulOffset + 1 ] = ( BYTE ) ( ( ulRelative >>  8 ) & 0xFF );
   pCode[ ulOffset + 2 ] = ( BYTE ) ( ( ulRelative >> 16 ) & 0xFF );
   pCode[ ulOffset + 3 ] = ( BYTE ) ( ( ulRelative >> 24 ) & 0xFF );

/* #elseif 16 bits and low byte first */
/* #elseif 32 bits and high byte first */
/* #elseif ... */
/* #endif */
}

