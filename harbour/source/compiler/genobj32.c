/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Compiler Windows/DOS OBJ32 generation
 *
 * Copyright 1999 Antonio Linares <alinares@fivetech.com>
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

#include "hbcomp.h"

#ifndef HARBOUR_OBJ_GENERATION

void hb_compGenObj32( PHB_FNAME pFileName )
{
   HB_SYMBOL_UNUSED( pFileName );

   printf( "\nThis feature is not included in this build." );
   fflush( stdout );
}

#else

static ULONG GetSymbolsSize( void );
static PCOMSYMBOL GetFirstSymbol( void );
static char * GetSymbolName( ULONG ulPos );
static ULONG GetPCodesSize( void );
static ULONG GetSymbolsAmount( void );
static BOOL IsExternal( ULONG ulSymbol );
static USHORT GetExternalPos( char * szExternal );
static void GenerateLocalNames( FILE * hObjFile );
static void GenerateSymbolsSegment( FILE * hObjFile );
static void GenerateDataSegment( FILE * hObjFile );
static void GenerateCodeSegment( FILE * hObjFile );
static void GenerateExternals( FILE * hObjFile );
static void putbyte( BYTE b, FILE * hObjFile, BYTE * pbChecksum );
static void putword( USHORT w, FILE * hObjFile, BYTE * pbChecksum );
static void CompiledFileName( FILE * hObjFile, char * szFileName );
static void CompilerVersion( FILE * hObjFile, char * szVersion );
static void LocalNames( FILE * hObjFile, char * szNames[] );
static void ExternalNames( FILE * hObjFile, char * szNames[] );
static void CodeSegment( FILE * hObjFile, BYTE * prgCode, ULONG ulPrgLen, USHORT wFunctions );
static void DataSegment( FILE * hObjFile, BYTE * symbol, ULONG wSymLen, ULONG wSymbols, ULONG ulTotalSize );
static void DefineSegment( FILE * hObjFile, BYTE bName, BYTE bClass, USHORT wLen );
static void PubDef( FILE * hObjFile, char * szName, USHORT wSegment, USHORT wOffset );
static void Fixup( FILE * hObjFile, BYTE bType, USHORT wOffset, BYTE bFlags, BYTE bSymbol );
static void EnumeratedData( FILE * hObjFile, BYTE bSegment, BYTE * pData, USHORT wLen, USHORT wOffset );
static void End( FILE * hObjFile );
static void GroupDef( FILE * hObjFile, BYTE bName, BYTE * aSegs );

static BYTE prgFunction[] = { 0x68, 0x00, 0x00, 0x00, 0x00, 0x68, 0x00, 0x00, 0x00,
                              0x00, 0xE8, 0x00, 0x00, 0x00, 0x00, 0x83, 0xC4, 0x08, 0xC3 };

static char * * externNames = 0;
static USHORT wExternals = 1; /* _hb_vmExecute is always added */
static char * szPrefix = "_HB_FUN_";

void hb_compGenObj32( PHB_FNAME pFileName )
{
  char szFileName[ _POSIX_PATH_MAX ];
  FILE * hObjFile;  /* file handle for OBJ output */

  if( ! pFileName->szExtension )
    pFileName->szExtension = ".obj";
  hb_fsFNameMerge( szFileName, pFileName );

  if( ( hObjFile = fopen( szFileName, "wb" ) ) == NULL )
    {
      hb_compGenError( hb_comp_szErrors, 'E', HB_COMP_ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
    }

  if( ! hb_comp_bQuiet )
  {
    printf( "Generating Windows/DOS OBJ32 output to \'%s\'... ", szFileName );
    fflush( stdout );
  }

  CompiledFileName( hObjFile, szFileName );
  CompilerVersion( hObjFile, "Harbour" );
  GenerateLocalNames( hObjFile );
  GenerateExternals( hObjFile );
  GenerateCodeSegment( hObjFile );
  GenerateDataSegment( hObjFile );
  GenerateSymbolsSegment( hObjFile );
  End( hObjFile );

  fclose( hObjFile );

  if( ! hb_comp_bQuiet )
    printf( "Done.\n" );
}

static ULONG GetSymbolsSize( void )
{
  return hb_comp_symbols.iCount * sizeof( HB_SYMB );
}

static PCOMSYMBOL GetFirstSymbol( void )
{
  PCOMSYMBOL pSymbol = hb_comp_symbols.pFirst;
  return pSymbol;
}

static char * GetSymbolName( ULONG ulPos )
{
  PCOMSYMBOL pSymbol = GetFirstSymbol();
  ULONG ul = 0;

  while( pSymbol && ( ul++ < ulPos ) )
    pSymbol = pSymbol->pNext;

  return pSymbol->szName;
}

static BYTE GetFunctionLocalsCount( PFUNCTION pFunction )
{
   PVAR pLocal = pFunction->pLocals;
   BYTE bLocals = 0;

   while( pLocal )
   {
      pLocal = pLocal->pNext;
      bLocals++;
   }
   return bLocals;
}

static ULONG GetPCodesSize( void )
{
  ULONG ulTotal = 0;
  PFUNCTION pFunction = hb_comp_functions.pFirst;

  if( ! hb_comp_bStartProc )
    pFunction = pFunction->pNext;

  while( pFunction )
    {
      ulTotal += pFunction->lPCodePos; //  + 1; /* HB_P_ENDPROC !!! */

      if( ! ( GetFunctionLocalsCount( pFunction ) || pFunction->wParamCount ) )
         ulTotal -= 3; /* remove frame 3 bytes pcode */

      if( ! ( pFunction->bFlags & FUN_USES_STATICS ) )
         ulTotal -= 3; /* remove statics frame */

      pFunction = pFunction->pNext;
    }
  return ulTotal;
}

static ULONG GetSymbolsAmount( void )
{
  PCOMSYMBOL pSymbol = GetFirstSymbol();
  ULONG ulAmount = 1;

  while( pSymbol->pNext )
    {
      ulAmount++;
      pSymbol = pSymbol->pNext;
    }
  return ulAmount;
}

static BOOL IsExternal( ULONG ulSymbol )
{
  PCOMSYMBOL pSymbol = GetFirstSymbol();
  ULONG ul = 0;

  while( ul++ < ulSymbol )
    pSymbol = pSymbol->pNext;

  return ! hb_compFunctionFind( pSymbol->szName );
}

static USHORT GetExternalPos( char * szExternal )
{
  USHORT w = 0;

  while( w < wExternals )
    {
      if( ! strcmp( szExternal, externNames[ w ] ) )
        break;
      w++;
    }

  return w;
}

static void GenerateLocalNames( FILE * hObjFile )
{
  char * localNames[] = { "_TEXT", "CODE", "_DATA", "DATA",
                          "HB_SYMBOLS", "HB_STARTSYMBOLS", "HB_ENDSYMBOLS", "SYMGROUP", 0 };

  LocalNames( hObjFile, localNames );
}

static void GenerateSymbolsSegment( FILE * hObjFile )
{
  BYTE symbolsData[] = { 0, 0, 0, 0, 0, 0 };
  BYTE groupSegments[] = { 3, 4, 5, 0 }; /* segments order for HB_... */

  DefineSegment( hObjFile, 7, 5, 0 ); /* 7 = HB_STARTSYMBOLS, 5 = DATA */
  DefineSegment( hObjFile, 6, /* "HB_SYMBOLS" position + 1 into localNames */
                 5, /* "DATA" position + 1 into localNames */
                 6 ); /* segment length */
  DefineSegment( hObjFile, 8, 5, 0 ); /* 8 = HB_ENDSYMBOLS, 5 = DATA */

  GroupDef( hObjFile, 8, groupSegments ); /* 8 = "SYMGROUP" localNames position */

  * ( USHORT * ) symbolsData = GetSymbolsAmount();

  EnumeratedData( hObjFile, 4, symbolsData, sizeof( symbolsData ), 0 ); /* 4 = HB_SYMBOLS defined segment */

  Fixup( hObjFile, 0xE4, 2, 0x54, 2 ); /* Data: symbols location */
}

static void GenerateDataSegment( FILE * hObjFile )
{
  HB_SYMB symbol;
  ULONG ulSize = GetSymbolsSize();
  PCOMSYMBOL pSymbol = GetFirstSymbol();
  ULONG ulSymbols = GetSymbolsAmount(), ul;

  while( pSymbol )
    {
      ulSize += strlen( pSymbol->szName ) + 1;
      pSymbol = pSymbol->pNext;
    }

  ulSize += GetPCodesSize();

  DefineSegment( hObjFile, 4, /* "_DATA" position + 1 into localNames */
                 5, /* "DATA" position + 1 into localNames */
                 ulSize ); /* segment length */

  memset( &symbol, 0, sizeof( symbol ) );
  DataSegment( hObjFile, (BYTE *) &symbol,
               sizeof( symbol ), GetSymbolsAmount(), ulSize );

  pSymbol = GetFirstSymbol();
  for( ul = 0; ul < ulSymbols; ul++ )
    {
      Fixup( hObjFile, 0xE4, ( ul * sizeof( HB_SYMB ) ), 0x54, 2 ); /* Data symbol name location */

      if( IsExternal( ul ) )
        {
          if( ! ( pSymbol->cScope & HB_FS_MESSAGE ) )
            Fixup( hObjFile, 0xE4, ( ul * sizeof( HB_SYMB ) ) + 5, 0x56,
                   GetExternalPos( GetSymbolName( ul ) ) + 1 );
        }
      else
        {
          /* if( ! ( pSymbol->cScope & HB_FS_MESSAGE ) ) */
          Fixup( hObjFile, 0xE4, ( ul * sizeof( HB_SYMB ) ) + 5, 0x54, 1 ); /* function address location */
        }
      pSymbol = pSymbol->pNext;
    }
}

static void GenerateCodeSegment( FILE * hObjFile )
{
  USHORT wFunctions = hb_comp_functions.iCount - ( hb_comp_bStartProc ? 0: 1 );
  ULONG ulSize    = wFunctions * sizeof( prgFunction );
  PFUNCTION pFunc = ( hb_comp_bStartProc ? hb_comp_functions.pFirst: hb_comp_functions.pFirst->pNext );
  USHORT w = 0;

  DefineSegment( hObjFile, 2, /* "_TEXT" position + 1 into localNames */
                 3, /* "CODE" position + 1 into localNames */
                 ulSize ); /* segment length */

  while( pFunc )
    {
      if( !( pFunc->cScope & ( HB_FS_STATIC | HB_FS_INIT | HB_FS_EXIT ) ) )
        PubDef( hObjFile, pFunc->szName, 1, w * sizeof( prgFunction ) );
      w++;
      pFunc = pFunc->pNext;
    }

  CodeSegment( hObjFile, prgFunction, sizeof( prgFunction ), wFunctions );

  for( w = 0; w < wFunctions; w++ )
    {
      /* prgFunction fixups */
      Fixup( hObjFile, 0xE4, ( w * sizeof( prgFunction ) ) + 1,
             0x54, 2 ); /* Data: symbols location */

      Fixup( hObjFile, 0xE4, ( w * sizeof( prgFunction ) ) + 6,
             0x54, 2 ); /* Data pcode location */

      Fixup( hObjFile, 0xA4, ( w * sizeof( prgFunction ) ) + 11,
             0x56, 1 ); /* External: _hb_vmExecute */
    }
}

static void GenerateExternals( FILE * hObjFile )
{
  USHORT w;
  PFUNCTION pFunc, pFTemp;

  /* calculate amount of externals */
  pFunc = hb_comp_funcalls.pFirst;
  while( pFunc )
    {
      if( ( pFTemp = hb_compFunctionFind( pFunc->szName ) ) == NULL || pFTemp == hb_comp_functions.pFirst )
        wExternals++;
      pFunc = pFunc->pNext;
    }
  if( wExternals )
    {
      externNames = ( char * * ) hb_xgrab( sizeof( char * ) * ( wExternals + 2 ) );
      w = 1;
      externNames[ 0 ] = "_hb_vmExecute";

      pFunc = hb_comp_funcalls.pFirst;
      while( pFunc )
        {
          if( ( pFTemp = hb_compFunctionFind( pFunc->szName ) ) == NULL || pFTemp == hb_comp_functions.pFirst )
            externNames[ w++ ] = pFunc->szName;
          pFunc = pFunc->pNext;
        }
      externNames[ w ] = 0;
      ExternalNames( hObjFile, externNames );
    }
}

static void putbyte( BYTE b, FILE * hObjFile, BYTE * pbChecksum )
{
  fputc( b, hObjFile );
  * pbChecksum += b;
}

static void putword( USHORT w, FILE * hObjFile, BYTE * pbChecksum )
{
  putbyte( HB_LOBYTE( w ), hObjFile, pbChecksum );
  putbyte( HB_HIBYTE( w ), hObjFile, pbChecksum );
}

static void CompiledFileName( FILE * hObjFile, char * szFileName )
{
  USHORT wLen = strlen( szFileName );
  BYTE bChk = 0; /* this is a checksum the linker will check to asure OBJ integrity */
  BYTE bChar;

  putbyte( 0x80, hObjFile, &bChk );  /* this tells the linker the kind of OBJ record this is */
  putbyte( 1 + 1 + wLen, hObjFile, &bChk ); /* now it comes the total length of this OBJ record */
  putbyte( 0, hObjFile, &bChk );

  putbyte( wLen, hObjFile, &bChk );     /* szFileName length */

  while( (bChar = * szFileName++) )
      putbyte( bChar, hObjFile, &bChk );   /* each of the szFileName characters */

  putbyte( 256 - bChk, hObjFile, &bChk ); /* a checksum that will be recalculated by the linker */
}

static void CompilerVersion( FILE * hObjFile, char * szVersion )
{
  USHORT wLen = strlen( szVersion );
  BYTE bChk = 0; /* this is a checksum the linker will check to asure OBJ integrity */
  BYTE bChar;

  putbyte( 0x88, hObjFile, &bChk );  /* this tells the linker the kind of OBJ record this is */
  putword( 3 + wLen, hObjFile, &bChk ); /* now it comes the total length of this OBJ record */
  putword( 0, hObjFile, &bChk );

  while( ( bChar = * szVersion++ ) )
      putbyte( bChar, hObjFile, &bChk );   /* each of the szFileName characters */

  putbyte( 256 - bChk, hObjFile, &bChk ); /* a checksum that will be recalculated by the linker */
}

static void LocalNames( FILE * hObjFile, char * szNames[] )
{
  BYTE b = 0, c;
  USHORT wTotalLen = 0;
  BYTE bChk = 0;

  while( szNames[ b ] )
    wTotalLen += strlen( szNames[ b++ ] );
  wTotalLen += 2 + b;

  putbyte( 0x96, hObjFile, &bChk );
  putword( wTotalLen, hObjFile, &bChk );
  putbyte( 0, hObjFile, &bChk );

  b = 0;
  while( szNames[ b ] )
    {
      putbyte( strlen( szNames[ b ] ), hObjFile, &bChk );

      c = 0;
      while( szNames[ b ][ c ] )
          putbyte( szNames[ b ][ c++ ], hObjFile, &bChk );
      b++;
    }
  putbyte( 256 - bChk, hObjFile, &bChk );
}

static void ExternalNames( FILE * hObjFile, char * szNames[] )
{
  BYTE b = 0, c;
  USHORT wTotalLen = 0;
  BYTE bChk = 0;

  while( szNames[ b ] )
  {
    if( b == 0 )
       wTotalLen += strlen( szNames[ b++ ] ) + 1;
    else
       wTotalLen += strlen( szPrefix ) + strlen( szNames[ b++ ] ) + 1;
  }
  wTotalLen += 2 + b - 1;

  putbyte( 0x8C, hObjFile, &bChk );
  putword( wTotalLen, hObjFile, &bChk );

  b = 0;
  while( szNames[ b ] )
    {
      if( b == 0 )
         putbyte( strlen( szNames[ b ] ), hObjFile, &bChk );
      else
         putbyte( strlen( szPrefix ) + strlen( szNames[ b ] ), hObjFile, &bChk );

      c = 0;

      if( b > 0 )
      {
         while( szPrefix[ c ] )
            putbyte( szPrefix[ c++ ], hObjFile, &bChk );
         c = 0;
      }

      while( szNames[ b ][ c ] )
          putbyte( szNames[ b ][ c++ ], hObjFile, &bChk );
      putbyte( 0, hObjFile, &bChk );
      b++;
    }
  putbyte( 256 - bChk, hObjFile, &bChk );
}

static void CodeSegment( FILE * hObjFile, BYTE * prgCode, ULONG ulPrgLen, USHORT wFunctions )
{
  BYTE bChk = 0;
  USHORT y;
  USHORT wTotalLen = ( ulPrgLen * wFunctions ) + 4;
  ULONG ul;
  PFUNCTION pFunction = hb_comp_functions.pFirst;
  ULONG ulPCodeOffset = hb_comp_symbols.iCount * sizeof( HB_SYMB );

  if( ! hb_comp_bStartProc )
    pFunction = pFunction->pNext;

  putbyte( 0xA0, hObjFile, &bChk );
  putword( wTotalLen, hObjFile, &bChk );
  putbyte( 1, hObjFile, &bChk ); /* 1 = _TEXT segment */
  putword( 0, hObjFile, &bChk ); /* 0 = offset */

  for( y = 0; y < wFunctions; y++ )
    {
      * ( ULONG * ) &prgCode[ 6 ] = ulPCodeOffset; /* function pcode offset */
      for( ul = 0; ul < ulPrgLen; ul++ )
          putbyte( * ( prgCode + ul ), hObjFile, &bChk );
      ulPCodeOffset += pFunction->lPCodePos; //  + 1; /* HB_P_ENDPROC !!! */

      if( ! ( GetFunctionLocalsCount( pFunction ) || pFunction->wParamCount ) )
         ulPCodeOffset -= 3; /* remove frame 3 bytes pcode */

      if( ! ( pFunction->bFlags & FUN_USES_STATICS ) )
         ulPCodeOffset -= 3; /* remove statics frame */

      pFunction = pFunction->pNext;
    }

  putbyte( 256 - bChk, hObjFile, &bChk );
}

static void DataSegment( FILE * hObjFile, BYTE * symbol, ULONG wSymLen, ULONG wSymbols,
                         ULONG ulSize )
{
  BYTE bChk = 0;
  ULONG w, y;
  USHORT wTotalLen = 4 + ulSize;
  PCOMSYMBOL pSymbol = GetFirstSymbol();
  PFUNCTION pFunction = hb_comp_functions.pFirst;
  ULONG ulSymbolNameOffset = GetSymbolsSize() + GetPCodesSize();
  ULONG ulFunctionOffset;

  if( ! hb_comp_bStartProc )
    pFunction = pFunction->pNext;

  putbyte( 0xA0, hObjFile, &bChk );
  putword( wTotalLen, hObjFile, &bChk );
  putbyte( 2, hObjFile, &bChk ); /* 2 = _DATA segment */
  putword( 0, hObjFile, &bChk ); /* 0 = offset */

  for( y = 0; y < wSymbols; y++ )
    {
      * ( ULONG * ) symbol = ulSymbolNameOffset;

      if( ! IsExternal( y ) )
        {
          ulFunctionOffset = ( hb_compFunctionGetPos( pSymbol->szName ) - 1 ) *
            sizeof( prgFunction );
          * ( ( ULONG * ) &symbol[ 5 ] ) = ulFunctionOffset;
        }
      else
        * ( ( ULONG * ) &symbol[ 5 ] ) = 0;

      if( pSymbol->cScope == HB_FS_MESSAGE )
        symbol[ 4 ] = HB_FS_PUBLIC;
      else
        symbol[ 4 ] = pSymbol->cScope;

      for( w = 0; w < wSymLen; w++ )
         putbyte( * ( symbol + w ), hObjFile, &bChk );

      ulSymbolNameOffset += strlen( pSymbol->szName ) + 1;
      pSymbol = pSymbol->pNext;
    }

  while( pFunction )
    {
      w = 0;
      while( w < pFunction->lPCodePos )
        {
           switch( pFunction->pCode[ w ] )
           {
              case HB_P_JUMPSHORT:
              case HB_P_JUMPSHORTFALSE:
              case HB_P_JUMPSHORTTRUE:
              {
                 putbyte( pFunction->pCode[ w ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 1 ], hObjFile, &bChk );
                 w += 2;
              }
              break;

              case HB_P_ARRAYDIM:
              case HB_P_ARRAYGEN:
              case HB_P_DO:
              case HB_P_FUNCTION:
              case HB_P_JUMP:
              case HB_P_JUMPFALSE:
              case HB_P_JUMPTRUE:
              case HB_P_LINE:
              case HB_P_MESSAGE:
              case HB_P_POPALIASEDFIELD:
              case HB_P_POPFIELD:
              case HB_P_POPLOCAL:
              case HB_P_POPMEMVAR:
              case HB_P_POPSTATIC:
              case HB_P_POPVARIABLE:
              case HB_P_PUSHALIASEDFIELD:
              case HB_P_PUSHALIASEDVAR:
              case HB_P_PUSHFIELD:
              case HB_P_PUSHINT:
              case HB_P_PUSHLOCAL:
              case HB_P_PUSHLOCALREF:
              case HB_P_PUSHMEMVAR:
              case HB_P_PUSHMEMVARREF:
              case HB_P_PUSHSTATIC:
              case HB_P_PUSHSTATICREF:
              case HB_P_PUSHSYM:
              case HB_P_PUSHVARIABLE:
              {
                 putbyte( pFunction->pCode[ w ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 1 ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 2 ], hObjFile, &bChk );
                 w += 3;
              }
              break;

              case HB_P_JUMPFAR:
              case HB_P_JUMPFARFALSE:
              case HB_P_JUMPFARTRUE:
              case HB_P_PARAMETER:
              case HB_P_SEQBEGIN:
              case HB_P_SEQEND:
              {
                 putbyte( pFunction->pCode[ w ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 1 ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 2 ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 3 ], hObjFile, &bChk );
                 w += 4;
              }
              break;

              case HB_P_PUSHLONG:
              case HB_P_STATICS:
              {
                 putbyte( pFunction->pCode[ w ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 1 ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 2 ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 3 ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 4 ], hObjFile, &bChk );
                 w += 5;
              }
              break;

              case HB_P_FRAME:
              {
                 BYTE bLocals = GetFunctionLocalsCount( pFunction );

                 if( bLocals || pFunction->wParamCount )
                 {
                    putbyte( pFunction->pCode[ w ], hObjFile, &bChk );
                    putbyte( bLocals - pFunction->wParamCount, hObjFile, &bChk );
                    putbyte( pFunction->wParamCount, hObjFile, &bChk );
                 }
                 w += 3;
              }
              break;

              case HB_P_SFRAME:
              {
                 USHORT wPos;
                 if( pFunction->bFlags & FUN_USES_STATICS )
                 {
                    hb_compSymbolFind( hb_comp_pInitFunc->szName, &wPos );
                    putbyte( pFunction->pCode[ w ], hObjFile, &bChk );
                    putword( wPos, hObjFile, &bChk );
                 }
                 w += 3;
              }
              break;

              case HB_P_PUSHSTR:
              {
                 USHORT uLen = pFunction->pCode[ w + 1 ] +
                               pFunction->pCode[ w + 2 ] * 256;
                 putbyte( pFunction->pCode[ w ], hObjFile, &bChk );
                 putword( uLen, hObjFile, &bChk );
                 w += 3;

                 if( uLen > 0 )
                 {
                    USHORT u = 0;

                    while( u++ < uLen )
                       putbyte( pFunction->pCode[ w++ ], hObjFile, &bChk );
                 }
              }
              break;

              case HB_P_LOCALNAME:
              {
                 putbyte( pFunction->pCode[ w ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 1 ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 2 ], hObjFile, &bChk );
                 w += 3;

                 do {
                    putbyte( pFunction->pCode[ w ], hObjFile, &bChk );
                 } while( pFunction->pCode[ w++ ] );
              }
              break;

              case HB_P_MODULENAME:
              {
                 putbyte( pFunction->pCode[ w++ ], hObjFile, &bChk );

                 do {
                    putbyte( pFunction->pCode[ w ], hObjFile, &bChk );
                 } while( pFunction->pCode[ w++ ] );
              }
              break;

              case HB_P_PUSHBLOCK:
              {
                 USHORT uVars = pFunction->pCode[ w + 5 ] +
                                pFunction->pCode[ w + 6 ] * 256;
                 putbyte( pFunction->pCode[ w ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 1 ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 2 ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 3 ], hObjFile, &bChk );
                 putbyte( pFunction->pCode[ w + 4 ], hObjFile, &bChk );
                 putword( uVars, hObjFile, &bChk );
                 w += 7;
                 while( uVars-- )
                 {
                    putbyte( pFunction->pCode[ w++ ], hObjFile, &bChk );
                    putbyte( pFunction->pCode[ w++ ], hObjFile, &bChk );
                 }
              }
              break;

              case HB_P_PUSHDOUBLE:
              {
                 int i = 0;
                 putbyte( pFunction->pCode[ w ], hObjFile, &bChk );
                 while( i++ < sizeof( double ) + sizeof( BYTE ) )
                    putbyte( pFunction->pCode[ ++w ], hObjFile, &bChk );
                 ++w;
              }
              break;

              default:
                 putbyte( pFunction->pCode[ w++ ], hObjFile, &bChk );
           }
        }

      pFunction = pFunction->pNext;
    }

  pSymbol = GetFirstSymbol();

  while( pSymbol )
    {
      for( w = 0; w < strlen( pSymbol->szName ); w++ )
          putbyte( pSymbol->szName[ w ], hObjFile, &bChk );

      putbyte( 0, hObjFile, &bChk );
      pSymbol = pSymbol->pNext;
    }

  putbyte( 256 - bChk, hObjFile, &bChk );
}

static void DefineSegment( FILE * hObjFile, BYTE bName, BYTE bClass, USHORT wLen )
{
  BYTE bChk = 0;

  putbyte( 0x98, hObjFile, &bChk );
  putbyte( 7, hObjFile, &bChk );      /* SegDef records have always this length */
  putbyte( 0, hObjFile, &bChk );

  putbyte( 0xA9, hObjFile, &bChk );
  putword( wLen, hObjFile, &bChk );
  putbyte( bName, hObjFile, &bChk );
  putbyte( bClass, hObjFile, &bChk );
  putbyte( 0, hObjFile, &bChk );

  putbyte( 256 - bChk, hObjFile, &bChk );
}

static void PubDef( FILE * hObjFile, char * szName, USHORT wSegment, USHORT wOffset )
{
  BYTE bChk = 0;
  BYTE bChar;
  USHORT wLen = 2 + 2 + strlen( szPrefix ) + strlen( szName ) + 2 + 1;
  char * szTemp;

  putbyte( 0x90, hObjFile, &bChk );
  putword( wLen, hObjFile, &bChk );
  putbyte( 0x00, hObjFile, &bChk );
  putbyte( wSegment, hObjFile, &bChk );
  putbyte( strlen( szPrefix ) + strlen( szName ), hObjFile, &bChk );

  szTemp = szPrefix;
  while( ( bChar = * szTemp++ ) )
     putbyte( bChar, hObjFile, &bChk );

  while( ( bChar = * szName++ ) )
     putbyte( bChar, hObjFile, &bChk );

  putword( wOffset, hObjFile, &bChk );
  putbyte( 0x00, hObjFile, &bChk );

  putbyte( 256 - bChk, hObjFile, &bChk );
}

static void Fixup( FILE * hObjFile, BYTE bType, USHORT wOffset, BYTE bFlags, BYTE bSymbol )
{
  BYTE bChk = 0;

  putbyte( 0x9D, hObjFile, &bChk );
  putword( 5, hObjFile, &bChk );
  putbyte( bType + HB_HIBYTE( wOffset ), hObjFile, &bChk );
  putbyte( HB_LOBYTE( wOffset ), hObjFile, &bChk );
  putbyte( bFlags, hObjFile, &bChk );
  putbyte( bSymbol, hObjFile, &bChk );

  putbyte( 256 - bChk, hObjFile, &bChk );
}

static void EnumeratedData( FILE * hObjFile, BYTE bSegment, BYTE * pData, USHORT wLen, USHORT wOffset )
{
  BYTE bChk = 0;
  USHORT w;

  putbyte( 0xA0, hObjFile, &bChk );
  putword( ( USHORT ) ( wLen + 4 ), hObjFile, &bChk );
  putbyte( bSegment, hObjFile, &bChk );
  putword( wOffset, hObjFile, &bChk );

  for( w = 0; w < wLen; w++ )
     putbyte( * ( pData + w ), hObjFile, &bChk );

  putbyte( 256 - bChk, hObjFile, &bChk );
}

static void End( FILE * hObjFile )
{
  BYTE bChk = 0;

  putbyte( 0x8A, hObjFile, &bChk );
  putbyte( 0x02, hObjFile, &bChk );
  putbyte( 0x00, hObjFile, &bChk );
  putbyte( 0x00, hObjFile, &bChk );
  putbyte( 256 - bChk, hObjFile, &bChk );
}

static void GroupDef( FILE * hObjFile, BYTE bName, BYTE * aSegs )
{
  BYTE bChk = 0;
  USHORT wRecLen = 2;
  USHORT w       = 0;

  while( aSegs[ w++ ] )
    wRecLen += 2;

  putbyte( 0x9A, hObjFile, &bChk );
  putword( wRecLen, hObjFile, &bChk );
  putbyte( bName + 1, hObjFile, &bChk );

  w = 0;
  while( aSegs[ w ] )
  {
      putbyte( 0xFF, hObjFile, &bChk );
      putbyte( aSegs[ w++ ], hObjFile, &bChk );
  }

  putbyte( 256 - bChk, hObjFile, &bChk );
}

#endif /* HARBOUR_OBJ_GENERATION */