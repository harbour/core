/*
 * $Id$
 */

/*
   Harbour Project source code

   Harbour OBJ32 Generation.

   Copyright 1999  Antonio Linares <alinares@fivetech.com>
   www - http://www.harbour-project.org

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version, with one exception:

   The exception is that if you link the Harbour Runtime Library (HRL)
   and/or the Harbour Virtual Machine (HVM) with other files to produce
   an executable, this does not by itself cause the resulting executable
   to be covered by the GNU General Public License. Your use of that
   executable is in no way restricted on account of linking the HRL
   and/or HVM code into it.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA (or visit
   their web site at http://www.gnu.org/).
*/

#include "extend.h"
#include "compiler.h"
#include "pcode.h"
#include "hberrors.h"

static void CompiledFileName( FILE * hObjFile, char * szFileName );
static void CompilerVersion( FILE * hObjFile, char * szVersion );
static void LocalNames( FILE * hObjFile, char * szNames[] );
static void ExternalNames( FILE * hObjFile, char * szNames[] );
static void Fixup( FILE * hObjFile, BYTE bType, WORD wOffset, BYTE bFlags, BYTE bSymbol );
static void DefineSegment( FILE * hObjFile, BYTE bName, BYTE bClass, WORD wLen );
static void PubDef( FILE * hObjFile, char * szName, WORD wSegment, WORD wOffset );
static void EnumeratedData( FILE * hObjFile, BYTE bSegment, BYTE * pData, WORD wLen, WORD wOffset );
static void GroupDef( FILE * hObjFile, BYTE bName, BYTE * aSegs );
static void End( FILE * hObjFile );

static void CodeSegment( FILE * hObjFile, BYTE * prgCode, ULONG ulPrgLen,
                         WORD wFunctions );
static void DataSegment( FILE * hObjFile, BYTE * symbol, WORD wSymLen,
                         WORD wSymbols, ULONG ulTotalSize );

static void GenerateLocalNames( FILE * hObjFile );
static void GenerateExternals( FILE * hObjFile );
static void GenerateCodeSegment( FILE * hObjFile );
static void GenerateDataSegment( FILE * hObjFile );
static void GenerateSymbolsSegment( FILE * hObjFile );

static BYTE prgFunction[] = { 0x68, 0x00, 0x00, 0x00, 0x00, 0x68, 0x00, 0x00, 0x00,
                              0x00, 0xE8, 0x00, 0x00, 0x00, 0x00, 0x83, 0xC4, 0x08, 0xC3 };

static char * * externNames = 0;
WORD wExternals = 1; /* _hb_vmExecute is always added */

void GenObj32( char * szObjFileName, char * szFileName )
{
  FILE * hObjFile;  /* file handle for OBJ output */

  if( ! ( hObjFile = fopen( szObjFileName, "wb" ) ) )
    {
      GenError( _szCErrors, 'E', ERR_CREATE_OUTPUT, szFileName, NULL );
      return;
    }

  if( ! _bQuiet )
    printf( "\nGenerating Windows/Dos OBJ 32 bits..." );

  CompiledFileName( hObjFile, szFileName );
  CompilerVersion( hObjFile, "Harbour" );
  GenerateLocalNames( hObjFile );
  GenerateExternals( hObjFile );
  GenerateCodeSegment( hObjFile );
  GenerateDataSegment( hObjFile );
  GenerateSymbolsSegment( hObjFile );
  End( hObjFile );

  fclose( hObjFile );

  if( ! _bQuiet )
    printf( "\n%s -> done!\n", szObjFileName );
}

static ULONG GetSymbolsSize( void )
{
  return ( symbols.iCount - ( _bStartProc ? 0: 1 ) ) * sizeof( HB_SYMB );
}

static PCOMSYMBOL GetFirstSymbol( void )
{
  PCOMSYMBOL pSymbol = symbols.pFirst;

  if( ! _bStartProc )
    pSymbol = pSymbol->pNext;

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

static ULONG GetPCodesSize( void )
{
  ULONG ulTotal = 0;
  PFUNCTION pFunction = functions.pFirst;

  if( ! _bStartProc )
    pFunction = pFunction->pNext;

  while( pFunction )
    {
      ulTotal += pFunction->lPCodePos + 1; /* HB_P_ENDPROC !!! */
      pFunction = pFunction->pNext;
    }
  return ulTotal;
}

ULONG GetSymbolsAmount( void )
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

BOOL IsExternal( ULONG ulSymbol )
{
  PCOMSYMBOL pSymbol = GetFirstSymbol();
  ULONG ul = 0;

  while( ul++ < ulSymbol )
    pSymbol = pSymbol->pNext;

  return ! GetFunction( pSymbol->szName );
}

WORD GetExternalPos( char * szExternal )
{
  WORD w = 0;

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

  * ( WORD * ) symbolsData = GetSymbolsAmount();

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
          if( ! ( pSymbol->cScope & FS_MESSAGE ) )
            Fixup( hObjFile, 0xE4, ( ul * sizeof( HB_SYMB ) ) + 5, 0x56,
                   GetExternalPos( GetSymbolName( ul ) ) + 1 );
        }
      else
        {
          /* if( ! ( pSymbol->cScope & FS_MESSAGE ) ) */
          Fixup( hObjFile, 0xE4, ( ul * sizeof( HB_SYMB ) ) + 5, 0x54, 1 ); /* function address location */
        }
      pSymbol = pSymbol->pNext;
    }
}

static void GenerateCodeSegment( FILE * hObjFile )
{
  WORD wFunctions = functions.iCount - ( _bStartProc ? 0: 1 );
  ULONG ulSize    = wFunctions * sizeof( prgFunction );
  PFUNCTION pFunc = ( _bStartProc ? functions.pFirst: functions.pFirst->pNext );
  WORD w = 0;

  DefineSegment( hObjFile, 2, /* "_TEXT" position + 1 into localNames */
                 3, /* "CODE" position + 1 into localNames */
                 ulSize ); /* segment length */

  while( pFunc )
    {
      if( !( pFunc->cScope & ( FS_STATIC | FS_INIT | FS_EXIT ) ) )
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
  WORD w;
  PFUNCTION pFunc, pFTemp;

  /* calculate amount of externals */
  pFunc = funcalls.pFirst;
  while( pFunc )
    {
      if( ! ( pFTemp = GetFunction( pFunc->szName ) ) || pFTemp == functions.pFirst )
        wExternals++;
      pFunc = pFunc->pNext;
    }
  if( wExternals )
    {
      externNames = ( char * * ) hb_xgrab( sizeof( char * ) * ( wExternals + 2 ) );
      w = 1;
      externNames[ 0 ] = "_hb_vmExecute";

      pFunc = funcalls.pFirst;
      while( pFunc )
        {
          if( ! ( pFTemp = GetFunction( pFunc->szName ) ) || pFTemp == functions.pFirst )
            externNames[ w++ ] = pFunc->szName;
          pFunc = pFunc->pNext;
        }
      externNames[ w ] = 0;
      ExternalNames( hObjFile, externNames );
    }
}

static void putbyte( BYTE b, FILE * hObjFile )
{
  fputc( b, hObjFile );
}

void putword( WORD w, FILE * hObjFile )
{
  putbyte( LOBYTE( w ), hObjFile );
  putbyte( HIBYTE( w ), hObjFile );
}

static void CompiledFileName( FILE * hObjFile, char * szFileName )
{
  WORD wLen = strlen( szFileName );
  BYTE bChk = 0; /* this is a checksum the linker will check to asure OBJ integrity */
  BYTE bChar;

  putbyte( 0x80, hObjFile );  /* this tells the linker the kind of OBJ record this is */
  bChk += 0x80;

  putbyte( 1 + 1 + wLen, hObjFile ); /* now it comes the total length of this OBJ record */
  bChk += ( 1 + 1 + wLen );
  putbyte( 0, hObjFile );

  putbyte( wLen, hObjFile );     /* szFileName length */
  bChk += wLen;

  while( (bChar = * szFileName++) )
    {
      putbyte( bChar, hObjFile );   /* each of the szFileName characters */
      bChk += bChar;
    }

  putbyte( 256 - bChk, hObjFile ); /* a checksum that will be recalculated by the linker */
}

static void CompilerVersion( FILE * hObjFile, char * szVersion )
{
  WORD wLen = strlen( szVersion );
  BYTE bChk = 0; /* this is a checksum the linker will check to asure OBJ integrity */
  BYTE bChar;

  putbyte( 0x88, hObjFile );  /* this tells the linker the kind of OBJ record this is */
  bChk += 0x88;

  putword( 3 + wLen, hObjFile ); /* now it comes the total length of this OBJ record */
  bChk += LOBYTE( 3 + wLen );
  bChk += HIBYTE( 3 + wLen );

  putword( 0, hObjFile );

  while( (bChar = * szVersion++) )
    {
      putbyte( bChar, hObjFile );   /* each of the szFileName characters */
      bChk += bChar;
    }

  putbyte( 256 - bChk, hObjFile ); /* a checksum that will be recalculated by the linker */
}

static void LocalNames( FILE * hObjFile, char * szNames[] )
{
  BYTE b = 0, c;
  WORD wTotalLen = 0;
  BYTE bChk = 0;

  while( szNames[ b ] )
    wTotalLen += strlen( szNames[ b++ ] );
  wTotalLen += 2 + b;

  putbyte( 0x96, hObjFile );
  bChk += 0x96;

  putbyte( LOBYTE( wTotalLen ), hObjFile );
  bChk += LOBYTE( wTotalLen );
  putbyte( HIBYTE( wTotalLen ), hObjFile );
  bChk += HIBYTE( wTotalLen );

  putbyte( 0, hObjFile );

  b = 0;
  while( szNames[ b ] )
    {
      putbyte( strlen( szNames[ b ] ), hObjFile );
      bChk += strlen( szNames[ b ] );

      c = 0;
      while( szNames[ b ][ c ] )
        {
          putbyte( szNames[ b ][ c ], hObjFile );
          bChk += szNames[ b ][ c++ ];
        }
      b++;
    }
  putbyte( 256 - bChk, hObjFile );
}

static void ExternalNames( FILE * hObjFile, char * szNames[] )
{
  BYTE b = 0, c;
  WORD wTotalLen = 0;
  BYTE bChk = 0;

  while( szNames[ b ] )
    wTotalLen += strlen( szNames[ b++ ] ) + 1;
  wTotalLen += 2 + b - 1;

  putbyte( 0x8C, hObjFile );
  bChk += 0x8C;

  putword( wTotalLen, hObjFile );
  bChk += LOBYTE( wTotalLen );
  bChk += HIBYTE( wTotalLen );

  b = 0;
  while( szNames[ b ] )
    {
      putbyte( strlen( szNames[ b ] ), hObjFile );
      bChk += strlen( szNames[ b ] );

      c = 0;

      while( szNames[ b ][ c ] )
        {
          putbyte( szNames[ b ][ c ], hObjFile );
          bChk += szNames[ b ][ c++ ];
        }
      putbyte( 0, hObjFile );
      b++;
    }
  putbyte( 256 - bChk, hObjFile );
}

static void CodeSegment( FILE * hObjFile, BYTE * prgCode, ULONG ulPrgLen, WORD wFunctions )
{
  BYTE bCheckSum = 0;
  WORD y;
  WORD wTotalLen = ( ulPrgLen * wFunctions ) + 4;
  ULONG ul;
  PFUNCTION pFunction = functions.pFirst;
  ULONG ulPCodeOffset = ( symbols.iCount - ( _bStartProc ? 0: 1 ) ) * sizeof( HB_SYMB );

  if( ! _bStartProc )
    pFunction = pFunction->pNext;

  putbyte( 0xA0, hObjFile );
  bCheckSum += 0xA0;

  putword( wTotalLen, hObjFile );
  bCheckSum += LOBYTE( wTotalLen );
  bCheckSum += HIBYTE( wTotalLen );

  putbyte( 1, hObjFile ); /* 1 = _TEXT segment */
  bCheckSum += 1;

  putword( 0, hObjFile ); /* 0 = offset */

  for( y = 0; y < wFunctions; y++ )
    {
      * ( ULONG * ) &prgCode[ 6 ] = ulPCodeOffset; /* function pcode offset */
      for( ul = 0; ul < ulPrgLen; ul++ )
        {
          putbyte( * ( prgCode + ul ), hObjFile );
          bCheckSum += * ( prgCode + ul );
        }
      ulPCodeOffset += pFunction->lPCodePos + 1; /* HB_P_ENDPROC !!! */
      pFunction = pFunction->pNext;
    }

  putbyte( 256 - bCheckSum, hObjFile );
}

static void DataSegment( FILE * hObjFile, BYTE * symbol, WORD wSymLen, WORD wSymbols,
                         ULONG ulSize )
{
  BYTE bCheckSum = 0;
  WORD w, y;
  WORD wTotalLen = 4 + ulSize;
  PCOMSYMBOL pSymbol = GetFirstSymbol();
  PFUNCTION pFunction = functions.pFirst;
  ULONG ulSymbolNameOffset = GetSymbolsSize() + GetPCodesSize();
  ULONG ulFunctionOffset;

  if( ! _bStartProc )
    pFunction = pFunction->pNext;

  putbyte( 0xA0, hObjFile );
  bCheckSum += 0xA0;

  putword( wTotalLen, hObjFile );
  bCheckSum += LOBYTE( wTotalLen );
  bCheckSum += HIBYTE( wTotalLen );

  putbyte( 2, hObjFile ); /* 2 = _DATA segment */
  bCheckSum += 2;

  putword( 0, hObjFile ); /* 0 = offset */

  for( y = 0; y < wSymbols; y++ )
    {
      * ( ULONG * ) symbol = ulSymbolNameOffset;

      if( ! IsExternal( y ) )
        {
          ulFunctionOffset = ( GetFunctionPos( pSymbol->szName ) - 1 ) *
            sizeof( prgFunction );
          * ( ( ULONG * ) &symbol[ 5 ] ) = ulFunctionOffset;
        }
      else
        * ( ( ULONG * ) &symbol[ 5 ] ) = 0;

      if( pSymbol->cScope == FS_MESSAGE )
        symbol[ 4 ] = FS_PUBLIC;
      else
        symbol[ 4 ] = pSymbol->cScope;

      for( w = 0; w < wSymLen; w++ )
        {
          putbyte( * ( symbol + w ), hObjFile );
          bCheckSum += * ( symbol + w );
        }

      ulSymbolNameOffset += strlen( pSymbol->szName ) + 1;
      pSymbol = pSymbol->pNext;
    }

  while( pFunction )
    {
      for( w = 0; w < pFunction->lPCodePos; w++ )
        {
          putbyte( pFunction->pCode[ w ], hObjFile );
          bCheckSum += pFunction->pCode[ w ];
        }
      putbyte( HB_P_ENDPROC, hObjFile );
      bCheckSum += HB_P_ENDPROC;
      pFunction = pFunction->pNext;
    }

  pSymbol = GetFirstSymbol();

  while( pSymbol )
    {
      for( w = 0; w < ( WORD ) strlen( pSymbol->szName ); w++ )
        {
          putbyte( pSymbol->szName[ w ], hObjFile );
          bCheckSum += pSymbol->szName[ w ];
        }
      putbyte( 0, hObjFile );
      pSymbol = pSymbol->pNext;
    }

  putbyte( 256 - bCheckSum, hObjFile );
}

static void DefineSegment( FILE * hObjFile, BYTE bName, BYTE bClass, WORD wLen )
{
  BYTE bCheckSum = 0;

  putbyte( 0x98, hObjFile );
  bCheckSum += 0x98;
  putbyte( 7, hObjFile );         /* SegDef records have always this length */
  bCheckSum += 7;
  putbyte( 0, hObjFile );

  putbyte( 0xA9, hObjFile );
  bCheckSum += 0xA9;

  putword( wLen, hObjFile );
  bCheckSum += LOBYTE( wLen );
  bCheckSum += HIBYTE( wLen );

  putbyte( bName, hObjFile );
  bCheckSum += bName;
  putbyte( bClass, hObjFile );
  bCheckSum += bClass;
  putbyte( 0, hObjFile );

  putbyte( 256 - bCheckSum, hObjFile );
}

static void PubDef( FILE * hObjFile, char * szName, WORD wSegment, WORD wOffset )
{
  BYTE bChk = 0;
  BYTE bChar;
  WORD wLen = 2 + 2 + strlen( szName ) + 2 + 1;

  putbyte( 0x90, hObjFile );
  bChk += 0x90;

  putword( wLen, hObjFile );
  bChk += LOBYTE( wLen );
  bChk += HIBYTE( wLen );

  putbyte( 0x00, hObjFile );
  putbyte( wSegment, hObjFile );
  bChk += wSegment;

  putbyte( strlen( szName ), hObjFile );
  bChk += strlen( szName );

  while( (bChar = * szName++) )
    {
      putbyte( bChar, hObjFile );
      bChk += bChar;
    }

  putword( wOffset, hObjFile);
  bChk += LOBYTE( wOffset );
  bChk += HIBYTE( wOffset );
  putbyte( 0x00, hObjFile );

  putbyte( 256 - bChk, hObjFile );
}

static void Fixup( FILE * hObjFile, BYTE bType, WORD wOffset, BYTE bFlags, BYTE bSymbol )
{
  BYTE bCheckSum = 0;

  putbyte( 0x9D, hObjFile ); bCheckSum += 0x9D;

  putword( 5, hObjFile ); bCheckSum += 5;

  putbyte( bType + HIBYTE( wOffset ), hObjFile );
  bCheckSum += bType + HIBYTE( wOffset );

  putbyte( LOBYTE( wOffset ), hObjFile );
  bCheckSum += LOBYTE( wOffset );

  putbyte( bFlags, hObjFile );  bCheckSum += bFlags;
  putbyte( bSymbol, hObjFile ); bCheckSum += bSymbol;

  putbyte( 256 - bCheckSum, hObjFile );
}

static void EnumeratedData( FILE * hObjFile, BYTE bSegment, BYTE * pData, WORD wLen, WORD wOffset )
{
  BYTE bCheckSum = 0;
  WORD w;

  putbyte( 0xA0, hObjFile );
  bCheckSum += 0xA0;

  putword( ( WORD ) ( wLen + 4 ), hObjFile );
  bCheckSum += LOBYTE( wLen + 4 );
  bCheckSum += HIBYTE( wLen + 4 );

  putbyte( bSegment, hObjFile );
  bCheckSum += bSegment;

  putword( wOffset, hObjFile );
  bCheckSum += wOffset;

  for( w = 0; w < wLen; w++ )
    {
      putbyte( * ( pData + w ), hObjFile );
      bCheckSum += * ( pData + w );
    }

  putbyte( 256 - bCheckSum, hObjFile );
}

static void End( FILE * hObjFile )
{
  BYTE bChk = 0;

  putbyte( 0x8A, hObjFile );  bChk += 0x8A;

  putbyte( 0x02, hObjFile );   bChk += 0x02;
  putbyte( 0x00, hObjFile );

  putbyte( 0x00, hObjFile );    bChk += 0x00;

  putbyte( 256 - bChk, hObjFile );
}

static void GroupDef( FILE * hObjFile, BYTE bName, BYTE * aSegs )
{
  BYTE bCheckSum = 0;
  WORD wRecLen   = 2;
  WORD w         = 0;

  while( aSegs[ w++ ] )
    wRecLen += 2;

  putbyte( 0x9A, hObjFile );
  bCheckSum += 0x9A;

  putword( wRecLen, hObjFile );
  bCheckSum += LOBYTE( wRecLen );
  bCheckSum += HIBYTE( wRecLen );

  putbyte( bName + 1, hObjFile );
  bCheckSum += bName + 1;

  w = 0;
  while( aSegs[ w ] )
    {
      putbyte( 0xFF, hObjFile );
      bCheckSum += 0xFF;
      putbyte( aSegs[ w ], hObjFile );
      bCheckSum += aSegs[ w ];
      w++;
    }

  putbyte( 256 - bCheckSum, hObjFile );
}
