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
  char compiler[ 70 ];

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

  sprintf( compiler, "Harbour Compiler %d.%d%s (Build %d) (%04d.%02d.%02d) (%s)",
           HB_VER_MAJOR, HB_VER_MINOR, HB_VER_REVISION, HB_VER_BUILD, HB_VER_YEAR,
           HB_VER_MONTH, HB_VER_DAY, HB_VER_LEX );

  CompiledFileName( hObjFile, szFileName );
  CompilerVersion( hObjFile, compiler );
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

static ULONG GetPCodesSize( void )
{
  ULONG ulTotal = 0;
  PFUNCTION pFunction = hb_comp_functions.pFirst;

  if( ! hb_comp_bStartProc )
    pFunction = pFunction->pNext;

  while( pFunction )
    {
      ulTotal += pFunction->lPCodePos;
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
  char * localNames[] = { "_TEXT", "CODE",
                          "_NULL", "_DATA", "DATA",
                          "_BSS", "BSS", "DGROUP",
                          "HB_STARTSYMBOLS", "HB_SYMBOLS", "HB_ENDSYMBOLS", "HARBOUR",
                          "HB_STARTBORSYMBOLS", "_INIT_", "HB_ENDBORSYMBOLS", "INITDATA", "BORLAND",
                          0 };

  LocalNames( hObjFile, localNames );
}

static void GenerateSymbolsSegment( FILE * hObjFile )
{
  BYTE symbolsData[]   = { 0, 0, 0, 0, 0, 0, 0, 0 };
  BYTE groupDGroup[]   = { 2, 3, 4, 0 }; /* segments defined order for DGROUP */
  BYTE groupSymGroup[] = { 5, 6, 7, 0 }; /* segments defined order for SYMGROUP */
  BYTE groupInitData[] = { 8, 9, 10, 0 }; /* segments defined order for INITDATA */

  DefineSegment( hObjFile, 10,   /* HB_STARTSYMBOLS position + 1 into localnames */
                            6,   /* "DATA" position + 1 into localNames */
                            0 ); /* segment length */
  DefineSegment( hObjFile, 11,   /* HB_SYMBOLS position + 1 into localNames */
                            6,   /* "DATA" position + 1 into localNames */
                            8 ); /* segment length */
  DefineSegment( hObjFile, 12,   /* HB_ENDSYMBOLS position + 1 into localNames */
                            6,   /* "DATA" position + 1 into localNames */
                            0 ); /* segment length */

  DefineSegment( hObjFile, 14,   /* HB_STARTBORSYMBOLS position + 1 into localnames */
                           17,   /* INITDATA position + 1 into localNames */
                            0 ); /* segment length */
  DefineSegment( hObjFile, 15,   /* HB_STARTSYMBOLS position + 1 into localnames */
                           17,   /* INITDATA position + 1 into localNames */
                            0 ); /* segment length */
  DefineSegment( hObjFile, 16,   /* HB_ENDBORSYMBOLS position + 1 into localnames */
                           17,   /* INITDATA position + 1 into localNames */
                            0 ); /* segment length */

  GroupDef( hObjFile,  8, groupDGroup );   /* "DGROUP" localNames position - 1 */
  GroupDef( hObjFile, 12, groupSymGroup ); /* "SYMGROUP" localNames position - 1 */
  GroupDef( hObjFile, 17, groupInitData ); /* "BORLAND" localNames position - 1 */

  * ( USHORT * ) symbolsData = GetSymbolsAmount();

  EnumeratedData( hObjFile, 6, symbolsData, sizeof( symbolsData ), 0 ); /* HB_SYMBOLS defined order segment */

  Fixup( hObjFile, 0xE4, 4, /* offset into HB_SYMBOLS segment */
                      0x54,
                         4 ); /* DATA segment defined order */
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

  DefineSegment( hObjFile,  4,   /* _NULL position + 1 into localnames */
                            6,   /* "DATA" position + 1 into localNames */
                            0 ); /* segment length */
  DefineSegment( hObjFile,  7,   /* _BSS position + 1 into localNames */
                            8,   /* "BSS" position + 1 into localNames */
                            0 ); /* segment length */
  DefineSegment( hObjFile, 5, /* "_DATA" position + 1 into localNames */
                           6, /* "DATA" position + 1 into localNames */
                    ulSize ); /* segment length */

  memset( &symbol, 0, sizeof( symbol ) );
  DataSegment( hObjFile, (BYTE *) &symbol,
               sizeof( symbol ), GetSymbolsAmount(), ulSize );

  pSymbol = GetFirstSymbol();
  for( ul = 0; ul < ulSymbols; ul++ )
    {
      Fixup( hObjFile, 0xE4, ( ul * sizeof( HB_SYMB ) ), 0x54, 4 ); /* 4 = Data symbol name location */

      if( IsExternal( ul ) )
        {
          if( ! ( pSymbol->cScope & HB_FS_MESSAGE ) )
            Fixup( hObjFile, 0xE4, ( ul * sizeof( HB_SYMB ) ) + 8, 0x56,
                   GetExternalPos( GetSymbolName( ul ) ) + 1 );
        }
      else
        {
          /* if( ! ( pSymbol->cScope & HB_FS_MESSAGE ) ) */
          Fixup( hObjFile, 0xE4, ( ul * sizeof( HB_SYMB ) ) + 8, 0x54, 1 ); /* function address location */
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
             0x54, 4 ); /* 4 = DATA segment defined order */

      Fixup( hObjFile, 0xE4, ( w * sizeof( prgFunction ) ) + 6,
             0x54, 4 ); /* DATA segment define order - pcode location */

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

  while( ( bChar = * szFileName++ ) != 0 )
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

  while( ( bChar = * szVersion++ ) != 0 )
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
      ulPCodeOffset += pFunction->lPCodePos;
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
  putbyte( 4, hObjFile, &bChk ); /* 2 = _DATA segment defined order */
  putword( 0, hObjFile, &bChk ); /* 0 = offset */

  for( y = 0; y < wSymbols; y++ )
    {
      * ( ULONG * ) symbol = ulSymbolNameOffset;

      if( ! IsExternal( y ) )
        {
          ulFunctionOffset = ( hb_compFunctionGetPos( pSymbol->szName ) - 1 ) *
            sizeof( prgFunction );
          * ( ( ULONG * ) &symbol[ 8 ] ) = ulFunctionOffset; /* 8 offset of function pointer into symbol */
        }
      else
        * ( ( ULONG * ) &symbol[ 8 ] ) = 0; /* 8 offset of function pointer into symbol */

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
         putbyte( pFunction->pCode[ w++ ], hObjFile, &bChk );

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
  while( ( bChar = * szTemp++ ) != 0 )
     putbyte( bChar, hObjFile, &bChk );

  while( ( bChar = * szName++ ) != 0 )
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