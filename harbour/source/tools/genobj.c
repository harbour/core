/* Windows/DOS OBJs 32 bits generation support routines */

#include <fcntl.h>
#include <io.h>
#include <stdio.h>
#include <string.h>
#include <types.h>

void CompiledFileName( int hObjFile, char * szFileName );
void CompilerVersion( int hObjFile, char * szFileName );
void LocalNames( int hObjFile, char * szNames[] );
void PubDef( int hObjFile, char * szName, WORD wSegment, WORD wOffset );
void ExternalNames( int hObjFile, char * szNames[] );
void DefineSegment( int hObjFile, BYTE bName, BYTE bClass, WORD wLen );
void InitSegment( int hObjFile );
void EnumeratedData( int hObjFile, BYTE bSegment, BYTE * pData, WORD wLen, WORD wOffset );
void Fixup( int hObjFile, BYTE bType, WORD wOffset, BYTE bFlags, BYTE bSymbol );
void CodeSegment( int hObjFile, BYTE * initCode, WORD wInitLen,
                  BYTE * prgCode, WORD wPrgLen, WORD wFunctions );
void End( int hObjFile );

int main( int argc, char * argv[] )
{
   int hObjFile = open( "test.obj", O_CREAT | O_TRUNC | O_RDWR | O_BINARY );
   char * localNames[] = { "_TEXT", "CODE", "_DATA", "DATA", "DGROUP",
                           "_BSS", "BSS", "_INIT_", "INITDATA", 0 };
   char * externalNames[] = { "QOUT", "_ProcessSymbols", "_VirtualMachine", 0 };
   BYTE initData[] = { 0x00, 0x64, 0x00, 0x00, 0x00, 0x00 };
   BYTE initCode[] = { 0x6A, 0x02, 0x68, 0x00, 0x00, 0x00, 0x00, 0xE8, 0x00, 0x00,
                       0x00, 0x00, 0x83, 0xC4, 0x08, 0xC3 };
   BYTE prgFunction[] = { 0x68, 0x00, 0x00, 0x00, 0x00, 0x68, 0x1A, 0x00, 0x00,
                    0x00, 0xE8, 0x00, 0x00, 0x00, 0x00, 0x83, 0xC4, 0x08, 0xC3 };
   BYTE symbol[] = { 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x00 };
   WORD w;

   CompiledFileName( hObjFile, "test.prg" );
   CompilerVersion( hObjFile, "Harbour build 21-1" );
   LocalNames( hObjFile, localNames );
   ExternalNames( hObjFile, externalNames );

   /* code segment */
   DefineSegment( hObjFile, 2, /* "_TEXT" position + 1 into localNames */
                            3, /* "CODE" position + 1 into localNames */
                            0x3D ); /* segment length */

   PubDef( hObjFile, "MAIN", 1, 0x10 ); /* 1 = _TEXT segment, 0x10 offset into it );

   /* data segment */
   DefineSegment( hObjFile, 4, /* "_DATA" position + 1 into localNames */
                            5, /* "DATA" position + 1 into localNames */
                            0x48 ); /* segment length */
   InitSegment( hObjFile );

   CodeSegment( hObjFile, initCode, sizeof( initCode ), prgFunction,
                sizeof( prgFunction ), 3 ); /* 1 PRG function only */

   /* init fixups */
   Fixup( hObjFile, 0xE4, 3, 0x54, 2 ); /* Data: symbols location */
   Fixup( hObjFile, 0xA4, 8, 0x56, 2 ); /* External: _ProcessSymbols */

   for( w = 0; w < 3; w++ )  /* 3 PRG functions */
   {
      /* prgFunction fixups */
      Fixup( hObjFile, 0xE4, sizeof( initCode ) +
             ( w * sizeof( prgFunction ) ) + 1, 0x54, 2 ); /* Data: symbols location */

      Fixup( hObjFile, 0xA4, sizeof( initCode ) +
             ( w * sizeof( prgFunction ) ) + 11, 0x56, 3 ); /* External: _VirtualMachine */
   }

   EnumeratedData( hObjFile, 2, symbol, sizeof( symbol ), 0 ); /* 2 = _DATA segment */
   Fixup( hObjFile, 0xE4, 0, 0x54, 2 ); /* 0xE4 type, 0 offset, 0x54 flag, 2 = segment */
   Fixup( hObjFile, 0xE4, 5, 0x54, 1 ); /* 0xE4 type, 5 offset, 0x54 flag, 1 = segment */

   EnumeratedData( hObjFile, 3, initData, sizeof( initData ), 0 ); /* 3 = _INIT_ segment */
   Fixup( hObjFile, 0xE4, 2, 0x54, 1 ); /* 0xE4 type, 2 offset, 0x54 flag, 1 = symbol */

   End( hObjFile );

   close( hObjFile );
}

void putbyte( BYTE b, int hObjFile )
{
   write( hObjFile, &b, 1 );
}

void putword( WORD w, int hObjFile )
{
   write( hObjFile, &w, 2 );
}

void CompiledFileName( int hObjFile, char * szFileName )
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

   while( bChar = * szFileName++ )
   {
      putbyte( bChar, hObjFile );   /* each of the szFileName characters */
      bChk += bChar;
   }

   putbyte( 256 - bChk, hObjFile ); /* a checksum that will be recalculated by the linker */
}

void CompilerVersion( int hObjFile, char * szVersion )
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

   while( bChar = * szVersion++ )
   {
      putbyte( bChar, hObjFile );   /* each of the szFileName characters */
      bChk += bChar;
   }

   putbyte( 256 - bChk, hObjFile ); /* a checksum that will be recalculated by the linker */
}

void LocalNames( int hObjFile, char * szNames[] )
{
   BYTE b = 0, c;
   WORD wTotalLen = 0;
   BYTE bChk = 0;
   BYTE bChar;

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

void PubDef( int hObjFile, char * szName, WORD wSegment, WORD wOffset )
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

   while( bChar = * szName++ )
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

void ExternalNames( int hObjFile, char * szNames[] )
{
   BYTE b = 0, c;
   WORD wTotalLen = 0;
   BYTE bChk = 0;
   BYTE bChar;

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

void DefineSegment( int hObjFile, BYTE bName, BYTE bClass, WORD wLen )
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

void InitSegment( int hObjFile )
{
   BYTE bCheckSum = 0;

   putbyte( 0x98, hObjFile );
   bCheckSum += 0x98;
   putbyte( 7, hObjFile );         /* SegDef records have always this length */
   bCheckSum += 7;
   putbyte( 0, hObjFile );
   putbyte( 0x49, hObjFile );
   bCheckSum += 0x49;

   putword( 6, hObjFile );
   bCheckSum += 6;

   putbyte( 9, hObjFile );
   bCheckSum += 9;
   putbyte( 10, hObjFile );
   bCheckSum += 10;
   putbyte( 0, hObjFile );

   putbyte( 256 - bCheckSum, hObjFile );
}

void EnumeratedData( int hObjFile, BYTE bSegment, BYTE * pData, WORD wLen, WORD wOffset )
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

void CodeSegment( int hObjFile, BYTE * initCode, WORD wInitLen,
                  BYTE * prgCode, WORD wPrgLen, WORD wFunctions )
{
   BYTE bCheckSum = 0;
   WORD w, y;
   WORD wTotalLen = wInitLen + ( wPrgLen * wFunctions ) + 4;

   putbyte( 0xA0, hObjFile );
   bCheckSum += 0xA0;

   putword( wTotalLen, hObjFile );
   bCheckSum += LOBYTE( wTotalLen );
   bCheckSum += HIBYTE( wTotalLen );

   putbyte( 1, hObjFile ); /* 1 = _TEXT segment */
   bCheckSum += 1;

   putword( 0, hObjFile ); /* 0 = offset */

   for( w = 0; w < wInitLen; w++ )
   {
      putbyte( * ( initCode + w ), hObjFile );
      bCheckSum += * ( initCode + w );
   }

   for( y = 0; y < wFunctions; y++ )
   {
      for( w = 0; w < wPrgLen; w++ )
      {
         putbyte( * ( prgCode + w ), hObjFile );
         bCheckSum += * ( prgCode + w );
      }
   }

   putbyte( 256 - bCheckSum, hObjFile );
}

void Fixup( int hObjFile, BYTE bType, WORD wOffset, BYTE bFlags, BYTE bSymbol )
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

void End( int hObjFile )
{
   BYTE bChk = 0;
   BYTE bChar;

   putbyte( 0x8A, hObjFile );  bChk += 0x8A;

   putbyte( 0x02, hObjFile );   bChk += 0x02;
   putbyte( 0x00, hObjFile );

   putbyte( 0x00, hObjFile );    bChk += 0x00;

   putbyte( 256 - bChk, hObjFile );
}

