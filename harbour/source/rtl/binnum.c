/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * BIN2W(), BIN2I(), BIN2L(), BIN2U(), I2BIN(), L2BIN(), W2BIN(), U2BIN() functions
 *
 * Copyright 1999 {list of individual authors and e-mail addresses}
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

/*
 * The following parts are Copyright of the individual authors.
 * www - http://www.harbour-project.org
 *
 * Copyright 1999 Victor Szakats <info@szelvesz.hu>
 *    BIN2U()
 *    W2BIN()
 *    U2BIN()
 *
 * Copyright 2000 Chen Kedem <niki@actcom.co.il>
 *    Documentation for all functions
 *
 * See doc/license.txt for licensing terms.
 *
 */

#include "extend.h"
#include "itemapi.h"

/*  $DOC$
 *  $FUNCNAME$
 *      BIN2W()
 *  $CATEGORY$
 *      Binary conversion
 *  $ONELINER$
 *      Convert unsigned short encoded bytes into Harbour numeric
 *  $SYNTAX$
 *      BIN2W( <cBuffer> ) --> nNumber
 *  $ARGUMENTS$
 *      <cBuffer> is a character string that contain 16 bit encoded unsigned
 *      short integer (least significant byte first). The first two bytes
 *      are taken into account, the rest if any are ignored.
 *  $RETURNS$
 *      BIN2W() return numeric integer (or 0 if <cBuffer> is not a string).
 *  $DESCRIPTION$
 *      BIN2W() is one of the low level binary conversion functions, those
 *      functions convert between Harbour numeric and a character
 *      representation of numeric value. BIN2W() take two bytes of encoded
 *      16 bit unsigned short integer and convert it into standard Harbour
 *      numeric value.
 *
 *      You might ask what is the need for such functions, well, first of
 *      all it allow you to read/write information from/to a binary file
 *      (like extracting information from DBF header), it is also a useful
 *      way to share information from source other than Harbour (C for
 *      instance).
 *
 *      BIN2W() is the opposite of W2BIN()
 *  $EXAMPLES$
 *      // Show header length of a DBF
 *      FUNCTION main()
 *      LOCAL nHandle, cBuffer := space( 2 )
 *      nHandle := fopen( "test.dbf" )
 *      IF nHandle > 0
 *         fseek( nHandle, 8 )
 *         fread( nHandle, @cBuffer, 2 )
 *         ? "Length of DBF header in bytes:", BIN2W( cBuffer )
 *         fclose( nHandle )
 *      ELSE
 *         ? "Can not open file"
 *      ENDIF
 *      RETURN NIL
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      BIN2W() works exactly like CA-Clipper's BIN2W()
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      BIN2I(),BIN2L(),BIN2U(),I2BIN(),L2BIN(),W2BIN(),WORD(),U2BIN(),
 *      FREAD()
 *  $END$
 */

HARBOUR HB_BIN2W( void )
{
   PHB_ITEM pItem = hb_param( 1, IT_STRING );

   if( pItem )
   {
      char * pszString = hb_itemGetCPtr( pItem );
      ULONG ulLen = hb_itemGetCLen( pItem );
      BYTE byBuffer[ 2 ];

      byBuffer[ 0 ] = ( ulLen >= 1 ) ? ( BYTE ) pszString[ 0 ] : 0;
      byBuffer[ 1 ] = ( ulLen >= 2 ) ? ( BYTE ) pszString[ 1 ] : 0;

      hb_retni( HB_MKUSHORT( byBuffer[ 0 ],
                             byBuffer[ 1 ] ) );
   }
   else
      hb_retni( 0 );
}

/*  $DOC$
 *  $FUNCNAME$
 *      BIN2I()
 *  $CATEGORY$
 *      Binary conversion
 *  $ONELINER$
 *      Convert signed short encoded bytes into Harbour numeric
 *  $SYNTAX$
 *      BIN2I( <cBuffer> ) --> nNumber
 *  $ARGUMENTS$
 *      <cBuffer> is a character string that contain 16 bit encoded signed
 *      short integer (least significant byte first). The first two bytes
 *      are taken into account, the rest if any are ignored.
 *  $RETURNS$
 *      BIN2I() return numeric integer (or 0 if <cBuffer> is not a string).
 *  $DESCRIPTION$
 *      BIN2I() is one of the low level binary conversion functions, those
 *      functions convert between Harbour numeric and a character
 *      representation of numeric value. BIN2I() take two bytes of encoded
 *      16 bit signed short integer and convert it into standard Harbour
 *      numeric value.
 *
 *      You might ask what is the need for such functions, well, first of
 *      all it allow you to read/write information from/to a binary file
 *      (like extracting information from DBF header), it is also a useful
 *      way to share information from source other than Harbour (C for
 *      instance).
 *
 *      BIN2I() is the opposite of I2BIN()
 *  $EXAMPLES$
 *      // Show DBF last update date
 *      FUNCTION main()
 *      LOCAL nHandle, cYear, cMonth, cDay
 *      nHandle := fopen( "test.dbf" )
 *      IF nHandle > 0
 *         fseek( nHandle, 1 )
 *         cYear := cMonth := cDay := " "
 *         fread( nHandle, @cYear , 1 )
 *         fread( nHandle, @cMonth, 1 )
 *         fread( nHandle, @cDay  , 1 )
 *         ? "Last update:", BIN2I( cYear ), BIN2I( cMonth ), BIN2I( cDay )
 *         fclose( nHandle )
 *      ELSE
 *         ? "Can not open file"
 *      ENDIF
 *      RETURN NIL
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      BIN2I() works exactly like CA-Clipper's BIN2I()
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      BIN2L(),BIN2U(),BIN2W(),I2BIN(),L2BIN(),W2BIN(),WORD(),U2BIN(),
 *      FREAD()
 *  $END$
 */

HARBOUR HB_BIN2I( void )
{
   PHB_ITEM pItem = hb_param( 1, IT_STRING );

   if( pItem )
   {
      char * pszString = hb_itemGetCPtr( pItem );
      ULONG ulLen = hb_itemGetCLen( pItem );
      BYTE byBuffer[ 2 ];

      byBuffer[ 0 ] = ( ulLen >= 1 ) ? ( BYTE ) pszString[ 0 ] : 0;
      byBuffer[ 1 ] = ( ulLen >= 2 ) ? ( BYTE ) pszString[ 1 ] : 0;

      hb_retni( HB_MKSHORT( byBuffer[ 0 ],
                            byBuffer[ 1 ] ) );
   }
   else
      hb_retni( 0 );
}

/*  $DOC$
 *  $FUNCNAME$
 *      BIN2L()
 *  $CATEGORY$
 *      Binary conversion
 *  $ONELINER$
 *      Convert signed long encoded bytes into Harbour numeric
 *  $SYNTAX$
 *      BIN2L( <cBuffer> ) --> nNumber
 *  $ARGUMENTS$
 *      <cBuffer> is a character string that contain 32 bit encoded signed
 *      long integer (least significant byte first). The first four bytes
 *      are taken into account, the rest if any are ignored.
 *  $RETURNS$
 *      BIN2L() return numeric integer (or 0 if <cBuffer> is not a string).
 *  $DESCRIPTION$
 *      BIN2L() is one of the low level binary conversion functions, those
 *      functions convert between Harbour numeric and a character
 *      representation of numeric value. BIN2L() take four bytes of encoded
 *      32 bit signed long integer and convert it into standard Harbour
 *      numeric value.
 *
 *      You might ask what is the need for such functions, well, first of
 *      all it allow you to read/write information from/to a binary file
 *      (like extracting information from DBF header), it is also a useful
 *      way to share information from source other than Harbour (C for
 *      instance).
 *
 *      BIN2L() is the opposite of L2BIN()
 *  $EXAMPLES$
 *      // Show number of records in DBF
 *      FUNCTION main()
 *      LOCAL nHandle, cBuffer := space( 4 )
 *      nHandle := fopen( "test.dbf" )
 *      IF nHandle > 0
 *         fseek( nHandle, 4 )
 *         fread( nHandle, @cBuffer, 4 )
 *         ? "Number of records in file:", BIN2L( cBuffer )
 *         fclose( nHandle )
 *      ELSE
 *         ? "Can not open file"
 *      ENDIF
 *      RETURN NIL
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      BIN2L() works exactly like CA-Clipper's BIN2L()
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      BIN2I(),BIN2U(),BIN2W(),I2BIN(),L2BIN(),W2BIN(),WORD(),U2BIN(),
 *      FREAD()
 *  $END$
 */

HARBOUR HB_BIN2L( void )
{
   PHB_ITEM pItem = hb_param( 1, IT_STRING );

   if( pItem )
   {
      char * pszString = hb_itemGetCPtr( pItem );
      ULONG ulLen = hb_itemGetCLen( pItem );
      BYTE byBuffer[ 4 ];

      byBuffer[ 0 ] = ( ulLen >= 1 ) ? ( BYTE ) pszString[ 0 ] : 0;
      byBuffer[ 1 ] = ( ulLen >= 2 ) ? ( BYTE ) pszString[ 1 ] : 0;
      byBuffer[ 2 ] = ( ulLen >= 3 ) ? ( BYTE ) pszString[ 2 ] : 0;
      byBuffer[ 3 ] = ( ulLen >= 4 ) ? ( BYTE ) pszString[ 3 ] : 0;

      hb_retnl( HB_MKLONG( byBuffer[ 0 ],
                           byBuffer[ 1 ],
                           byBuffer[ 2 ],
                           byBuffer[ 3 ] ) );
   }
   else
      hb_retnl( 0 );
}

/*  $DOC$
 *  $FUNCNAME$
 *      BIN2U()
 *  $CATEGORY$
 *      Binary conversion
 *  $ONELINER$
 *      Convert unsigned long encoded bytes into Harbour numeric
 *  $SYNTAX$
 *      BIN2U( <cBuffer> ) --> nNumber
 *  $ARGUMENTS$
 *      <cBuffer> is a character string that contain 32 bit encoded unsigned
 *      long integer (least significant byte first). The first four bytes
 *      are taken into account, the rest if any are ignored.
 *  $RETURNS$
 *      BIN2U() return numeric integer (or 0 if <cBuffer> is not a string).
 *  $DESCRIPTION$
 *      BIN2U() is one of the low level binary conversion functions, those
 *      functions convert between Harbour numeric and a character
 *      representation of numeric value. BIN2U() take four bytes of encoded
 *      32 bit unsigned long integer and convert it into standard Harbour
 *      numeric value.
 *
 *      You might ask what is the need for such functions, well, first of
 *      all it allow you to read/write information from/to a binary file
 *      (like extracting information from DBF header), it is also a useful
 *      way to share information from source other than Harbour (C for
 *      instance).
 *
 *      BIN2U() is the opposite of U2BIN()
 *  $EXAMPLES$
 *      // Show number of records in DBF
 *      FUNCTION main()
 *      LOCAL nHandle, cBuffer := space( 4 )
 *      nHandle := fopen( "test.dbf" )
 *      IF nHandle > 0
 *         fseek( nHandle, 4 )
 *         fread( nHandle, @cBuffer, 4 )
 *         ? "Number of records in file:", BIN2U( cBuffer )
 *         fclose( nHandle )
 *      ELSE
 *         ? "Can not open file"
 *      ENDIF
 *      RETURN NIL
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      BIN2U() is an XBase++ compatibility function and does not exist
 *      as a standard CA-Clipper 5.x function.
 *
 *      This function is only visible if source/rtl/binnum.c was compiled
 *      with the HB_COMPAT_XPP flag.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      BIN2I(),BIN2L(),BIN2W(),I2BIN(),L2BIN(),W2BIN(),WORD(),U2BIN(),
 *      FREAD()
 *  $END$
 */

#ifdef HB_COMPAT_XPP

/* NOTE: XBase++ compatible function */

HARBOUR HB_BIN2U( void )
{
   PHB_ITEM pItem = hb_param( 1, IT_STRING );

   if( pItem )
   {
      char * pszString = hb_itemGetCPtr( pItem );
      ULONG ulLen = hb_itemGetCLen( pItem );
      BYTE byBuffer[ 4 ];

      byBuffer[ 0 ] = ( ulLen >= 1 ) ? ( BYTE ) pszString[ 0 ] : 0;
      byBuffer[ 1 ] = ( ulLen >= 2 ) ? ( BYTE ) pszString[ 1 ] : 0;
      byBuffer[ 2 ] = ( ulLen >= 3 ) ? ( BYTE ) pszString[ 2 ] : 0;
      byBuffer[ 3 ] = ( ulLen >= 4 ) ? ( BYTE ) pszString[ 3 ] : 0;

      hb_retnl( HB_MKULONG( byBuffer[ 0 ],
                            byBuffer[ 1 ],
                            byBuffer[ 2 ],
                            byBuffer[ 3 ] ) );
   }
   else
      hb_retnl( 0 );
}

#endif

/*  $DOC$
 *  $FUNCNAME$
 *      I2BIN()
 *  $CATEGORY$
 *      Binary conversion
 *  $ONELINER$
 *      Convert Harbour numeric into signed short encoded bytes
 *  $SYNTAX$
 *      I2BIN( <nNumber> ) --> cBuffer
 *  $ARGUMENTS$
 *      <nNumber> is a numeric value to convert (decimal digits are
 *      ignored).
 *  $RETURNS$
 *      I2BIN() return two bytes character string that contain 16 bit
 *      encoded signed short integer (least significant byte first).
 *  $DESCRIPTION$
 *      I2BIN() is one of the low level binary conversion functions, those
 *      functions convert between Harbour numeric and a character
 *      representation of numeric value. I2BIN() take a numeric integer
 *      value and convert it into two bytes of encoded 16 bit signed short
 *      integer.
 *
 *      You might ask what is the need for such functions, well, first of
 *      all it allow you to read/write information from/to a binary file
 *      (like extracting information from DBF header), it is also a useful
 *      way to share information from source other than Harbour (C for
 *      instance).
 *
 *      I2BIN() is the opposite of BIN2I()
 *  $EXAMPLES$
 *      // Update DBF "last update" date
 *      #include "fileio.ch"
 *      FUNCTION main()
 *      LOCAL nHandle, cYear, cMonth, cDay
 *      use test
 *      ? "Original update date is:", lupdate()
 *      close
 *      nHandle := fopen( "test.dbf", FO_READWRITE )
 *      IF nHandle > 0
 *         fseek( nHandle, 1, )
 *         cYear  := I2BIN( 68 )
 *         cMonth := I2BIN(  8 )
 *         cDay   := I2BIN(  1 )
 *         fwrite( nHandle, cYear , 1 )   // write only the first byte
 *         fwrite( nHandle, cMonth, 1 )
 *         fwrite( nHandle, cDay  , 1 )
 *         fclose( nHandle )
 *         use test
 *         ? "New update date is:", lupdate()
 *         close
 *      ELSE
 *         ? "Can not open file"
 *      ENDIF
 *      RETURN NIL
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      I2BIN() works exactly like CA-Clipper's I2BIN()
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      BIN2I,BIN2L(),BIN2U(),BIN2W(),L2BIN(),W2BIN(),WORD(),U2BIN(),
 *      FWRITE()
 *  $END$
 */

HARBOUR HB_I2BIN( void )
{
   char szString[ 2 ];

   if( ISNUM( 1 ) )
   {
      SHORT iValue = hb_parni( 1 );

      szString[ 0 ] =   iValue & 0x00FF;
      szString[ 1 ] = ( iValue & 0xFF00 ) >> 8;
   }
   else
   {
      szString[ 0 ] =
      szString[ 1 ] = '\0';
   }

   hb_retclen( szString, 2 );
}

/*  $DOC$
 *  $FUNCNAME$
 *      W2BIN()
 *  $CATEGORY$
 *      Binary conversion
 *  $ONELINER$
 *      Convert Harbour numeric into unsigned short encoded bytes
 *  $SYNTAX$
 *      W2BIN( <nNumber> ) --> cBuffer
 *  $ARGUMENTS$
 *      <nNumber> is a numeric value to convert (decimal digits are
 *      ignored).
 *  $RETURNS$
 *      W2BIN() return two bytes character string that contain 16 bit
 *      encoded unsigned short integer (least significant byte first).
 *  $DESCRIPTION$
 *      W2BIN() is one of the low level binary conversion functions, those
 *      functions convert between Harbour numeric and a character
 *      representation of numeric value. W2BIN() take a numeric integer
 *      value and convert it into two bytes of encoded 16 bit unsigned short
 *      integer.
 *
 *      You might ask what is the need for such functions, well, first of
 *      all it allow you to read/write information from/to a binary file
 *      (like extracting information from DBF header), it is also a useful
 *      way to share information from source other than Harbour (C for
 *      instance).
 *
 *      W2BIN() is the opposite of BIN2W()
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      W2BIN() is an XBase++ compatibility function and does not exist
 *      as a standard CA-Clipper 5.x function.
 *
 *      This function is only visible if source/rtl/binnum.c was compiled
 *      with the HB_COMPAT_XPP flag.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      BIN2I,BIN2L(),BIN2U(),BIN2W(),I2BIN(),L2BIN(),WORD(),U2BIN(),
 *      FWRITE()
 *  $END$
 */

#ifdef HB_COMPAT_XPP

/* NOTE: XBase++ compatible function */

HARBOUR HB_W2BIN( void )
{
   char szString[ 2 ];

   if( ISNUM( 1 ) )
   {
      USHORT uiValue = ( USHORT ) hb_parni( 1 );

      szString[ 0 ] =   uiValue & 0x00FF;
      szString[ 1 ] = ( uiValue & 0xFF00 ) >> 8;
   }
   else
   {
      szString[ 0 ] =
      szString[ 1 ] = '\0';
   }

   hb_retclen( szString, 2 );
}

#endif

/*  $DOC$
 *  $FUNCNAME$
 *      L2BIN()
 *  $CATEGORY$
 *      Binary conversion
 *  $ONELINER$
 *      Convert Harbour numeric into signed long encoded bytes
 *  $SYNTAX$
 *      L2BIN( <nNumber> ) --> cBuffer
 *  $ARGUMENTS$
 *      <nNumber> is a numeric value to convert (decimal digits are
 *      ignored).
 *  $RETURNS$
 *      L2BIN() return four bytes character string that contain 32 bit
 *      encoded signed long integer (least significant byte first).
 *  $DESCRIPTION$
 *      L2BIN() is one of the low level binary conversion functions, those
 *      functions convert between Harbour numeric and a character
 *      representation of numeric value. L2BIN() take a numeric integer
 *      value and convert it into four bytes of encoded 32 bit signed long
 *      integer.
 *
 *      You might ask what is the need for such functions, well, first of
 *      all it allow you to read/write information from/to a binary file
 *      (like extracting information from DBF header), it is also a useful
 *      way to share information from source other than Harbour (C for
 *      instance).
 *
 *      L2BIN() is the opposite of BIN2L()
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      L2BIN() works exactly like CA-Clipper's L2BIN()
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      BIN2I,BIN2L(),BIN2U(),BIN2W(),I2BIN(),W2BIN(),WORD(),U2BIN(),
 *      FWRITE()
 *  $END$
 */

HARBOUR HB_L2BIN( void )
{
   char szString[ 4 ];

   if( ISNUM( 1 ) )
   {
      LONG lValue = hb_parnl( 1 );

      szString[ 0 ] =   lValue & 0x000000FF;
      szString[ 1 ] = ( lValue & 0x0000FF00 ) >> 8;
      szString[ 2 ] = ( lValue & 0x00FF0000 ) >> 16;
      szString[ 3 ] = ( lValue & 0xFF000000 ) >> 24;
   }
   else
   {
      szString[ 0 ] =
      szString[ 1 ] =
      szString[ 2 ] =
      szString[ 3 ] = '\0';
   }

   hb_retclen( szString, 4 );
}

/*  $DOC$
 *  $FUNCNAME$
 *      U2BIN()
 *  $CATEGORY$
 *      Binary conversion
 *  $ONELINER$
 *      Convert Harbour numeric into unsigned long encoded bytes
 *  $SYNTAX$
 *      U2BIN( <nNumber> ) --> cBuffer
 *  $ARGUMENTS$
 *      <nNumber> is a numeric value to convert (decimal digits are
 *      ignored).
 *  $RETURNS$
 *      U2BIN() return four bytes character string that contain 32 bit
 *      encoded unsigned long integer (least significant byte first).
 *  $DESCRIPTION$
 *      U2BIN() is one of the low level binary conversion functions, those
 *      functions convert between Harbour numeric and a character
 *      representation of numeric value. U2BIN() take a numeric integer
 *      value and convert it into four bytes of encoded 32 bit unsigned long
 *      integer.
 *
 *      You might ask what is the need for such functions, well, first of
 *      all it allow you to read/write information from/to a binary file
 *      (like extracting information from DBF header), it is also a useful
 *      way to share information from source other than Harbour (C for
 *      instance).
 *
 *      U2BIN() is the opposite of BIN2U()
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      R
 *  $COMPLIANCE$
 *      U2BIN() is an XBase++ compatibility function and does not exist
 *      as a standard CA-Clipper 5.x function.
 *
 *      This function is only visible if source/rtl/binnum.c was compiled
 *      with the HB_COMPAT_XPP flag.
 *  $PLATFORMS$
 *  $FILES$
 *  $SEEALSO$
 *      BIN2I,BIN2L(),BIN2U(),BIN2W(),I2BIN(),L2BIN(),W2BIN(),WORD()
 *      FWRITE()
 *  $END$
 */

#ifdef HB_COMPAT_XPP

/* NOTE: XBase++ compatible function */

HARBOUR HB_U2BIN( void )
{
   char szString[ 4 ];

   if( ISNUM( 1 ) )
   {
      ULONG ulValue = ( ULONG ) hb_parnl( 1 );

      szString[ 0 ] =   ulValue & 0x000000FF;
      szString[ 1 ] = ( ulValue & 0x0000FF00 ) >> 8;
      szString[ 2 ] = ( ulValue & 0x00FF0000 ) >> 16;
      szString[ 3 ] = ( ulValue & 0xFF000000 ) >> 24;
   }
   else
   {
      szString[ 0 ] =
      szString[ 1 ] =
      szString[ 2 ] =
      szString[ 3 ] = '\0';
   }

   hb_retclen( szString, 4 );
}

#endif

