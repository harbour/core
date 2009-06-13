/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *   CT3 video functions:
 *
 * SCREENATTR(), SCREENMIX(), SAYSCREEN(),
 * CLEARWIN(), INVERTWIN(), UNTEXTWIN(), CHARWIN(), COLORWIN(), COLORREPL()
 *
 *   and Harbour extension:
 *
 * SCREENTEXT()
 *
 * Copyright 2007 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 *
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

#include "hbdefs.h"
#include "hbapi.h"
#include "hbapigt.h"

/*  $DOC$
 *  $FUNCNAME$
 *      SCREENATTR()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *  $SYNTAX$
 *      SCREENATTR ( [<nRow>],[<nColumn>] ) -> <nAttr>
 *  $ARGUMENTS$
 *   <nRow>     Designates the line from which to determine the attribute.
 *              The default is the cursor line.
 *
 *   <nColumn>  Designates the column from which to determine the
 *              attribute.  The default is the cursor column.
 *
 *  $RETURNS$
 *      SCREENATTR() returns the attribute at the designated position.
 *
 *  $DESCRIPTION$
 *      SCREENATTR() returns the current screen attribute at <nRow> and
 *      <nColumn>.  You can query targeted attributes this way and save them
 *      to use later, or process them later with INVERTATTR().
 *
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is screen1.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( SCREENATTR )
{
   SHORT sRow, sCol;
   SHORT iRow, iCol;
   BYTE bColor, bAttr;
   USHORT usChar;

   hb_gtGetPos( &sRow, &sCol );
   iRow = HB_ISNUM( 1 ) ? ( SHORT ) hb_parni( 1 ) : sRow;
   iCol = HB_ISNUM( 2 ) ? ( SHORT ) hb_parni( 2 ) : sCol;

   if( hb_gtGetChar( iRow, iCol, &bColor, &bAttr, &usChar ) != HB_SUCCESS )
      bColor = 0;

   hb_retni( ( int ) bColor );
}


/*  $DOC$
 *  $FUNCNAME$
 *      SCREENMIX()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *  $SYNTAX$
 *      SCREENMIX (<cCharString>, <cAttributeString>, [<nRow>], [<nCol>]) -> <cEmptyString>
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is screen1.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( SCREENMIX )
{
   ULONG ulLen = hb_parclen( 1 );

   if( ulLen )
   {
      char * szText = hb_parc( 1 );
      const char * szAttr;
      ULONG ulAttr = hb_parclen( 2 ), ul = 0;
      SHORT sRow, sCol;
      SHORT iRow, iCol, i;

      if( ulAttr == 0 )
      {
         szAttr = " ";
         ulAttr = 1;
      }
      else
         szAttr = hb_parc( 2 );

      hb_gtGetPos( &sRow, &sCol );
      iRow = HB_ISNUM( 3 ) ? ( SHORT ) hb_parni( 3 ) : sRow;
      iCol = HB_ISNUM( 4 ) ? ( SHORT ) hb_parni( 4 ) : sCol;

      if( iRow >= 0 && iCol >= 0 &&
          iRow <= hb_gtMaxRow() && iCol <= hb_gtMaxCol() )
      {
         hb_gtBeginWrite();
         i = iCol;
         do
         {
            if( hb_gtPutChar( iRow, i++, szAttr[ ul ], 0, *szText++ ) != HB_SUCCESS )
            {
               if( ++iRow > hb_gtMaxRow() )
                  break;
               --szText;
               ++ulLen;
               i = iCol;
            }
            else if( ++ul == ulAttr )
               ul = 0;
         }
         while( --ulLen );
         hb_gtEndWrite();
      }
   }

   hb_retc( NULL );
}


/*  $DOC$
 *  $FUNCNAME$
 *      SAYSCREEN()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *  $SYNTAX$
 *      SAYSCREEN( <cString>, [<nRow>], [<nCol>] ) -> <cEmptyString>
 *  $ARGUMENTS$
 *      <cString> - the string to output. Although undocumented, can be NIL.
 *      <nRow> - row number, defaults to cursor row.
 *      <nCol> - column number, defaults to cursor column.
 *  $RETURNS$
 *      Returns an empty string.
 *  $DESCRIPTION$
 *      Outputs a string at specified coordinates without changing character
 *      attributes.
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Ready
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is screen1.c, library is libct.
 *  $SEEALSO$
 *      SCREENMIX()
 *  $END$
 */

HB_FUNC( SAYSCREEN )
{
   ULONG ulLen = hb_parclen( 1 );

   if( ulLen )
   {
      char * szText = hb_parc( 1 );
      SHORT sRow, sCol;
      SHORT iRow, iCol, i;

      hb_gtGetPos( &sRow, &sCol );
      iRow = HB_ISNUM( 2 ) ? ( SHORT ) hb_parni( 2 ) : sRow;
      iCol = HB_ISNUM( 3 ) ? ( SHORT ) hb_parni( 3 ) : sCol;

      if( iRow >= 0 && iCol >= 0 &&
          iRow <= hb_gtMaxRow() && iCol <= hb_gtMaxCol() )
      {
         hb_gtBeginWrite();
         i = iCol;
         do
         {
            BYTE bColor, bAttr;
            USHORT usChar;
            if( hb_gtGetChar( iRow, i, &bColor, &bAttr, &usChar ) != HB_SUCCESS )
            {
               if( ++iRow > hb_gtMaxRow() )
                  break;
               ++ulLen;
               i = iCol;
            }
            else
               hb_gtPutChar( iRow, i++, bColor, bAttr, *szText++ );
         }
         while( --ulLen );
         hb_gtEndWrite();
      }
   }

   hb_retc( NULL );
}

static BOOL hb_ctGetWinCord( int * piTop, int * piLeft,
                             int * piBottom, int * piRight )
{
   int iMaxRow = hb_gtMaxRow();
   int iMaxCol = hb_gtMaxCol();

   hb_gtGetPosEx( piTop, piLeft );

   if( HB_ISNUM( 1 ) )
      *piTop = hb_parni( 1 );
   if( HB_ISNUM( 2 ) )
      *piLeft   = hb_parni( 2 );
   if( HB_ISNUM( 3 ) )
   {
      *piBottom = hb_parni( 3 );
      if( *piBottom > iMaxRow )
         *piBottom = iMaxRow;
   }
   else
      *piBottom = iMaxRow;
   if( HB_ISNUM( 4 ) )
   {
      *piRight = hb_parni( 4 );
      if( *piRight > iMaxCol )
         *piRight = iMaxCol;
   }
   else
      *piRight = iMaxCol;

   return *piTop >= 0 && *piLeft >= 0 &&
          *piTop <= *piBottom && *piLeft <= *piRight;
}

static int hb_ctGetClearChar( int iParam )
{
   int iChar;

   if( HB_ISNUM( iParam ) )
      iChar = hb_parni( iParam );
   else if( HB_ISCHAR( iParam ) )
      iChar = ( UCHAR ) hb_parc( iParam )[0];
   else
      iChar = hb_gtGetClearChar();

   return iChar;
}

static int hb_ctGetClearColor( int iParam )
{
   int iColor;

   if( HB_ISNUM( iParam ) )
      iColor = hb_parni( iParam );
   else if( HB_ISCHAR( iParam ) )
   {
      iColor = hb_gtColorToN( hb_parc( iParam ) );
      if( iColor == -1 )
         iColor = 0;
   }
   else
      iColor = hb_gtGetClearColor();

   return iColor;
}

HB_FUNC( CLEARWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      BYTE bColor, bChar;

      bColor = ( BYTE ) hb_ctGetClearColor( 5 );
      bChar  = ( BYTE ) hb_ctGetClearChar( 6 );

      hb_gtScrollEx( iTop, iLeft, iBottom, iRight, bColor, bChar, 0, 0 );
   }

   hb_retc( NULL );
}


/*  $DOC$
 *  $FUNCNAME$
 *      INVERTWIN()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *
 *  $SYNTAX$
 *
 *  $ARGUMENTS$
 *  $RETURNS$
 *  $DESCRIPTION$
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *      INVERTWIN() is compatible with CT3's INVERTWIN().
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is color.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( INVERTWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      hb_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            BYTE bColor, bAttr;
            USHORT usChar;

            hb_gtGetChar( iTop, iCol, &bColor, &bAttr, &usChar );
            bColor = ( bColor & 0x88 ) |
                     ( ( bColor & 0x07 ) << 4 ) |
                     ( ( bColor >> 4 ) & 0x07 );
            hb_gtPutChar( iTop, iCol, bColor, bAttr, usChar );
            ++iCol;
         }
         ++iTop;
      }
      hb_gtEndWrite();
   }

   hb_retc( NULL );
}


/*  $DOC$
 *  $FUNCNAME$
 *      UNTEXTWIN()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *  $SYNTAX$
 *      UNTEXTWIN(<nTopLine>, <nLeftColumn>, <nBottomLine>, <nRightColumn>,
 *                <cReplacementCharacter|nReplacementCharacter>,
 *                [<cInitialCharacter|nInitialCharacter>],
 *                [<cEndCharacter|nEndCharacter>]) --> cNull
 *  $ARGUMENTS$
 *   <nTopLine>  Designates the line for the upper-left corner of the
 *     area.
 *   <nLeftColumn>  Designates the column for the upper-left corner of
 *     the area.
 *   <nBottomLine>  Designates the line for the bottom-right corner of
 *     the area.
 *   <nRightColumn>  Designates the line for the bottom-right column of
 *     the area.
 *   <cReplacementCharacter|nReplacementCharacter>  Replaces each
 *     character within the window, with the exception of those within the
 *     range of <cInitialCharacter|nInitialCharacter> and
 *   <cEndCharacter|nEndCharacter>.
 *   <cInitialCharacter|nInitialCharacter>  Designates the beginning of
 *     the bracketed area.  The character can be number in the range of 0 to
 *     255, or the character string type.  The default value is 176.
 *   <cEndCharacter|nEndCharacter>  Designates the end of the bracketed
 *     area.  The character can be number in the range of 0 to 255 or the
 *     character string type.  The default value is 223.
 *  $RETURNS$
 *      Returns a null string.
 *  $DESCRIPTION$
 *      Replaces an area of characters from a region of the screen
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is screen1.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( UNTEXTWIN )
{
   int iTop, iLeft, iBottom, iRight;
   UCHAR ucRepl, ucInit, ucEnd;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      ucRepl = ( UCHAR ) hb_ctGetClearChar( 5 );

      if( HB_ISNUM( 6 ) )
         ucInit = ( UCHAR ) hb_parni( 6 );
      else if( hb_parclen( 6 ) > 0 )
         ucInit = ( UCHAR ) hb_parc( 6 )[0];
      else
         ucInit = 176;

      if( HB_ISNUM( 7 ) )
         ucEnd = ( UCHAR ) hb_parni( 7 );
      else if( hb_parclen( 7 ) > 0 )
         ucEnd = ( UCHAR ) hb_parc( 7 )[0];
      else
         ucEnd = 223;

      hb_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            BYTE bColor, bAttr;
            USHORT usChar;

            hb_gtGetChar( iTop, iCol, &bColor, &bAttr, &usChar );
            if( ucInit <= ucEnd ? ( usChar < ucInit || usChar > ucEnd ) :
                                  ( usChar > ucEnd && usChar < ucInit ) )
               hb_gtPutChar( iTop, iCol, bColor, bAttr, ucRepl );
            ++iCol;
         }
         ++iTop;
      }
      hb_gtEndWrite();
   }

   hb_retc( NULL );
}

/*  $DOC$
 *  $FUNCNAME$
 *      CHARWIN()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *  $SYNTAX$
 *      CHARWIN (<nTop>, <nLeft>, <nBottom>, <nRight>, [<cNewChar|nNewChar>],
 *               [<cOldChar|nOldChar>]) --> <cEmptyString>
 *  $ARGUMENTS$
 *    <nTop> - top row number, default 0
 *    <nLeft> - left column number, default 0
 *    <nBottom> - top row number, default MaxRow()
 *    <nRight> - right column number, default MaxCol()
 *    <cNewChar|nNewChar> - new character for the screen area,
 *          as a numeric value in the range of 0 to
 *          255 or as a character string, default value is the CLEARB.
 *    <cOldChar|nOldChar> - character to exchange. Specify the parameter
 *          as a numeric in the range of 0 to 255
 *          or as a character string.  The default is to exchange all characters.
 *  $RETURNS$
 *      Returns an empty string.
 *  $DESCRIPTION$
 *      Exchanges particular characters in a screen area.
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is screen1.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( CHARWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      UCHAR ucNewChar, ucOldChar = 0;
      BOOL fAll = FALSE;

      ucNewChar = ( UCHAR ) hb_ctGetClearChar( 5 );

      if( HB_ISNUM( 6 ) )
         ucOldChar = ( UCHAR ) hb_parni( 6 );
      else if( hb_parclen( 6 ) > 0 )
         ucOldChar = ( UCHAR ) hb_parc( 6 )[0];
      else
         fAll = TRUE;

      hb_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            BYTE bColor, bAttr;
            USHORT usChar;

            hb_gtGetChar( iTop, iCol, &bColor, &bAttr, &usChar );
            if( fAll || usChar == ucOldChar )
               hb_gtPutChar( iTop, iCol, bColor, bAttr, ucNewChar );
            ++iCol;
         }
         ++iTop;
      }
      hb_gtEndWrite();
   }

   hb_retc( NULL );
}


/*  $DOC$
 *  $FUNCNAME$
 *      COLORWIN()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *  $SYNTAX$
 *      COLORWIN([<nTopLine>], [<nLeftCol>], [<nBottomLine>], [<nRightCol>],
 *               [<cNewAttr|nNewAttr>], [<cOldAttr|nOldAttr>]) --> cNull
 *  $ARGUMENTS$
 *   <nTopLine>  Designates the topmost line to begin processing.  The
 *     default is the cursor line.
 *   <nLeftCol>  Designates the leftmost column to begin processing.  The
 *     default is the cursor column.
 *   <nBottomLine>  Designates the bottommost line that is processed.
 *     The default is the last screen line or window line.
 *   <nRightCol>  Designates the rightmost column to clear.  The default
 *     is the right screen border or window border.
 *   <cNewAttr|nNewAttr>  Designates the new attribute to replace the old
 *     one.  The default is the standard attribute CLEARA.
 *   <cOldAttr|nOldAttr>  Designates the old character to exchange.  The
 *     default is "exchange all attributes".
 *  $RETURNS$
 *      Returns an empty string.
 *  $DESCRIPTION$
 *      Exchanges particular attributes in a screen area
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is screen1.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( COLORWIN )
{
   int iTop, iLeft, iBottom, iRight;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      UCHAR ucNewColor, ucOldColor = 0;
      BOOL fAll = FALSE;

      ucNewColor = ( UCHAR ) hb_ctGetClearColor( 5 );

      if( HB_ISNUM( 6 ) || HB_ISCHAR( 6 ) )
         ucOldColor = ( UCHAR ) hb_ctGetClearColor( 6 );
      else
         fAll = TRUE;

      hb_gtBeginWrite();
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            BYTE bColor, bAttr;
            USHORT usChar;

            hb_gtGetChar( iTop, iCol, &bColor, &bAttr, &usChar );
            if( fAll || bColor == ucOldColor )
               hb_gtPutChar( iTop, iCol, ucNewColor, bAttr, usChar );
            ++iCol;
         }
         ++iTop;
      }
      hb_gtEndWrite();
   }

   hb_retc( NULL );
}


/*  $DOC$
 *  $FUNCNAME$
 *      SCREENTEXT()
 *  $CATEGORY$
 *      CT video functions (Harbour extension)
 *  $ONELINER$
 *  $SYNTAX$
 *      SCREENTEXT(<nTop>, <nLeft>, <nBottom>, <nRight>)
 *  $ARGUMENTS$
 *    <nTop> - top row number, default 0
 *    <nLeft> - left column number, default 0
 *    <nBottom> - top row number, default MaxRow()
 *    <nRight> - right column number, default MaxCol()
 *  $RETURNS$
 *      Returns string with characters taken from given screen region.
 *  $DESCRIPTION$
 *      Returns string with characters taken from given screen region.
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is screen1.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( SCREENTEXT )
{
   int iTop, iLeft, iBottom, iRight;
   char * pBuffer, * szText;
   ULONG ulSize;

   if( hb_ctGetWinCord( &iTop, &iLeft, &iBottom, &iRight ) )
   {
      ulSize = ( ULONG ) ( iBottom - iTop + 1 ) * ( iRight - iLeft + 1 );
      szText = pBuffer = ( char * ) hb_xgrab( ulSize + 1 );
      while( iTop <= iBottom )
      {
         int iCol = iLeft;
         while( iCol <= iRight )
         {
            BYTE bColor, bAttr;
            USHORT usChar;
            hb_gtGetChar( iTop, iCol, &bColor, &bAttr, &usChar );
            *szText++ = ( char ) usChar;
            ++iCol;
         }
         ++iTop;
      }
      hb_retclen_buffer( pBuffer, ulSize );
   }
   else
      hb_retc( NULL );
}

/*  $DOC$
 *  $FUNCNAME$
 *      COLORREPL()
 *  $CATEGORY$
 *      CT3 video functions
 *  $ONELINER$
 *  $SYNTAX$
 *      COLORREPL([<cNewAttr|nNewAttr>], [<cOldAttr|nOldAttr>]) --> cNull
 *  $ARGUMENTS$
 *   <cNewAttr|nNewAttr>  Designates the new attribute.  The default is
 *     CLEARA.
 *   <cOldAttr|InOldAttr>  Designates the old attribute to exchange.  The
 *     default is all existing attributes.
 *  $RETURNS$
 *      Returns an empty string.
 *  $DESCRIPTION$
 *      Exchanges particular screen attributes
 *      TODO: add documentation
 *  $EXAMPLES$
 *  $TESTS$
 *  $STATUS$
 *      Started
 *  $COMPLIANCE$
 *  $PLATFORMS$
 *      All
 *  $FILES$
 *      Source is screen1.c, library is libct.
 *  $SEEALSO$
 *  $END$
 */

HB_FUNC( COLORREPL )
{
   int iMaxRow = hb_gtMaxRow();
   int iMaxCol = hb_gtMaxCol();
   int iRow = 0, iCol;
   UCHAR ucNewColor, ucOldColor = 0;
   BOOL fAll = FALSE;

   ucNewColor = ( UCHAR ) hb_ctGetClearColor( 1 );

   if( HB_ISNUM( 2 ) || HB_ISCHAR( 2 ) )
      ucOldColor = ( UCHAR ) hb_ctGetClearColor( 2 );
   else
      fAll = TRUE;

   hb_gtBeginWrite();
   while( iRow <= iMaxRow )
   {
      iCol = 0;
      while( iCol <= iMaxCol )
      {
         BYTE bColor, bAttr;
         USHORT usChar;

         hb_gtGetChar( iRow, iCol, &bColor, &bAttr, &usChar );
         if( fAll || bColor == ucOldColor )
            hb_gtPutChar( iRow, iCol, ucNewColor, bAttr, usChar );
         ++iCol;
      }
      ++iRow;
   }
   hb_gtEndWrite();

   hb_retc( NULL );
}
