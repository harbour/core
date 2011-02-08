/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * TBrowse Class
 *
 * Copyright 2008 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * This implementation contains code and notes by:
 * Copyright 2008 Viktor Szakats (harbour.01 syenar.hu)
 * www - http://harbour-project.org
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

#define HB_CLS_NOTOBJECT

#include "hbclass.ch"

#include "button.ch"
#include "color.ch"
#include "common.ch"
#include "error.ch"
#include "inkey.ch"
#include "setcurs.ch"
#include "tbrowse.ch"

/* HB_BRW_STATICMOUSE controls if mouse position is static
 * and set by call to hitTest() method or dynamic calculated
 * by calls to MPOS() / MCOL(). CL53 uses dynamic mouse but
 * I guess that some Harbour GUI libraries inherit from TBROWSE
 * and because they do not support MCOL()/MROW() (when someone
 * will create GUI library integrated with GT system?) then they
 * need static mouse with positions passed by GUI code. [druzus]
 */
/* #define HB_BRW_STATICMOUSE */


#define _TBCI_COLOBJECT       1   // column object
#define _TBCI_COLWIDTH        2   // width of the column
#define _TBCI_COLPOS          3   // column position on screen
#define _TBCI_CELLWIDTH       4   // width of the cell
#define _TBCI_CELLPOS         5   // cell position in column
#define _TBCI_COLSEP          6   // column separator
#define _TBCI_SEPWIDTH        7   // width of the separator
#define _TBCI_HEADING         8   // column heading
#define _TBCI_FOOTING         9   // column footing
#define _TBCI_HEADSEP        10   // heading separator
#define _TBCI_FOOTSEP        11   // footing separator
#define _TBCI_DEFCOLOR       12   // default color
#define _TBCI_FROZENSPACE    13   // space after frozen columns
#define _TBCI_LASTSPACE      14   // space after last visible column
#define _TBCI_SIZE           14   // size of array with TBrowse column data

#define _TBC_SETKEY_KEY       1
#define _TBC_SETKEY_BLOCK     2

#define _TBC_CLR_STANDARD     1
#define _TBC_CLR_SELECTED     2
#define _TBC_CLR_HEADING      3
#define _TBC_CLR_FOOTING      4
#define _TBC_CLR_MAX          4

#define _TBR_CONF_COLORS      1
#define _TBR_CONF_COLUMNS     2
#define _TBR_CONF_ALL         3

/* Footing/heading line separator. */
#define _TBR_CHR_LINEDELIMITER ";"

#define _TBR_COORD( n )       Int( n )

CREATE CLASS TBROWSE

/* The first 18 instance variables are exactly the same as in Clipper
 * so also some code which access them directly by array indexes should work
 */
   /* === Start of CA-Cl*pper compatible TBrowse instance area === */
   VAR cargo      AS USUAL          EXPORTED    // 01. User-definable variable

PROTECTED:
   VAR n_Top      AS NUMERIC INIT 0             // 02. Top row number for the TBrowse display
   VAR n_Left     AS NUMERIC INIT 0             // 03. Leftmost column for the TBrowse display
   VAR n_Bottom   AS NUMERIC INIT 0             // 04. Bottom row number for the TBrowse display
   VAR n_Right    AS NUMERIC INIT 0             // 05. Rightmost column for the TBrowse display

   VAR columns    AS ARRAY INIT {}              // 06. Array of TBrowse columns

   VAR cHeadSep   AS CHARACTER INIT ""          // 07. Heading separator characters
   VAR cColSep    AS CHARACTER INIT " "         // 08. Column separator characters
   VAR cFootSep   AS CHARACTER INIT ""          // 09. Footing separator characters

   VAR cColorSpec AS CHARACTER                  // 10. Color table for the TBrowse display

   VAR bSkipBlock     AS BLOCK INIT {|| NIL }   // 11. Code block used to reposition data source
   VAR bGoTopBlock    AS BLOCK INIT {|| NIL }   // 12. Code block executed by TBrowse:goTop()
   VAR bGoBottomBlock AS BLOCK INIT {|| NIL }   // 13. Code block executed by TBrowse:goBottom()

#ifdef HB_COMPAT_C53
   VAR dummy                   INIT ""          // 14. ??? In Clipper it's character variable with internal C level structure containing browse data
   VAR cBorder    AS CHARACTER                  // 15. character value defining characters drawn around object
   VAR cMessage                                 // 16. character string displayed on status bar
   VAR keys       AS ARRAY                      // 17. array with SetKey() method values
   VAR styles     AS ARRAY                      // 18. array with SetStyle() method values
#endif
   /* === End of CA-Cl*pper compatible TBrowse instance area === */

EXPORTED:

#ifdef HB_COMPAT_C53
#ifdef HB_BRW_STATICMOUSE
   VAR mRowPos    AS INTEGER INIT 0             // numeric value indicating the data row of the mouse position
   VAR mColPos    AS INTEGER INIT 0             // numeric value indicating the data column of the mouse position
#else
   METHOD mRowPos SETGET                        // numeric value indicating the data row of the mouse position
   METHOD mColPos SETGET                        // numeric value indicating the data column of the mouse position
#endif

   METHOD setStyle( nStyle, lNewValue )         // maintains a dictionary within an object
   METHOD setKey( nKey, bBlock )                // get/set a code block associated with an INKEY() value
   METHOD applyKey( nKey )                      // evaluate the code block associated with given INKEY() value
   METHOD hitTest( mRow, mCol )                 // indicate position of mouse cursor relative to TBrowse
   METHOD nRow SETGET                           // screen row number for the actual cell
   METHOD nCol SETGET                           // screen column number for the actual cell
   METHOD border( cBorder ) SETGET              // get/set character value used for TBrowse are border
   METHOD message( cMessage ) SETGET            // get/set character string displayed on status bar
#endif

   METHOD nTop( nTop ) SETGET                   // get/set top row number for the TBrowse display
   METHOD nLeft( nLeft ) SETGET                 // get/set leftmost column for the TBrowse display
   METHOD nBottom( nBottom ) SETGET             // get/set bottom row number for the TBrowse display
   METHOD nRight( nRight ) SETGET               // get/set rightmost column for the TBrowse display

   METHOD headSep( cHeadSep ) SETGET            // get/set heading separator characters
   METHOD colSep( cColSep ) SETGET              // get/set column separator characters
   METHOD footSep( cFootSep ) SETGET            // get/set footing separator characters
   METHOD skipBlock( bSkipBlock ) SETGET        // get/set code block used to reposition data source
   METHOD goTopBlock( bBlock ) SETGET           // get/set code block executed by TBrowse:goTop()
   METHOD goBottomBlock( bBlock ) SETGET        // get/set code block executed by TBrowse:goBottom()

   METHOD colorSpec( cColorSpec ) SETGET        // get/set string value with color table for the TBrowse display

   ACCESS rowPos    METHOD getRowPos            // get current cursor row position
   ASSIGN rowPos    METHOD setRowPos            // set current cursor row position

   ACCESS colPos    METHOD getColPos            // get current cursor column position
   ASSIGN colPos    METHOD setColPos            // set current cursor column position

   ACCESS freeze    METHOD getFrozen            // get number of frozen columns
   ASSIGN freeze    METHOD freeze               // set number of columns to freeze

   ACCESS hitTop    METHOD getTopFlag           // get the beginning of available data flag
   ASSIGN hitTop    METHOD setTopFlag           // set the beginning of available data flag

   ACCESS hitBottom METHOD getBottomFlag        // get the end of available data flag
   ASSIGN hitBottom METHOD setBottomFlag        // set the end of available data flag

   ACCESS autoLite  METHOD getAutoLite          // get automatic highlighting state
   ASSIGN autoLite  METHOD setAutoLite          // set automatic highlighting

   ACCESS stable    METHOD getStableFlag        // get flag indicating if the TBrowse object is stable
   ASSIGN stable    METHOD setStableFlag        // set flag indicating if the TBrowse object is stable

   METHOD addColumn( oCol )                     // adds a TBColumn object to the TBrowse object
   METHOD delColumn( nColumn )                  // delete a column object from a browse
   METHOD insColumn( nColumn, oCol )            // insert a column object in a browse
   METHOD setColumn( nColumn, oCol )            // replaces one TBColumn object with another
   METHOD getColumn( nColumn )                  // gets a specific TBColumn object

   METHOD rowCount()                            // number of visible data rows in the TBrowse display
   METHOD colCount()                            // number of browse columns

   METHOD colWidth( nColumn )                   // returns the display width of a particular column

   METHOD leftVisible()                         // indicates position of leftmost unfrozen column in display
   METHOD rightVisible()                        // indicates position of rightmost unfrozen column in display

   METHOD hilite()                              // highlights the current cell
   METHOD deHilite()                            // dehighlights the current cell
   METHOD refreshAll()                          // causes all data to be recalculated during the next stabilize
   METHOD refreshCurrent()                      // causes the current row to be refilled and repainted on next stabilize
   METHOD forceStable()                         // performs a full stabilization
   METHOD invalidate()                          // forces entire redraw during next stabilization

   METHOD up()                                  // moves the cursor up one row
   METHOD down()                                // moves the cursor down one row
   METHOD left()                                // moves the cursor left one column
   METHOD right()                               // moves the cursor right one column
   METHOD pageUp()                              // repositions the data source upward
   METHOD pageDown()                            // repositions the data source downward
   METHOD home()                                // moves the cursor to the leftmost visible data column
   METHOD end()                                 // moves the cursor to the rightmost visible data column

   METHOD goTop()                               // repositions the data source to the top of file
   METHOD goBottom()                            // repositions the data source to the bottom of file

   METHOD panLeft()                             // pans left without changing the cursor position
   METHOD panRight()                            // pans right without changing the cursor position
   METHOD panHome()                             // moves the cursor to the leftmost visible data column
   METHOD panEnd()                              // moves the cursor to the rightmost data column

   METHOD stabilize()                           // performs incremental stabilization
   METHOD colorRect( aRect, aColors )           // alters the color of a rectangular group of cells

   /* NOTE: nMode is an undocumented parameter in CA-Cl*pper */
   METHOD configure( nMode )                    // mark that the internal settings of the TBrowse object should be reconfigured

   METHOD new( nTop, nLeft, nBottom, nRight )   // constructor, NOTE: This method is a Harbour extension [vszakats]

PROTECTED:
   VAR nRowPos       AS INTEGER INIT 1          // current cursor row position
   VAR nColPos       AS INTEGER INIT 1          // current cursor column position
   VAR nLeftVisible  AS INTEGER INIT 0          // indicates position of leftmost unfrozen column in display
   VAR nRightVisible AS INTEGER INIT 0          // indicates position of rightmost unfrozen column in display
   VAR n_Row         AS INTEGER INIT 0          // current cursor screen row position
   VAR n_Col         AS INTEGER INIT 0          // current cursor screen column position
   VAR nHeadHeight   AS INTEGER INIT 0          // heading vertical size
   VAR nFootHeight   AS INTEGER INIT 0          // footing vertical size
   VAR nFrozen       AS INTEGER INIT 0          // number of frozen columns
   VAR nBufferPos    AS INTEGER INIT 1          // position in row buffer
   VAR nMoveOffset   AS INTEGER INIT 0          // requested repositioning
   VAR nLastRow      AS INTEGER INIT 0          // last row in the buffer
   VAR nLastScroll   AS INTEGER INIT 0          // last srcoll value
   VAR nConfigure    AS INTEGER INIT _TBR_CONF_ALL // configuration status
   VAR nLastPos      AS INTEGER INIT 0          // last calculated column position
   VAR lHitTop       AS LOGICAL INIT .F.        // indicates the beginning of available data
   VAR lHitBottom    AS LOGICAL INIT .F.        // indicates the end of available data
   VAR lHiLited      AS LOGICAL INIT .F.        // indicates if current cell is highlighted
   VAR lAutoLite     AS LOGICAL INIT .T.        // logical value to control highlighting
   VAR lStable       AS LOGICAL INIT .F.        // indicates if the TBrowse object is stable
   VAR lInvalid      AS LOGICAL INIT .T.        // indicates that TBrowse object data should be fully redrawn
   VAR lRefresh      AS LOGICAL INIT .F.        // indicates that record buffer should be discarded in next stabilization
   VAR lFrames       AS LOGICAL INIT .F.        // indicates that headings and footings should be redrawn
   VAR lHeadSep      AS LOGICAL INIT .F.        // indicates if heading separator exists
   VAR lFootSep      AS LOGICAL INIT .F.        // indicates if footing separator exists
   VAR aColData      AS ARRAY   INIT {}         // column information, see _TBCI_*
   VAR aColors       AS ARRAY   INIT {}         // array with TBrowse colors, see _TBC_CLR_*
   VAR aDispStatus   AS ARRAY   INIT {}         // record buffer status
   VAR aCellStatus   AS ARRAY   INIT {}         // record buffer status
   VAR aCellValues   AS ARRAY   INIT {}         // cell values buffers for each record
   VAR aCellColors   AS ARRAY   INIT {}         // cell colors buffers for each record

   METHOD doConfigure()                         // reconfigures the internal settings of the TBrowse object
   METHOD setUnstable()                         // set TBrows in unstable mode resetting flags
   METHOD setPosition()                         // synchronize record position with the buffer
   METHOD readRecord( nRow )                    // read current record into the buffer

   METHOD setVisible()                          // set visible columns
   METHOD setCursorPos()                        // set screen cursor position at current cell
   METHOD scrollBuffer( nRows )                 // scroll internal buffer for given row numbers
   METHOD colorValue( nColorIndex )             // get color value for given index
   METHOD cellValue( nRow, nCol )               // get cell color indexes
   METHOD cellColor( nRow, nCol )               // get cell formatted value
   METHOD dispFrames()                          // display TBrowse border, columns' headings, footings and separators
   METHOD dispRow( nRow )                       // display TBrowse data

   FRIEND FUNCTION _mBrwPos                     // helper function for mRow() and mCol() methods

ENDCLASS



FUNCTION TBrowseNew( nTop, nLeft, nBottom, nRight )

   RETURN TBrowse():new( nTop, nLeft, nBottom, nRight )


METHOD new( nTop, nLeft, nBottom, nRight ) CLASS TBROWSE

   DEFAULT nTop    TO 0
   DEFAULT nLeft   TO 0
   DEFAULT nBottom TO MaxRow()
   DEFAULT nRight  TO MaxCol()

   ::nTop    := nTop
   ::nLeft   := nLeft
   ::nBottom := nBottom
   ::nRight  := nRight

   ::colorSpec := SetColor()

   RETURN Self

STATIC FUNCTION _SKIP_RESULT( xResult )

   RETURN iif( ISNUMBER( xResult ), Int( xResult ), 0 )


STATIC PROCEDURE _DISP_FHSEP( nRow, nType, cColor, aColData )
   LOCAL aCol
   LOCAL cSep
   LOCAL nLen
   LOCAL nWidth
   LOCAL lFirst := .T.
   LOCAL lFirstVisible := .T.

   FOR EACH aCol IN aColData
      IF aCol[ _TBCI_COLPOS ] != NIL
         cSep := aCol[ nType ]
         nWidth := aCol[ _TBCI_COLWIDTH ]

         /* This is in my opinion bug which should be fixed
          * and the First column should be shown with the
          * same conditions as first visible column.
          * Now I replicated exact CA-Cl*pper behavior but
          * probably in the future it will be changed. [druzus]
          */
         IF lFirst
            lFirst := lFirstVisible := .F.
            cSep := Replicate( Right( cSep, 1 ), nWidth + ;
                               aCol[ _TBCI_FROZENSPACE ] )
         ELSEIF lFirstVisible
            lFirstVisible := .F.
            nLen := Len( cSep )
            IF nLen <= aCol[ _TBCI_SEPWIDTH ]
               cSep := Replicate( Right( cSep, 1 ), nWidth + ;
                                  aCol[ _TBCI_FROZENSPACE ] )
            ELSE
               cSep := Substr( cSep, aCol[ _TBCI_SEPWIDTH ] + 1, nWidth )
               IF ( nLen -= aCol[ _TBCI_SEPWIDTH ] + nWidth ) < 0
                  cSep += Replicate( Right( cSep, 1 ), -nLen )
               ENDIF
               IF aCol[ _TBCI_FROZENSPACE ] > 0
                  cSep := Replicate( Left( cSep, 1 ), aCol[ _TBCI_FROZENSPACE ] ) + ;
                                     cSep
               ENDIF
            ENDIF
         ELSE
            nLen := Len( cSep ) - aCol[ _TBCI_SEPWIDTH ] - nWidth
            IF nLen > 0
               cSep := Left( cSep, aCol[ _TBCI_SEPWIDTH ] + nWidth )
            ELSEIF nLen < 0
               cSep += Replicate( Right( cSep, 1 ), -nLen )
            ENDIF
            IF aCol[ _TBCI_FROZENSPACE ] > 0
               cSep := Stuff( cSep, aCol[ _TBCI_SEPWIDTH ] + 1, 0, ;
                              Replicate( Substr( cSep, aCol[ _TBCI_SEPWIDTH ] + 1, 1 ), ;
                                         aCol[ _TBCI_FROZENSPACE ] ), cSep )
            ENDIF
         ENDIF
         IF aCol[ _TBCI_LASTSPACE ] > 0
            cSep += Replicate( Right( cSep, 1 ), aCol[ _TBCI_LASTSPACE ] )
         ELSEIF aCol[ _TBCI_LASTSPACE ] < 0
            cSep := Left( cSep, Len( cSep ) + aCol[ _TBCI_LASTSPACE ] )
         ENDIF
         hb_dispOutAtBox( nRow, aCol[ _TBCI_COLPOS ] - aCol[ _TBCI_FROZENSPACE ], ;
                          cSep, cColor )
      ELSEIF aCol[ _TBCI_CELLWIDTH ] > 0
         lFirst := .F.
      ENDIF
   NEXT

   RETURN


STATIC PROCEDURE _DISP_FHNAME( nRow, nHeight, nLeft, nRight, nType, nColor, aColors, aColData )

   LOCAL aCol
   LOCAL cName
   LOCAL nPos
   LOCAL nCol
   LOCAL nWidth
   LOCAL lFirst := .T.

   hb_dispBox( nRow, nLeft, nRow + nHeight - 1, nRight, ;
               Space( 9 ), aColors[ _TBC_CLR_STANDARD ] )

   FOR EACH aCol IN aColData
      IF aCol[ _TBCI_COLPOS ] != NIL
         cName := aCol[ nType ]
         nCol := aCol[ _TBCI_COLPOS ]
         IF lFirst
            lFirst := .F.
         ELSE
            nCol += aCol[ _TBCI_SEPWIDTH ]
         ENDIF
         nWidth := aCol[ _TBCI_COLWIDTH ]
         IF aCol[ _TBCI_LASTSPACE ] < 0
            nWidth += aCol[ _TBCI_LASTSPACE ]
         ENDIF
         FOR nPos := 1 TO nHeight
            hb_dispOutAt( nRow + nPos - 1, nCol, ;
                          PadR( hb_tokenGet( cName, nPos, _TBR_CHR_LINEDELIMITER ), nWidth ), ;
                          iif( aCol[ _TBCI_DEFCOLOR ][ nColor ] == 0, "N/N", ;
                               aColors[ aCol[ _TBCI_DEFCOLOR ][ nColor ] ] ) )
         NEXT
      ENDIF
   NEXT

   RETURN


METHOD dispFrames() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   DispBegin()

   IF ::lInvalid .AND. !Empty( ::cBorder )
      hb_dispBox( ::nTop, ::nLeft, ::nBottom, ::nRight, ::cBorder, ::colorValue( _TBC_CLR_STANDARD ) )
   ENDIF

   IF ::nHeadHeight > 0
      _DISP_FHNAME( ::n_Top, ::nHeadHeight, ::n_Left, ::n_Right, _TBCI_HEADING, ;
                    iif( ::lHeadSep, _TBC_CLR_HEADING, _TBC_CLR_STANDARD ), ;
                    ::aColors, ::aColData )
   ENDIF
   IF ::lHeadSep
      _DISP_FHSEP( ::n_Top + ::nHeadHeight, _TBCI_HEADSEP, ;
                   ::colorValue( _TBC_CLR_STANDARD ), ::aColData )
   ENDIF
   IF ::lFootSep
      _DISP_FHSEP( ::n_Bottom - ::nFootHeight, _TBCI_FOOTSEP, ;
                   ::colorValue( _TBC_CLR_STANDARD ), ::aColData )
   ENDIF
   IF ::nFootHeight > 0
      _DISP_FHNAME( ::n_Bottom - ::nFootHeight + 1, ::nFootHeight, ::n_Left, ::n_Right, _TBCI_FOOTING, ;
                    iif( ::lFootSep, _TBC_CLR_FOOTING, _TBC_CLR_STANDARD ), ;
                    ::aColors, ::aColData )
   ENDIF

   DispEnd()

   ::lFrames := .F.

   RETURN Self


METHOD dispRow( nRow ) CLASS TBROWSE

   LOCAL nRowPos, nColPos
   LOCAL aCol
   LOCAL lFirst
   LOCAL cValue, cColor, cStdColor
   LOCAL aColors

   IF nRow >= 1 .AND. nRow <= ::rowCount

      DispBegin()

      nRowPos := ::n_Top + ::nHeadHeight + iif( ::lHeadSep, 1, 0 ) + nRow - 1
      cStdColor := ::colorValue( _TBC_CLR_STANDARD )

      hb_dispBox( nRowPos, ::n_Left, nRowPos, ::n_Right, Space( 9 ), cStdColor )

      lFirst := .T.
      FOR EACH aCol, cValue, aColors IN ::aColData, ::aCellValues[ nRow ], ::aCellColors[ nRow ]
         IF aCol[ _TBCI_COLPOS ] != NIL
            nColPos := aCol[ _TBCI_COLPOS ]
            IF lFirst
               lFirst := .F.
            ELSEIF aCol[ _TBCI_SEPWIDTH ] > 0
               hb_dispOutAtBox( nRowPos, aCol[ _TBCI_COLPOS ] - aCol[ _TBCI_FROZENSPACE ], ;
                                aCol[ _TBCI_COLSEP ], cStdColor )
               nColPos += aCol[ _TBCI_SEPWIDTH ]
            ENDIF
            nColPos += aCol[ _TBCI_CELLPOS ]
            cColor := ::colorValue( aColors[ _TBC_CLR_STANDARD ] )
            IF aCol[ _TBCI_LASTSPACE ] < 0
               hb_dispOutAt( nRowPos, nColPos, ;
                             Left( cValue, ::n_Right - nColPos + 1 ), cColor )
            ELSE
#ifdef HB_CLP_STRICT
               hb_dispOutAt( nRowPos, nColPos, ;
                             Left( cValue, aCol[ _TBCI_COLWIDTH ] - aCol[ _TBCI_CELLPOS ] ), cColor )
#else
               hb_dispOutAt( nRowPos, nColPos, cValue, cColor )
#endif
            ENDIF
         ENDIF
      NEXT

      ::aDispStatus[ nRow ] := .F.

      DispEnd()
   ENDIF

   RETURN Self


METHOD colorRect( aRect, aColors ) CLASS TBROWSE

   LOCAL nRow := ::rowCount
   LOCAL nCol := ::colCount

   /* CA-Cl*pper checks all this conditions */
   IF ISARRAY( aRect ) .AND. Len( aRect ) >= 4 .AND. ;
      ISNUMBER( aRect[ 1 ] ) .AND. ISNUMBER( aRect[ 2 ] ) .AND. ;
      ISNUMBER( aRect[ 3 ] ) .AND. ISNUMBER( aRect[ 4 ] ) .AND. ;
      aRect[ 1 ] >= 1 .AND. aRect[ 1 ] <= nRow .AND. ;
      aRect[ 2 ] >= 1 .AND. aRect[ 2 ] <= nCol .AND. ;
      aRect[ 3 ] >= aRect[ 1 ] .AND. aRect[ 3 ] <= nRow .AND. ;
      aRect[ 4 ] >= aRect[ 2 ] .AND. aRect[ 4 ] <= nCol .AND. ;
      ;
      ISARRAY( aColors ) .AND. Len( aColors ) >= 2 .AND. ;
      ISNUMBER( aColors[ 1 ] ) .AND. ISNUMBER( aColors[ 2 ] ) .AND. ;
      ; /* in colorRect() index 0 is not supported */
      aColors[ 1 ] >= 1 .AND. aColors[ 1 ] <= Len( ::aColors ) .AND. ;
      aColors[ 2 ] >= 1 .AND. aColors[ 2 ] <= Len( ::aColors )

      ::setVisible()

      FOR nRow := aRect[ 1 ] TO aRect[ 3 ]
         ::readRecord( nRow )
         FOR nCol := aRect[ 2 ] TO aRect[ 4 ]
            ::aCellColors[ nRow, nCol, 1 ] := aColors[ 1 ]
            ::aCellColors[ nRow, nCol, 2 ] := aColors[ 2 ]
         NEXT
         ::dispRow( nRow )
      NEXT
   ENDIF

   RETURN Self


METHOD scrollBuffer( nRows ) CLASS TBROWSE

   LOCAL nRowCount := ::rowCount
   LOCAL aValues, aColors

   /* Store last scroll value to chose refresh order. [druzus] */
   ::nLastScroll := nRows

   IF nRows >= nRowCount .OR. nRows <= -nRowCount
      AFill( ::aCellStatus, .F. )
   ELSE
      hb_scroll( ::n_Top + ::nHeadHeight + iif( ::lHeadSep, 1, 0 ), ::n_Left, ;
                 ::n_Bottom - ::nFootHeight - iif( ::lFootSep, 1, 0 ), ::n_Right, ;
                 nRows,, ::colorValue( _TBC_CLR_STANDARD ) )
      IF nRows > 0
         DO WHILE --nRows >= 0
            aValues := ::aCellValues[ 1 ]
            aColors := ::aCellColors[ 1 ]
            ADel( ::aCellValues, 1 )
            ADel( ::aCellColors, 1 )
            ADel( ::aCellStatus, 1 )
            ADel( ::aDispStatus, 1 )
            ::aCellValues[ nRowCount ] := aValues
            ::aCellColors[ nRowCount ] := aColors
            ::aCellStatus[ nRowCount ] := .F.
            ::aDispStatus[ nRowCount ] := .T.
         ENDDO
      ELSEIF nRows < 0
         DO WHILE ++nRows <= 0
            HB_AIns( ::aCellValues, 1, ATail( ::aCellValues ), .F. )
            HB_AIns( ::aCellColors, 1, ATail( ::aCellColors ), .F. )
            HB_AIns( ::aCellStatus, 1, .F., .F. )
            HB_AIns( ::aDispStatus, 1, .T., .F. )
         ENDDO
      ENDIF
   ENDIF

   RETURN Self


METHOD readRecord( nRow ) CLASS TBROWSE

   LOCAL aCol
   LOCAL oCol
   LOCAL cValue
   LOCAL aColor
   LOCAL nColors, nToMove, nMoved
   LOCAL nRowCount := ::rowCount
   LOCAL lRead := .F.

   IF nRow >= 1 .AND. nRow <= nRowCount .AND. !::aCellStatus[ nRow ]

      IF nRow <= ::nLastRow
         nToMove := nRow - ::nBufferPos
         nMoved := _SKIP_RESULT( Eval( ::bSkipBlock, nToMove ) )
         /* TOFIX: add protection against unexpected results
          *        CA-Cl*pper does not fully respect here the returned
          *        value and current code below replicates what Clipper
          *        seems to do but it means that in network environment
          *        with concurent modifications wrong records can be
          *        shown. [druzus]
          */
         IF nToMove > 0
            IF nMoved < 0
               nMoved := 0
            ENDIF
         ELSEIF nToMove < 0
            nMoved := nToMove
         ELSE
            nMoved := 0
         ENDIF
         ::nBufferPos += nMoved
         IF nToMove > 0 .AND. nMoved < nToMove
            ::nLastRow := ::nBufferPos
         ELSE
            lRead := .T.
         ENDIF
      ENDIF

      nColors := Len( ::aColors )
      IF nRow <= ::nLastRow
         FOR EACH aCol, cValue, aColor IN ::aColData, ::aCellValues[ nRow ], ::aCellColors[ nRow ]
            oCol := aCol[ _TBCI_COLOBJECT ]
            cValue := Eval( oCol:block )
            aColor := _CELLCOLORS( aCol, cValue, nColors )
            IF ValType( cValue ) $ "CMNDTL"
               cValue := PadR( Transform( cValue, oCol:picture ), aCol[ _TBCI_CELLWIDTH ] )
            ELSE
               cValue := Space( aCol[ _TBCI_CELLWIDTH ] )
            ENDIF
         NEXT
      ELSE
         FOR EACH aCol, cValue, aColor IN ::aColData, ::aCellValues[ nRow ], ::aCellColors[ nRow ]
            aColor := { aCol[ _TBCI_DEFCOLOR ][ 1 ], aCol[ _TBCI_DEFCOLOR ][ 2 ] }
            cValue := Space( aCol[ _TBCI_CELLWIDTH ] )
         NEXT
      ENDIF

      ::aCellStatus[ nRow ] := .T.
      ::aDispStatus[ nRow ] := .T.

   ENDIF

   RETURN lRead


METHOD setPosition() CLASS TBROWSE

   LOCAL nMoved
   LOCAL nRowCount := ::rowCount
   LOCAL nMoveOffset := ::nMoveOffset + ( ::nRowPos - ::nBufferPos )
   LOCAL nNewPos := ::nBufferPos + nMoveOffset
   LOCAL lSetPos := .T.

   IF nNewPos < 1
      IF ::nMoveOffset < -1
         nMoveOffset -= ::nRowPos - 1
      ENDIF
   ELSEIF nNewPos > ::nLastRow
      IF ::nMoveOffset > 1
         nMoveOffset += ::nLastRow - ::nRowPos
      ENDIF
   ELSEIF lSetPos
      ::nRowPos := nNewPos
   ENDIF

   nMoved := _SKIP_RESULT( Eval( ::bSkipBlock, nMoveOffset ) )

   IF nMoved > 0
      ::nBufferPos += nMoved
      IF ::nBufferPos > ::nLastRow
         AFill( ::aCellStatus, .F., ::nLastRow + 1, ::nBufferPos - ::nLastRow )
      ENDIF
      IF ::nBufferPos > nRowCount
         ::scrollBuffer( ::nBufferPos - nRowCount )
         ::nBufferPos := nRowCount
         lSetPos := .F.
      ENDIF
      IF ::nBufferPos > ::nLastRow
         ::nLastRow := ::nBufferPos
         IF nMoved != nMoveOffset
            lSetPos := .F.
         ENDIF
      ENDIF
   ELSEIF nMoved < 0
      ::nBufferPos += nMoved
      IF ::nBufferPos < 1
         ::nLastRow := Min( nRowCount, ::nLastRow - ::nBufferPos + 1 )
         ::scrollBuffer( ::nBufferPos - 1 )
         ::nBufferPos := 1
         lSetPos := .F.
      ENDIF
   ELSE  /* nMoved == 0 */
      IF nMoveOffset > 0
         IF nMoveOffset != 0 .AND. ::nBufferPos == ::nRowPos
            ::lHitBottom := .T.
         ENDIF
         ::nLastRow := ::nBufferPos
         /* CA-Cl*pper does not do that */
         AFill( ::aCellStatus, .F., ::nLastRow + 1 )
      ELSEIF nMoveOffset < 0
         IF nMoveOffset != 0 .AND. ::nBufferPos == ::nRowPos
            ::lHitTop := .T.
         ENDIF
         /* CA-Cl*pper does not do that */
         IF ::nBufferPos > 1
            ::scrollBuffer( ::nBufferPos - 1 )
            ::nBufferPos := 1
         ENDIF
      ENDIF
   ENDIF

   IF lSetPos
      ::nRowPos := ::nBufferPos
   ENDIF

   ::nMoveOffset := 0

   RETURN Self


METHOD stabilize() CLASS TBROWSE

   LOCAL nRowCount, nToMove, nMoved
   LOCAL lDisp, lRead, lStat

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   IF !::lStable .OR. ::lInvalid .OR. ::lFrames .OR. ::lRefresh .OR. ;
      ::nMoveOffset != 0 .OR. ::nBufferPos != ::nRowPos

      nRowCount := ::rowCount

      IF ::lRefresh
         AFill( ::aCellStatus, .F. )
         ::nLastRow := nRowCount
         ::nLastScroll := 0
         ::lRefresh := .F.
      ENDIF

      ::setVisible()

      IF ::lFrames
         ::dispFrames()
         AFill( ::aDispStatus, .T. )
      ENDIF

      lRead := .F.
      IF ::nMoveOffset != 0
         ::setPosition()
         lRead := .T.
      ENDIF

      IF ::nLastScroll > 0
         FOR EACH lStat, lDisp IN ::aCellStatus, ::aDispStatus DESCEND
            IF !lStat
               IF lRead
                  RETURN .F.
               ENDIF
               lRead := ::readRecord( lStat:__enumIndex() )
            ENDIF
            IF lDisp
               ::dispRow( lDisp:__enumIndex() )
            ENDIF
         NEXT
      ELSE
         FOR EACH lStat, lDisp IN ::aCellStatus, ::aDispStatus
            IF !lStat
               IF lRead
                  RETURN .F.
               ENDIF
               lRead := ::readRecord( lStat:__enumIndex() )
            ENDIF
            IF lDisp
               ::dispRow( lDisp:__enumIndex() )
            ENDIF
         NEXT
      ENDIF

      IF ::nRowPos > ::nLastRow
         ::nRowPos := ::nLastRow
      ENDIF
      IF ::nBufferPos != ::nRowPos
         /* TOFIX: add protection against unexpected results
          *        CA-Cl*pper does not fully respect here the returned
          *        value and current code below replicates what Clipper
          *        seems to do but it means that in network environment
          *        with concurent modifications wrong records can be
          *        shown. [druzus]
          */
         nToMove := ::nRowPos - ::nBufferPos
         nMoved := _SKIP_RESULT( Eval( ::bSkipBlock, nToMove ) )
         IF nToMove > 0
            IF nMoved < 0
               nMoved := 0
            ENDIF
         ELSEIF nToMove < 0
            nMoved := nToMove
         ELSE
            nMoved := 0
         ENDIF
         ::nBufferPos += nMoved
         ::nRowPos := ::nBufferPos
      ENDIF
      ::lStable := .T.
      ::lInvalid := .F.
   ENDIF

   IF ::autoLite
      ::hilite()
   ELSE
      ::setCursorPos()
   ENDIF

   RETURN .T.


METHOD forceStable() CLASS TBROWSE

   /* TODO: CA-Cl*pper does not call ::stabilize() if TBrowse object
    *       is stable and does not need screen update. It may be important
    *       for applications which do not expect that cursor position may
    *       be changed. I'll change it in the future but first I will have
    *       to revert my stupid modifications in Harbour core code. Looking
    *       at old TBrowse implementation I replaced some:
    *             DO WHILE !oBrw:stabilize(); END
    *       with:
    *             oBrw:forceStable()
    *       In Clipper it's not the same because oBrw:forceStable()
    *       may not set cursor position and only ::stabilize() does it.
    *       [druzus]
    */
   DO WHILE !::stabilize()
   ENDDO

   RETURN Self


METHOD colorValue( nColorIndex ) CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   IF ISNUMBER( nColorIndex )
      IF nColorIndex >= 1 .AND. nColorIndex <= Len( ::aColors )
         RETURN ::aColors[ nColorIndex ]
      /* In CA-Cl*pper index 0 has special meaning - it's always N/N color */
      ELSEIF nColorIndex == 0
         RETURN "N/N"
      ENDIF
   ENDIF

   RETURN ::aColors[ _TBC_CLR_STANDARD ]


METHOD cellValue( nRow, nCol ) CLASS TBROWSE

   IF nRow >= 1 .AND. nRow <= ::rowCount .AND. ;
      nCol >= 1 .AND. nCol <= ::colCount .AND. ;
      ::aCellStatus[ nRow ]

      RETURN ::aCellValues[ nRow, nCol ]
   ENDIF

   RETURN NIL


METHOD cellColor( nRow, nCol ) CLASS TBROWSE

   IF nRow >= 1 .AND. nRow <= ::rowCount .AND. ;
      nCol >= 1 .AND. nCol <= ::colCount .AND. ;
      ::aCellStatus[ nRow ]

      RETURN ::aCellColors[ nRow, nCol ]
   ENDIF

   RETURN NIL


STATIC FUNCTION _DECODECOLORS( cColorSpec )
   LOCAL aColors := {}
   LOCAL nColors := hb_TokenCount( cColorSpec, "," )
   LOCAL cColor
   LOCAL nPos

   FOR nPos := 1 TO nColors
      cColor := hb_tokenGet( cColorSpec, nPos, "," )
      /* For 1-st two colors CA-Cl*pper checks if given color
       * definition has at least one of the following characters:
       * "*+/bBgGrRwWnNiIxXuU0123456789"
       * If not then it takes default color value.
       * In Harbour this validation is redirected to GT system which
       * decides if passed string is valid color definition. The default
       * GT implementation accepts exactly the same color definitions
       * as CA-Cl*pper but some new GTs may accept extended color
       * definitions and use a little bit different rules. [druzus]
       */
      IF nPos <= 2 .AND. hb_colorToN( cColor ) == -1
         cColor := iif( nPos == 1, "W/N", "N/W" )
      ENDIF
      AAdd( aColors, cColor )
   NEXT
   IF Empty( aColors )
      AAdd( aColors, "W/N" )
   ENDIF
   IF Len( aColors ) < 2
      AAdd( aColors, "N/W" )
   ENDIF
   DO WHILE Len( aColors ) < _TBC_CLR_MAX
      AAdd( aColors, aColors[ _TBC_CLR_STANDARD ] )
   ENDDO

   RETURN aColors


/* Color indexes returned by oCol:defColor are adopted to existing
 * TBrowse colors and buffered during configuration.
 * When index is greater then size of colorspec the default index is used
 * Index 0 has special meaning - it's color "N/N"
 * In CL5.3 headings and footings can have separated indexes (3 and 4)
 * but only when browser shows head/foot separator(s). [druzus]
 */
STATIC FUNCTION _COLDEFCOLORS( aDefColorsIdx, nMaxColorIndex )
   LOCAL aColorsIdx := { _TBC_CLR_STANDARD, _TBC_CLR_SELECTED, ;
                         _TBC_CLR_STANDARD, _TBC_CLR_STANDARD }
   LOCAL nColorIndex
   LOCAL nPos

   IF ISARRAY( aDefColorsIdx )
      FOR nPos := 1 TO _TBC_CLR_MAX
         IF nPos <= Len( aDefColorsIdx ) .AND. ;
            ISNUMBER( nColorIndex := aDefColorsIdx[ nPos ] ) .AND. ;
            ( nColorIndex := Int( nColorIndex ) ) >= 0 .AND. ;
            nColorIndex <= nMaxColorIndex

            aColorsIdx[ nPos ] := nColorIndex
         ELSEIF nPos > 2
            aColorsIdx[ nPos ] := aColorsIdx[ 1 ]
         ENDIF
      NEXT
   ENDIF

   RETURN aColorsIdx


/* If oCol:colorBlock does not return array length enough then colors
 * are taken from preprocessed during configuration oCol:defColor array.
 * oCol:colorBlock is used only for cells so only 1-st two color indexes
 * are significant. [druzus]
 */
STATIC FUNCTION _CELLCOLORS( aCol, xValue, nMaxColorIndex )
   LOCAL aColors := { aCol[ _TBCI_DEFCOLOR ][ _TBC_CLR_STANDARD ], ;
                      aCol[ _TBCI_DEFCOLOR ][ _TBC_CLR_SELECTED ] }
   LOCAL xColor := Eval( aCol[ _TBCI_COLOBJECT ]:colorBlock, xValue )
   LOCAL nColorIndex
   LOCAL nPos, nMax

   IF ISARRAY( xColor )
      nMax := Min( Len( xColor ), 2 )
      FOR nPos := 1 TO nMax
         nColorIndex := xColor[ nPos ]
         IF ISNUMBER( nColorIndex )
            nColorIndex := Int( nColorIndex )
            IF nColorIndex >= 0 .AND. nColorIndex <= nMaxColorIndex
               aColors[ nPos ] := nColorIndex
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN aColors


METHOD setCursorPos() CLASS TBROWSE

   LOCAL aCol
   LOCAL nRow, nCol

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   nRow := ::nRowPos
   nCol := ::nColPos

   IF nRow >= 1 .AND. nRow <= ::rowCount .AND. ;
      nCol >= 1 .AND. nCol <= ::colCount .AND. ;
      ( aCol := ::aColData[ nCol ] )[ _TBCI_COLPOS ] != NIL

      ::n_Row := ::n_Top + ::nHeadHeight + iif( ::lHeadSep, 0, -1 ) + nRow
      ::n_Col := ::aColData[ nCol ][ _TBCI_COLPOS ] + ;
                 ::aColData[ nCol ][ _TBCI_CELLPOS ]
      IF aCol[ _TBCI_SEPWIDTH ] > 0
         DO WHILE --nCol >= 1
            IF ::aColData[ nCol ][ _TBCI_COLPOS ] != NIL
               ::n_Col += aCol[ _TBCI_SEPWIDTH ]
               EXIT
            ENDIF
         ENDDO
      ENDIF
      SetPos( ::n_Row, ::n_Col )
      RETURN .T.
   ENDIF

   RETURN .F.


METHOD setUnstable() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   /* CA-Cl*pper dehighlights the current cell */
   IF ::lHiLited
      ::deHilite()
   ENDIF

   ::lHitTop    := .F.
   ::lHitBottom := .F.
   ::lStable    := .F.

   RETURN Self


METHOD invalidate() CLASS TBROWSE

   ::setUnstable()
   ::lInvalid := .T.
   ::lFrames := .T.

   RETURN Self


METHOD refreshAll() CLASS TBROWSE

   ::setUnstable()

   Eval( ::bSkipBlock, 1 - ::nBufferPos )
   ::nBufferPos := 1
   ::lFrames := .T.
   /* In CA-Cl*pper refreshAll() method does not discards
    * record buffer here but only set's flag that the record
    * buffer should be reloaded in stabilize method. [druzus]
    */
   ::lRefresh := .T.

   RETURN Self


METHOD refreshCurrent() CLASS TBROWSE

   ::setUnstable()

   IF ::nRowPos >= 1 .AND. ::nRowPos <= ::rowCount
      ::aCellStatus[ ::nRowPos ] := .F.
   ENDIF

   RETURN Self


METHOD up() CLASS TBROWSE

   ::setUnstable()
   ::nMoveOffset--

   RETURN Self


METHOD down() CLASS TBROWSE

   ::setUnstable()
   ::nMoveOffset++

   RETURN Self


METHOD pageUp() CLASS TBROWSE

   ::setUnstable()
   ::nMoveOffset -= ::rowCount

   RETURN Self


METHOD pageDown() CLASS TBROWSE

   ::setUnstable()
   ::nMoveOffset += ::rowCount

   RETURN Self


METHOD left() CLASS TBROWSE

   ::setUnstable()
   DO WHILE .T.
      ::nColPos--
      IF ::nColPos < 1 .OR. ::nColPos > ::colCount .OR. ;
         ::aColData[ ::nColPos, _TBCI_CELLWIDTH ] != 0
         EXIT
      ENDIF
   ENDDO

   RETURN Self


METHOD right() CLASS TBROWSE

   ::setUnstable()
   DO WHILE .T.
      ::nColPos++
      IF ::nColPos < 1 .OR. ::nColPos > ::colCount .OR. ;
         ::aColData[ ::nColPos, _TBCI_CELLWIDTH ] != 0
         EXIT
      ENDIF
   ENDDO

   RETURN Self


METHOD home() CLASS TBROWSE

   ::setUnstable()
   ::nColPos := iif( ::nLeftVisible < ::nRightVisible, ;
                     ::nLeftVisible, ::nRightVisible )
   RETURN Self


METHOD end() CLASS TBROWSE

   ::setUnstable()
   ::nColPos := ::nRightVisible

   RETURN Self


METHOD panLeft() CLASS TBROWSE

   LOCAL nNewPos

   ::setUnstable()
   nNewPos := _PREVCOLUMN( ::aColData, Min( ::colCount, ::nLeftVisible - 1 ) )

   IF nNewPos != 0 .AND. nNewPos != ::nLeftVisible
      /* It's replicated CA-Cl*pper behavior */
      ::nRightVisible := 0
      ::nLeftVisible := nNewPos
   ENDIF

   RETURN Self


METHOD panRight() CLASS TBROWSE

   LOCAL nNewPos

   ::setUnstable()
   nNewPos := _NEXTCOLUMN( ::aColData, Max( 1, ::nRightVisible + 1 ) )

   IF nNewPos != 0 .AND. nNewPos != ::nRightVisible
      /* It's replicated CA-Cl*pper behavior */
      ::nLeftVisible := 0
      ::nRightVisible := nNewPos
   ENDIF

   RETURN Self


METHOD panHome() CLASS TBROWSE

   ::setUnstable()
   ::nColPos := _NEXTCOLUMN( ::aColData, 1 )

   RETURN Self


METHOD panEnd() CLASS TBROWSE

   ::setUnstable()
   ::nColPos := _PREVCOLUMN( ::aColData, ::colCount )

   RETURN Self


METHOD goTop() CLASS TBROWSE

   ::setUnstable()

   Eval( ::bGoTopBlock )
   /* In CA-Cl*pper goTop() method does not discards
    * record buffer here but only set's flag that the record
    * buffer should be reloaded in stabilize method. [druzus]
    */
   ::lRefresh := .T.
   ::nRowPos := 1
   ::nBufferPos := 1
   ::nMoveOffset := 0
   Eval( ::bSkipBlock, 0 )

   RETURN Self


METHOD goBottom() CLASS TBROWSE

   LOCAL nMoved

   ::setUnstable()

   Eval( ::bGoBottomBlock )
   nMoved := _SKIP_RESULT( Eval( ::bSkipBlock, -( ::rowCount - 1 ) ) )
   /* In CA-Cl*pper goBottom() method does not discards
    * record buffer here but only set's flag that the record
    * buffer should be reloaded in stabilize method. [druzus]
    */
   ::lRefresh := .T.
   ::nRowPos := 1
   ::nBufferPos := 1
   ::nMoveOffset := -nMoved
   Eval( ::bSkipBlock, 0 )

   RETURN Self


METHOD configure( nMode ) CLASS TBROWSE

   /* method configure() does not touch the screen contents or
    * cursor position. In CA-Cl*pper it only sets flag indicating
    * that some internal data should be recalculated before
    * accessing [druzus]
    */

   IF !ISNUMBER( nMode ) .OR. nMode == 0 .OR. nMode > _TBR_CONF_ALL
      nMode := _TBR_CONF_ALL
   ENDIF
   ::nConfigure := HB_BITOR( ::nConfigure, nMode )

   RETURN Self


METHOD doConfigure() CLASS TBROWSE

   LOCAL oCol
   LOCAL aCol, aVal
   LOCAL nWidth, nHeight, nColCount, nRowCount
   LOCAL xValue
   LOCAL cType
   LOCAL cColSep
   LOCAL cHeadSep, cHeading
   LOCAL nHeadHeight
   LOCAL cFootSep, cFooting
   LOCAL nFootHeight
   LOCAL lHeadSep, lFootSep

   /* TODO: I do not know yet the exact flags behavior (::nConfigure)
    *       and internal conditions so I'll reconfigure all elements.
    *       [druzus]
    */

   ::nConfigure := 0

   /* update color table */
   ::aColors := _DECODECOLORS( ::cColorSpec )

   /* update column data */
   nHeadHeight := nFootHeight := 0
   lHeadSep := lFootSep := .F.
   nColCount := Len( ::columns )
   ASize( ::aColData, nColCount )
   FOR EACH oCol, aCol IN ::columns, ::aColData
      /* CA-Cl*pper always evaluates column block even if column is
       * hidden by setting :width to 0. [druzus]
       */
      xValue := Eval( oCol:block )
      cType  := ValType( xValue )
      nWidth := IIF( cType $ "CMNDTL", ;
                     Len( Transform( xValue, oCol:picture ) ), 0 )
      cColSep := oCol:colSep
      IF cColSep == NIL
         cColSep := ::cColSep
      ENDIF
      cHeadSep := oCol:headSep
      IF !ISCHARACTER( cHeadSep ) .OR. cHeadSep == ""
         cHeadSep := ::cHeadSep
         IF !ISCHARACTER( cHeadSep )
            cHeadSep := ""
         ENDIF
      ENDIF
      cFootSep := oCol:footSep
      IF !ISCHARACTER( cFootSep ) .OR. cFootSep == ""
         cFootSep := ::cFootSep
         IF !ISCHARACTER( cFootSep )
            cFootSep := ""
         ENDIF
      ENDIF
      aCol := Array( _TBCI_SIZE )
      aCol[ _TBCI_COLOBJECT   ] := oCol
      aCol[ _TBCI_COLWIDTH    ] := nWidth
      aCol[ _TBCI_COLPOS      ] := NIL
      aCol[ _TBCI_CELLWIDTH   ] := nWidth
      aCol[ _TBCI_CELLPOS     ] := 0
      aCol[ _TBCI_COLSEP      ] := cColSep
      aCol[ _TBCI_SEPWIDTH    ] := Len( cColSep )
      aCol[ _TBCI_HEADSEP     ] := cHeadSep
      aCol[ _TBCI_FOOTSEP     ] := cFootSep
      aCol[ _TBCI_DEFCOLOR    ] := _COLDEFCOLORS( oCol:defColor, Len( ::aColors ) )
      aCol[ _TBCI_FROZENSPACE ] := 0
      aCol[ _TBCI_LASTSPACE   ] := 0
      IF Len( cHeadSep ) > 0
         lHeadSep := .T.
      ENDIF
      IF Len( cFootSep ) > 0
         lFootSep := .T.
      ENDIF
      cHeading := oCol:heading
      IF _DECODE_FH( @cHeading, @nHeight, @nWidth )
         aCol[ _TBCI_COLWIDTH ] := Max( aCol[ _TBCI_COLWIDTH ], nWidth )
         IF nHeight > nHeadHeight
            nHeadHeight := nHeight
         ENDIF
      ENDIF
      aCol[ _TBCI_HEADING ] := cHeading
      cFooting := oCol:footing
      IF _DECODE_FH( @cFooting, @nHeight, @nWidth )
         aCol[ _TBCI_COLWIDTH ] := Max( aCol[ _TBCI_COLWIDTH ], nWidth )
         IF nHeight > nFootHeight
            nFootHeight := nHeight
         ENDIF
      ENDIF
      aCol[ _TBCI_FOOTING ] := cFooting
      nWidth := oCol:width
      IF nWidth != NIL
         IF nWidth > 0
            aCol[ _TBCI_COLWIDTH ] := nWidth
            IF nWidth < aCol[ _TBCI_CELLWIDTH ] .OR. cType == "C"
               aCol[ _TBCI_CELLWIDTH ] := nWidth
            ENDIF
         ELSE
            aCol[ _TBCI_CELLWIDTH ] := 0
         ENDIF
      ENDIF
      IF aCol[ _TBCI_CELLWIDTH ] > 0
         IF aCol[ _TBCI_COLWIDTH ] > aCol[ _TBCI_CELLWIDTH ]
            IF cType == "N"
               aCol[ _TBCI_CELLPOS ] := aCol[ _TBCI_COLWIDTH ] - aCol[ _TBCI_CELLWIDTH ]
            ELSEIF cType == "L"
               aCol[ _TBCI_CELLPOS ] := Int( ( aCol[ _TBCI_COLWIDTH ] - aCol[ _TBCI_CELLWIDTH ] ) / 2 )
            ENDIF
         ENDIF
#ifdef HB_CLP_STRICT
         /* This is bug in CA-Cl*pper TBrowse. It causes that column
          * is not well centered when picture increase the field size
          * it also has other bad side effects in Clipper. :hiLite()
          * method does not check for the cell size and shows the whole
          * formatted string starting from the middle of column. When
          * string is long enough it causes buffer overflow and other
          * TBrowse data becomes corrupted. I do not want to replicate
          * it. [druzus]
          */
         IF cType == "L"
            aCol[ _TBCI_CELLPOS ] := Int( aCol[ _TBCI_COLWIDTH ] / 2 )
         ENDIF
#endif
      ENDIF
   NEXT

   nHeight := Max( _TBR_COORD( ::n_Bottom ) - _TBR_COORD( ::n_Top ), 0 )
   IF lHeadSep .AND. nHeight > 0
      --nHeight
   ELSE
      lHeadSep := .F.
   ENDIF
   IF lFootSep .AND. nHeight > 0
      --nHeight
   ELSE
      lFootSep := .F.
   ENDIF
   IF nHeadHeight >= nHeight
      nHeadHeight := nHeight
      nHeight := 0
   ELSE
      nHeight -= nHeadHeight
   ENDIF
   IF nFootHeight >= nHeight
      nFootHeight := nHeight
      nHeight := 0
   ENDIF
   ::lHeadSep := lHeadSep
   ::nHeadHeight := nHeadHeight
   ::nFootHeight := nFootHeight
   ::lFootSep := lFootSep

   /* update headings to maximum size and missing head/foot separators */
   FOR EACH aCol IN ::aColData
      aCol[ _TBCI_HEADING ] := Replicate( _TBR_CHR_LINEDELIMITER, nHeadHeight - hb_TokenCount( aCol[ _TBCI_HEADING ], _TBR_CHR_LINEDELIMITER ) ) + ;
                               aCol[ _TBCI_HEADING ]
      IF lHeadSep .AND. aCol[ _TBCI_HEADSEP ] == ""
         aCol[ _TBCI_HEADSEP ] := " "
      ENDIF
      IF lFootSep .AND. aCol[ _TBCI_FOOTSEP ] == ""
         aCol[ _TBCI_FOOTSEP ] := " "
      ENDIF
   NEXT

   nRowCount := ::rowCount
   IF nRowCount == 0
      _GENLIMITRTE()
   ENDIF

   /* create new record buffer */
   ASize( ::aCellStatus, nRowCount )
   ASize( ::aDispStatus, nRowCount )
   ASize( ::aCellValues, nRowCount )
   ASize( ::aCellColors, nRowCount )
   AFill( ::aCellStatus, .F. )
   AFill( ::aDispStatus, .T. )
   FOR EACH aVal, aCol IN ::aCellValues, ::aCellColors
      IF aVal == NIL
         aVal := Array( nColCount )
      ELSE
         ASize( aVal, nColCount )
      ENDIF
      IF aCol == NIL
         aCol := Array( nColCount )
      ELSE
         ASize( aCol, nColCount )
      ENDIF
   NEXT

   ::lStable := .F.
   ::lFrames := .T.

   /* Clipper does not set refreshAll flag in Configure */
   /* ::lRefresh := .T. */

   ::nLastRow := nRowCount
   ::nLastScroll := 0

   /* CA-Cl*pper update visible columns here but without
    * colPos repositioning. [druzus]
    */
   _SETVISIBLE( ::aColData, _TBR_COORD( ::n_Right ) - _TBR_COORD( ::n_Left ) + 1, ;
                @::nFrozen, @::nLeftVisible, @::nRightVisible )

   ::nLastPos := 0

   IF ::nRowPos > nRowCount
      ::nRowPos := nRowCount
   ELSEIF ::nRowPos < 1
      ::nRowPos := 1
   ENDIF

   ::nBufferPos := ::nRowPos

   RETURN Self


STATIC PROCEDURE _GENLIMITRTE()

   LOCAL oError := ErrorNew()

   oError:severity    := ES_ERROR
   oError:genCode     := EG_LIMIT
   oError:subSystem   := "TBROWSE"
   oError:subCode     := 0
   oError:description := hb_LangErrMsg( EG_LIMIT )
   oError:canRetry    := .F.
   oError:canDefault  := .F.
   oError:fileName    := ""
   oError:osCode      := 0

   Eval( ErrorBlock(), oError )
   __errInHandler()

   RETURN


/* helper function to take headings and footing data */
STATIC FUNCTION _DECODE_FH( cName, nHeight, nWidth )

   LOCAL i

   nHeight := nWidth := 0
   IF ISCHARACTER( cName )

      IF Len( cName ) > 0
         /* When last character of heading/footing is ';' then CA-Cl*pper
          * does not calculate it as separator
          */
         IF Right( cName, 1 ) == _TBR_CHR_LINEDELIMITER
            cName := Left( cName, Len( cName ) - 1 )
         ENDIF
         nHeight := hb_TokenCount( cName, _TBR_CHR_LINEDELIMITER )
         FOR i := 1 TO nHeight
            nWidth := Max( nWidth, Len( hb_TokenGet( cName, i, _TBR_CHR_LINEDELIMITER ) ) )
         NEXT
      ENDIF

   ELSE
      /* CA-Cl*per bug, it accepts non character values though cannot
       * display them properly
       */
      /* nHeight := 1 */
      cName := ""
   ENDIF

   RETURN nHeight != 0


STATIC FUNCTION _MAXFREEZE( nColumns, aColData, nWidth )
   LOCAL aCol
   LOCAL lFirst
   LOCAL nCol, nColWidth, nTot

   IF nColumns > Len( aColData ) .OR. nColumns < 1
      RETURN 0
   ENDIF

   nTot := nWidth
   lFirst := .T.
   FOR nCol := 1 TO nColumns
      aCol := aColData[ nCol ]
      IF aCol[ _TBCI_CELLWIDTH ] > 0
         nColWidth := aCol[ _TBCI_COLWIDTH ]
         IF lFirst
            lFirst := .F.
         ELSE
            nColWidth += aCol[ _TBCI_SEPWIDTH ]
         ENDIF
         IF ( nWidth -= nColWidth ) < 0
            EXIT
         ENDIF
      ENDIF
   NEXT

   /* CA-Cl*pper allows to freeze all columns only when they
    * are fully visible, otherwise it reserves at least one
    * character for 1-st unfrozen column [druzus]
    */
   IF nWidth > 0 .OR. ;
      nWidth == 0 .AND. _NEXTCOLUMN( aColData, nColumns + 1 ) == 0

      RETURN nColumns
   ENDIF

   nWidth := nTot

   RETURN 0


STATIC FUNCTION _NEXTCOLUMN( aColData, nCol )
   LOCAL aCol

   DO WHILE nCol <= Len( aColData )
      aCol := aColData[ nCol ]
      IF aCol[ _TBCI_CELLWIDTH ] > 0
         RETURN nCol
      ENDIF
      ++nCol
   ENDDO

   RETURN 0


STATIC FUNCTION _PREVCOLUMN( aColData, nCol )
   LOCAL aCol

   DO WHILE nCol >= 1
      aCol := aColData[ nCol ]
      IF aCol[ _TBCI_CELLWIDTH ] > 0
         RETURN nCol
      ENDIF
      --nCol
   ENDDO

   RETURN 0


STATIC FUNCTION _SETCOLUMNS( nFrom, nTo, nStep, aColData, nFirst, nWidth, lFirst )
   LOCAL aCol
   LOCAL nCol, nColWidth
   LOCAL nLast := 0

   IF nWidth > 0
      FOR nCol := nFrom TO nTo STEP nStep
         aCol := aColData[ nCol ]
         IF aCol[ _TBCI_CELLWIDTH ] > 0
            IF nFirst == 0 .OR. nCol == nFirst
               nColWidth := aCol[ _TBCI_COLWIDTH ]
            ELSEIF nCol < nFirst
               nColWidth := aCol[ _TBCI_COLWIDTH ] + aColData[ nFirst ][ _TBCI_SEPWIDTH ]
            ELSE
               nColWidth := aCol[ _TBCI_COLWIDTH ] + aCol[ _TBCI_SEPWIDTH ]
            ENDIF
            IF nWidth >= nColWidth
               nLast := nCol
               nWidth -= nColWidth
               lFirst := .F.
               IF nFirst == 0 .OR. nCol < nFirst
                  nFirst := nCol
               ENDIF
            ELSE
               IF lFirst
                  nLast := nCol
                  nWidth := 0
                  lFirst := .F.
               ENDIF
               EXIT
            ENDIF
         ENDIF
      NEXT
   ENDIF

   RETURN iif( nLast == 0, nFrom - nStep, nLast )


STATIC PROCEDURE _SETVISIBLE( aColData, nWidth, nFrozen, nLeft, nRight )

   LOCAL nPos, nFirst
   LOCAL lLeft, lRight, lFirst
   LOCAL nColCount := Len( aColData )

   /* Check if frozen columns are still valid, if not reset it to 0
    * It also calculates the size left for unfrozen columns [druzus]
    */
   nFrozen := _MAXFREEZE( nFrozen, aColData, @nWidth )

   /* CA-Cl*pper checks here only for columns number and does not check
    * if at least one column is visible (oCol:width > 0) and if not then
    * wrongly calculates visible columns and some internal indexes.
    * Using linkers like EXOSPACE with memory protection it causes
    * application crash with GPF. [druzus]
    */
   IF nColCount == 0 .OR. _NEXTCOLUMN( aColData, 1 ) == 0
      nLeft := nRight := 0
   ELSE
      /* This algorithms keeps CA-Cl*pper precedence in visible column
       * updating. It's also important for proper working panLeft and
       * panRight methods which use leftVisible and rightVisible values
       * for horizontal scrolling just like in CA-Cl*pper. [druzus]
       */
      IF nWidth >= 1
         lRight := nRight > nFrozen .AND. nRight <= nColCount .AND. ;
                   aColData[ nRight ][ _TBCI_CELLWIDTH ] > 0
         lLeft  := nLeft > nFrozen .AND. nLeft <= nColCount .AND. ;
                   aColData[ nLeft ][ _TBCI_CELLWIDTH ] > 0
         IF !lLeft
            IF lRight
               IF ( nLeft := _PREVCOLUMN( aColData, nRight ) ) < nFrozen
                  nLeft := nRight
               ENDIF
            ELSE
               nPos := _NEXTCOLUMN( aColData, Max( nLeft + 1, nFrozen + 1 ) )
               IF nPos == 0
                  nPos := _PREVCOLUMN( aColData, Min( nColCount, nLeft - 1 ) )
               ENDIF
               IF nPos > nFrozen
                  nLeft := nPos
                  lLeft := .T.
               ENDIF
            ENDIF
         ENDIF
         lFirst := .T.
         nFirst := _PREVCOLUMN( aColData, nFrozen )
      ELSE
         lLeft := lRight := .F.
      ENDIF
      IF lLeft
         nRight := _SETCOLUMNS( nLeft, nColCount, 1, aColData, @nFirst, @nWidth, @lFirst )
         nLeft := _SETCOLUMNS( nLeft - 1, nFrozen + 1, -1, aColData, @nFirst, @nWidth, @lFirst )
      ELSEIF lRight
         nLeft := _SETCOLUMNS( nRight, nFrozen + 1, -1, aColData, @nFirst, @nWidth, @lFirst )
         nRight := _SETCOLUMNS( nRight + 1, nColCount, 1, aColData, @nFirst, @nWidth, @lFirst )
      ELSE
         nLeft := nFrozen + 1
         nRight := nFrozen
      ENDIF
   ENDIF

   RETURN


/* set visible columns */
METHOD setVisible() CLASS TBROWSE

   LOCAL nCol, nLeft, nFrozen, nLast, nColumns, nWidth, nColPos
   LOCAL lFirst, lFrames
   LOCAL aCol

   nColPos := ::nColPos
   IF nColPos < 1 .OR. nColPos > ::colCount .OR. ::nLastPos != nColPos .OR. ;
      ::lFrames .OR. ::nLeftVisible == 0 .OR. ::nRightVisible == 0 .OR. ;
      ::aColData[ nColPos ][ _TBCI_COLPOS ] == NIL

      lFrames := .F.
      nWidth := _TBR_COORD( ::n_Right ) - _TBR_COORD( ::n_Left ) + 1
      nColumns := Len( ::aColData )

      IF nColPos > nColumns
         ::nColPos := nColumns
         ::nLeftVisible := nColumns
         ::nRightVisible := nColumns
      ELSEIF ::nColPos < 1
         ::nColPos := 1
         ::nLeftVisible := 1
         ::nRightVisible := 1
      ELSEIF nColPos != ::nLastPos
         IF nColPos > ::nRightVisible
            ::nRightVisible := ::nColPos
            ::nLeftVisible := 0
         ELSEIF nColPos < ::nLeftVisible
            ::nLeftVisible := ::nColPos
            ::nRightVisible := 0
         ENDIF
      ELSEIF ::nColPos <= ::nFrozen .AND. ::nLeftVisible == 0
         nCol := _NEXTCOLUMN( ::aColData, ::nFrozen + 1 )
         ::nColPos := iif( nCol == 0, nColumns, nCol )
      ENDIF

      _SETVISIBLE( ::aColData, @nWidth, ;
                   @::nFrozen, @::nLeftVisible, @::nRightVisible )

      IF ::nColPos > ::nRightVisible
         ::nColPos := ::nRightVisible
      ELSEIF ::nColPos > ::nFrozen .AND. ::nColPos < ::nLeftVisible
         ::nColPos := ::nLeftVisible
      ENDIF

#if 0
      /* Always try to locate visible column.
       * CA-Cl*pper does not have such condition. [druzus]
       */
      IF ::nColPos >= 1 .AND. ::aColData[ ::nColPos ][ _TBCI_CELLWIDTH ] <= 0
         nCol := _PREVCOLUMN( ::aColData, ::nColPos - 1 )
         ::nColPos := iif( nCol == 0, ;
                           _NEXTCOLUMN( ::aColData, ::nColPos + 1 ), nCol )
      ENDIF
#endif

      /* update column size and positions on the screen */
      nLeft := _TBR_COORD( ::n_Left )
      lFirst := .T.
      FOR nCol := 1 TO ::nRightVisible
         aCol := ::aColData[ nCol ]
         IF aCol[ _TBCI_CELLWIDTH ] > 0 .AND. ;
            ( nCol <= ::nFrozen .OR. nCol >= ::nLeftVisible )

            nFrozen := iif( nCol == ::nLeftVisible, Int( nWidth / 2 ), 0 )
            nColPos := nLeft += nFrozen
            nLeft += aCol[ _TBCI_COLWIDTH ]
            IF lFirst
               lFirst := .F.
            ELSE
               nLeft += aCol[ _TBCI_SEPWIDTH ]
            ENDIF
            nLast := iif( nCol == ::nRightVisible, ;
                          _TBR_COORD( ::n_Right ) - nLeft + 1, 0 )

            IF aCol[ _TBCI_COLPOS      ] != nColPos  .OR. ;
               aCol[ _TBCI_FROZENSPACE ] != nFrozen  .OR. ;
               aCol[ _TBCI_LASTSPACE   ] != nLast

               lFrames := .T.
               aCol[ _TBCI_COLPOS      ] := nColPos
               aCol[ _TBCI_FROZENSPACE ] := nFrozen
               aCol[ _TBCI_LASTSPACE   ] := nLast
            ENDIF
         ELSE
            IF aCol[ _TBCI_COLPOS ] != NIL
               lFrames := .T.
            ENDIF
            aCol[ _TBCI_COLPOS ] := NIL
         ENDIF
      NEXT
      FOR nCol := ::nRightVisible + 1 TO nColumns
         aCol := ::aColData[ nCol ]
         IF aCol[ _TBCI_COLPOS ] != NIL
            lFrames := .T.
         ENDIF
         aCol[ _TBCI_COLPOS ] := NIL
      NEXT

      ::nLastPos := ::nColPos

      IF lFrames
         ::lFrames := .T.
      ENDIF

   ENDIF

   RETURN Self


METHOD hiLite() CLASS TBROWSE

   LOCAL cValue, cColor

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   DispBegin()

   IF ::setCursorPos()
      IF ( cValue := ::cellValue( ::nRowPos, ::nColPos ) ) != NIL
         cColor := ::colorValue( ::cellColor( ::nRowPos, ::nColPos )[ _TBC_CLR_SELECTED ] )
         IF ::n_Col + Len( cValue ) > _TBR_COORD( ::n_Right )
            cValue := Left( cValue, _TBR_COORD( ::n_Right ) - ::n_Col + 1 )
         ENDIF
         hb_dispOutAt( ::n_Row, ::n_Col, cValue, cColor )
         SetPos( ::n_Row, ::n_Col )
         ::lHiLited := .T.
      ENDIF
   ENDIF

   DispEnd()

   RETURN Self


METHOD deHilite() CLASS TBROWSE

   LOCAL cValue, cColor

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   DispBegin()

   IF ::setCursorPos()
      IF ( cValue := ::cellValue( ::nRowPos, ::nColPos ) ) != NIL
         cColor := ::colorValue( ::cellColor( ::nRowPos, ::nColPos )[ _TBC_CLR_STANDARD ] )
         IF ::n_Col + Len( cValue ) > _TBR_COORD( ::n_Right )
            cValue := Left( cValue, _TBR_COORD( ::n_Right ) - ::n_Col + 1 )
         ENDIF
         hb_dispOutAt( ::n_Row, ::n_Col, cValue, cColor )
         SetPos( ::n_Row, ::n_Col )
      ENDIF
   ENDIF
   ::lHiLited := .F.

   DispEnd()

   RETURN Self


/* Returns the display width of a particular column */
METHOD colWidth( nColumn ) CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   IF ISNUMBER( nColumn ) .AND. nColumn >= 1 .AND. nColumn <= ::colCount
      RETURN ::aColData[ nColumn ][ _TBCI_COLWIDTH ]
   ENDIF

   RETURN 0


/* get number of frozen columns */
METHOD getFrozen() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::nFrozen


/* set number of columns to freeze */
METHOD freeze( nColumns ) CLASS TBROWSE

   LOCAL nCols

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   IF ISNUMBER( nColumns )

      nCols := Int( nColumns )
      IF _MAXFREEZE( nCols, ::aColData, _TBR_COORD( ::n_Right ) - _TBR_COORD( ::n_Left ) + 1 ) == nCols

         ::nFrozen := nCols
         ::lFrames := .T.
         ::nLastPos := 0
         /* CA-Cl*pper update visible columns here but without
          * colPos repositioning. [druzus]
          */
         _SETVISIBLE( ::aColData, _TBR_COORD( ::n_Right ) - _TBR_COORD( ::n_Left ) + 1, ;
                      @::nFrozen, @::nLeftVisible, @::nRightVisible )
      ENDIF
      /* NOTE: CA-Cl*pper compatible behaviour. [vszakats] */
      RETURN nCols
   ENDIF

   RETURN ::nFrozen


/* get/set string value with color table for the TBrowse display */
METHOD colorSpec( cColorSpec ) CLASS TBROWSE

   IF cColorSpec != NIL
      ::cColorSpec := __eInstVar53( Self, "COLORSPEC", cColorSpec, "C", 1001 )
      ::configure( _TBR_CONF_COLORS )
   ENDIF

   RETURN ::cColorSpec


METHOD colCount() CLASS TBROWSE

   RETURN Len( ::columns )


METHOD rowCount() CLASS TBROWSE

   LOCAL nRows

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   nRows := _TBR_COORD( ::n_Bottom ) - _TBR_COORD( ::n_Top ) + 1 - ;
            ::nHeadHeight - iif( ::lHeadSep, 1, 0 ) - ;
            ::nFootHeight - iif( ::lFootSep, 1, 0 )

   RETURN iif( nRows > 0, nRows, 0 )


/* NOTE: CA-Cl*pper has a bug where negative nRowPos value will be translated
         to 16bit unsigned int, so the behaviour will be different in this case.
         [vszakats] */
METHOD setRowPos( nRowPos ) CLASS TBROWSE

   LOCAL nRow
   LOCAL nRowCount := ::rowCount    /* executes doConfigure internally */

   IF ISNUMBER( nRowPos )
      nRow := Int( nRowPos )
      ::nRowPos := iif( nRow > nRowCount, nRowCount, ;
                     iif( nRow < 1, 1, nRow ) )
      RETURN nRow
   ELSE
      ::nRowPos := Min( nRowCount, 1 )
      RETURN 0
   ENDIF

   RETURN ::nRowPos


METHOD getRowPos() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::nRowPos


/* NOTE: CA-Cl*pper has a bug where negative nRowPos value will be translated
         to 16bit unsigned int, so the behaviour will be different in this case.
         [vszakats] */
METHOD setColPos( nColPos ) CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   IF ISNUMBER( nColPos )
      ::nColPos := nColPos
   ELSE
      ::nColPos := 0
   ENDIF

   RETURN ::nColPos


METHOD getColPos() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::nColPos


METHOD getTopFlag() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::lHitTop


METHOD setTopFlag( lTop ) CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   IF !ISLOGICAL( lTop )
      RETURN .T.
   ENDIF

   ::lHitTop := lTop

   RETURN lTop


METHOD getBottomFlag() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::lHitBottom


METHOD setBottomFlag( lBottom ) CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   IF !ISLOGICAL( lBottom )
      RETURN .T.
   ENDIF

   ::lHitBottom := lBottom

   RETURN lBottom


METHOD getAutoLite() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::lAutoLite


METHOD setAutoLite( lAutoLite ) CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   IF !ISLOGICAL( lAutoLite )
      RETURN .T.
   ENDIF

   ::lAutoLite := lAutoLite

   RETURN lAutoLite


METHOD getStableFlag() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::lStable


METHOD setStableFlag( lStable ) CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   IF !ISLOGICAL( lStable )
      RETURN .T.
   ENDIF

   ::lStable := lStable

   RETURN lStable


METHOD leftVisible() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::nLeftVisible


METHOD rightVisible() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::nRightVisible


/* Adds a TBColumn object to the TBrowse object */
METHOD addColumn( oCol ) CLASS TBROWSE

   /* NOTE: CA-Cl*pper doesn't check the parameters. */

   AAdd( ::columns, oCol )
   ::configure( _TBR_CONF_COLUMNS )

   RETURN Self


/* Delete a column object from a browse */
METHOD delColumn( nColumn ) CLASS TBROWSE

   LOCAL oCol

   /* NOTE: CA-Cl*pper doesn't check the parameters. */
#ifndef HB_CLP_STRICT
   IF nColumn >= 1 .AND. nColumn <= ::colCount
#else
   IF .T.      /* It's optimized by compiler without any RT overhead */
#endif
      oCol := ::columns[ nColumn ]
      ADel( ::columns, nColumn )
      ASize( ::columns, Len( ::columns ) - 1 )
      ::configure( _TBR_CONF_COLUMNS )
   ENDIF

   RETURN oCol


/* Insert a column object in a browse */
METHOD insColumn( nColumn, oCol ) CLASS TBROWSE

   /* NOTE: CA-Cl*pper doesn't check the parameters. */
#ifndef HB_CLP_STRICT
   IF nColumn >= 1 .AND. nColumn <= ::colCount + 1
#else
   IF .T.      /* It's optimized by compiler without any RT overhead */
#endif
      HB_AIns( ::columns, nColumn, oCol, .T. )
      ::configure( _TBR_CONF_COLUMNS )
   ENDIF

   RETURN oCol


/* Replaces one TBColumn object with another */
METHOD setColumn( nColumn, oCol ) CLASS TBROWSE

   LOCAL oPrevCol

   IF nColumn != NIL .AND. oCol != NIL

      nColumn := __eInstVar53( Self, "COLUMN", nColumn, "N", 1001 )
      oCol := __eInstVar53( Self, "COLUMN", oCol, "O", 1001 )

      /* NOTE: CA-Cl*pper doesn't check nColumn range (and type in C5.3 - I didn't implement this behaviour),
               but crashes instead. */

#ifndef HB_CLP_STRICT
      IF nColumn >= 1 .AND. nColumn <= ::colCount
#else
      IF .T.      /* It's optimized by compiler without any RT overhead */
#endif
         oPrevCol := ::columns[ nColumn ]
         ::columns[ nColumn ] := oCol
         ::configure( _TBR_CONF_COLUMNS )
      ENDIF
   ENDIF

   /* NOTE: CA-Cl*pper 5.2 NG says this will return the previously set
            column, but it's returning Self instead. In C5.3 this bug
            was fixed and it works as expected (except when wrong
            parameter is passed, when it returns NIL). [vszakats] */
#ifdef HB_CLP_STRICT
   RETURN Self
#else
   RETURN oPrevCol
#endif


/* Gets a specific TBColumn object */
METHOD getColumn( nColumn ) CLASS TBROWSE

#ifdef HB_CLP_STRICT
   RETURN ::columns[ nColumn ]
#else
   RETURN iif( nColumn >= 1 .AND. nColumn <= ::colCount, ::columns[ nColumn ], NIL )
#endif


METHOD footSep( cFootSep ) CLASS TBROWSE

   IF cFootSep != NIL
      ::cFootSep := __eInstVar53( Self, "FOOTSEP", cFootSep, "C", 1001 )
   ENDIF

   RETURN ::cFootSep


METHOD colSep( cColSep ) CLASS TBROWSE

   IF cColSep != NIL
      ::cColSep := __eInstVar53( Self, "COLSEP", cColSep, "C", 1001 )
   ENDIF

   RETURN ::cColSep


METHOD headSep( cHeadSep ) CLASS TBROWSE

   IF cHeadSep != NIL
      ::cHeadSep := __eInstVar53( Self, "HEADSEP", cHeadSep, "C", 1001 )
   ENDIF

   RETURN ::cHeadSep


METHOD skipBlock( bSkipBlock ) CLASS TBROWSE

   IF bSkipBlock != NIL
      ::bSkipBlock := __eInstVar53( Self, "SKIPBLOCK", bSkipBlock, "B", 1001 )
   ENDIF

   RETURN ::bSkipBlock


METHOD goTopBlock( bBlock ) CLASS TBROWSE

   IF bBlock != NIL
      ::bGoTopBlock := __eInstVar53( Self, "GOTOPBLOCK", bBlock, "B", 1001 )
   ENDIF

   RETURN ::bGoTopBlock


METHOD goBottomBlock( bBlock ) CLASS TBROWSE

   IF bBlock != NIL
      /* NOTE: In CA-Cl*pper the string is: "GOBOTTOMBL" */
      ::bGoBottomBlock := __eInstVar53( Self, "GOBOTTOMBLOCK", bBlock, "B", 1001 )
   ENDIF

   RETURN ::bGoBottomBlock


METHOD nTop( nTop ) CLASS TBROWSE

   IF nTop != NIL
      #ifdef HB_COMPAT_C53
         ::n_Top := __eInstVar53( Self, "NTOP", nTop, "N", 1001 )
         IF !Empty( ::cBorder )
            ::n_Top++
         ENDIF
      #else
         ::n_Top := __eInstVar53( Self, "NTOP", nTop, "N", 1001, {| o, x | HB_SYMBOL_UNUSED( o ), x >= 0 } )
      #endif
      ::configure( _TBR_CONF_COLUMNS )
   ENDIF

   #ifdef HB_COMPAT_C53
      IF !Empty( ::cBorder )
         RETURN ::n_Top - 1
      ENDIF
   #endif

   RETURN ::n_Top


METHOD nLeft( nLeft ) CLASS TBROWSE

   IF nLeft != NIL
      #ifdef HB_COMPAT_C53
         ::n_Left := __eInstVar53( Self, "NLEFT", nLeft, "N", 1001 )
         IF !Empty( ::cBorder )
            ::n_Left++
         ENDIF
      #else
         ::n_Left := __eInstVar53( Self, "NLEFT", nLeft, "N", 1001, {| o, x | HB_SYMBOL_UNUSED( o ), x >= 0 } )
      #endif
      ::configure( _TBR_CONF_COLUMNS )
   ENDIF

   #ifdef HB_COMPAT_C53
      IF !Empty( ::cBorder )
         RETURN ::n_Left - 1
      ENDIF
   #endif

   RETURN ::n_Left


METHOD nBottom( nBottom ) CLASS TBROWSE

   IF nBottom != NIL
      ::n_Bottom := __eInstVar53( Self, "NBOTTOM", nBottom, "N", 1001, {| o, x | x >= o:nTop } )
      #ifdef HB_COMPAT_C53
         IF !Empty( ::cBorder )
            ::n_Bottom--
         ENDIF
      #endif
      ::configure( _TBR_CONF_COLUMNS )
   ENDIF

   #ifdef HB_COMPAT_C53
      IF !Empty( ::cBorder )
         RETURN ::n_Bottom + 1
      ENDIF
   #endif

   RETURN ::n_Bottom


METHOD nRight( nRight ) CLASS TBROWSE

   IF nRight != NIL
      ::n_Right := __eInstVar53( Self, "NRIGHT", nRight, "N", 1001, {| o, x | x >= o:nLeft } )
      #ifdef HB_COMPAT_C53
         IF !Empty( ::cBorder )
            ::n_Right--
         ENDIF
      #endif
      ::configure( _TBR_CONF_COLUMNS )
   ENDIF

   #ifdef HB_COMPAT_C53
      IF !Empty( ::cBorder )
         RETURN ::n_Right + 1
      ENDIF
   #endif

   RETURN ::n_Right

#ifdef HB_COMPAT_C53
METHOD nRow() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::n_Row


METHOD nCol() CLASS TBROWSE

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   RETURN ::n_Col


METHOD hitTest( mRow, mCol ) CLASS TBROWSE

   LOCAL nTop, nLeft, nBottom, nRight, nRet, nCol
   LOCAL lFirst
   LOCAL aCol

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

#ifdef HB_BRW_STATICMOUSE
   /* This is not CA-Cl*pper compatible, in Clipper ::mRowPos and ::mColPos
    * is calculated dynamically by call to MCOL() and MROW()
    */
   ::mRowPos := ::mColPos := 0
#endif

   IF !ISNUMBER( mRow ) .OR. !ISNUMBER( mCol ) .OR. ;
      mRow < ( nTop    := _TBR_COORD( ::n_Top    ) ) .OR. ;
      mRow > ( nBottom := _TBR_COORD( ::n_Bottom ) ) .OR. ;
      mCol < ( nLeft   := _TBR_COORD( ::n_Left   ) ) .OR. ;
      mCol > ( nRight  := _TBR_COORD( ::n_Right  ) )
      RETURN HTNOWHERE
   ENDIF

   nRet := HTNOWHERE

   IF !Empty( ::cBorder )
      IF mRow == nTop - 1
         IF mCol == nLeft - 1
            nRet := HTTOPLEFT
         ELSEIF mCol == nRight + 1
            nRet := HTTOPRIGHT
         ELSE
            nRet := HTTOP
         ENDIF
      ELSEIF mRow == nBottom + 1
         IF mCol == nLeft - 1
            nRet := HTBOTTOMLEFT
         ELSEIF mCol == nRight + 1
            nRet := HTBOTTOMRIGHT
         ELSE
            nRet := HTBOTTOM
         ENDIF
      ELSEIF mCol == nLeft - 1
         nRet := HTLEFT
      ELSEIF mCol == nRight + 1
         nRet := HTRIGHT
      ENDIF
   ENDIF

   IF nRet == HTNOWHERE
      IF mRow < nTop + ::nHeadHeight
         nRet := HTHEADING
      ELSEIF ::lHeadSep .AND. mRow == nTop + ::nHeadHeight
         nRet := HTHEADSEP
      ELSEIF ::lFootSep .AND. mRow == nBottom - ::nFootHeight
         nRet := HTFOOTSEP
      ELSEIF mRow > nBottom - ::nFootHeight
         nRet := HTFOOTING
      ELSE
         nRet := HTCELL
#ifdef HB_BRW_STATICMOUSE
         ::mRowPos := mRow - nTop - ::nHeadHeight - iif( ::lHeadSep, 1, 0 )
#endif
         lFirst := .T.
         nCol := 1
         DO WHILE nCol <= ::nRightVisible
            aCol := ::aColData[ nCol ]
            IF aCol[ _TBCI_COLPOS ] != NIL
               IF lFirst
                  lFirst := .F.
               ELSE
                  /* NOTE: CA-Cl*pper has bug here, it takes the size of
                   *       next column separator instead of the current one
                   */
                  IF ( nLeft += aCol[ _TBCI_SEPWIDTH ] ) > mCol
                     nRet := HTCOLSEP
                     EXIT
                  ENDIF
               ENDIF
#ifdef HB_BRW_STATICMOUSE
               ::mColPos := nCol
#endif
               IF ( nLeft += aCol[ _TBCI_COLWIDTH ] + ;
                             aCol[ _TBCI_FROZENSPACE ] + ;
                             aCol[ _TBCI_LASTSPACE ] ) > mCol
                  EXIT
               ENDIF
            ENDIF
            IF nCol == ::nFrozen .AND. nCol < ::nLeftVisible
               nCol := ::nLeftVisible
            ELSE
               nCol++
            ENDIF
         ENDDO
      ENDIF
   ENDIF

   RETURN nRet


#ifndef HB_BRW_STATICMOUSE
STATIC PROCEDURE _mBrwPos( oBrw, mRow, mCol )

   LOCAL nTop, nLeft, nBottom, nPos, nCol, aCol

   mRow := MRow()
   mCol := MCol()

   IF mRow >= ( nTop    := _TBR_COORD( oBrw:n_Top    ) ) .AND. ;
      mRow <= ( nBottom := _TBR_COORD( oBrw:n_Bottom ) ) .AND. ;
      mCol >= ( nLeft   := _TBR_COORD( oBrw:n_Left   ) ) .AND. ;
      mCol <= (            _TBR_COORD( oBrw:n_Right  ) )

      IF mRow < nTop + oBrw:nHeadHeight + iif( oBrw:lHeadSep, 1, 0 ) .OR. ;
         mRow > nBottom - oBrw:nFootHeight - iif( oBrw:lFootSep, 1, 0 )
         mRow := 0
      ELSE
         mRow -= nTop + oBrw:nHeadHeight - iif( oBrw:lHeadSep, 0, 1 )
      ENDIF

      nPos := 0
      nCol := 1
      DO WHILE nCol <= oBrw:nRightVisible
         aCol := oBrw:aColData[ nCol ]
         IF aCol[ _TBCI_COLPOS ] != NIL
            IF nPos != 0
               IF ( nLeft += aCol[ _TBCI_SEPWIDTH ] ) > mCol
                  EXIT
               ENDIF
            ENDIF
            nPos := nCol
            IF ( nLeft += aCol[ _TBCI_COLWIDTH ] + ;
                          aCol[ _TBCI_FROZENSPACE ] + ;
                          aCol[ _TBCI_LASTSPACE ] ) > mCol
               EXIT
            ENDIF
         ENDIF
         IF nCol == oBrw:nFrozen .AND. nCol < oBrw:nLeftVisible
            nCol := oBrw:nLeftVisible
         ELSE
            nCol++
         ENDIF
      ENDDO
      mCol := nPos
      IF nPos == 0
         mRow := 0
      ENDIF
   ELSE
      mRow := mCol := 0
   ENDIF

   RETURN


METHOD mRowPos() CLASS TBROWSE

   LOCAL mRow, mCol

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   _mBrwPos( self, @mRow, @mCol )

   RETURN mRow


METHOD mColPos() CLASS TBROWSE
   LOCAL mRow, mCol

   IF ::nConfigure != 0
      ::doConfigure()
   ENDIF

   _mBrwPos( self, @mRow, @mCol )

   RETURN mCol
#endif


METHOD border( cBorder ) CLASS TBROWSE

   IF cBorder != NIL

      cBorder := __eInstVar53( Self, "BORDER", cBorder, "C", 1001 )

      IF Len( cBorder ) == 0 .OR. Len( cBorder ) == 8

         IF Empty( ::cBorder ) .AND. !Empty( cBorder )
            ::n_Top++
            ::n_Left++
            ::n_Bottom--
            ::n_Right--
            ::configure( _TBR_CONF_COLUMNS )
         ELSEIF !Empty( ::cBorder ) .AND. Empty( cBorder )
            ::n_Top--
            ::n_Left--
            ::n_Bottom++
            ::n_Right++
            ::configure( _TBR_CONF_COLUMNS )
         ENDIF

         ::cBorder := cBorder
      ENDIF
   ENDIF

   RETURN ::cBorder


METHOD message( cMessage ) CLASS TBROWSE

   IF cMessage != NIL
      ::cMessage := __eInstVar53( Self, "MESSAGE", cMessage, "C", 1001 )
   ENDIF

   RETURN ::cMessage


METHOD applyKey( nKey ) CLASS TBROWSE

   LOCAL bBlock := ::SetKey( nKey )

   IF bBlock == NIL
      bBlock := ::SetKey( 0 )

      IF bBlock == NIL
         RETURN TBR_EXCEPTION
      ENDIF
   ENDIF

   RETURN Eval( bBlock, Self, nKey )


METHOD setKey( nKey, bBlock ) CLASS TBROWSE

   LOCAL bReturn
   LOCAL nPos

   /* NOTE: Assigned codeblock receives two parameters:
            {| oTBrowse, nKey | <action> } */

   IF ::keys == NIL
      ::keys := { { K_DOWN       , {| o | o:Down()    , TBR_CONTINUE   } },;
                  { K_END        , {| o | o:End()     , TBR_CONTINUE   } },;
                  { K_CTRL_PGDN  , {| o | o:GoBottom(), TBR_CONTINUE   } },;
                  { K_CTRL_PGUP  , {| o | o:GoTop()   , TBR_CONTINUE   } },;
                  { K_HOME       , {| o | o:Home()    , TBR_CONTINUE   } },;
                  { K_LEFT       , {| o | o:Left()    , TBR_CONTINUE   } },;
                  { K_PGDN       , {| o | o:PageDown(), TBR_CONTINUE   } },;
                  { K_PGUP       , {| o | o:PageUp()  , TBR_CONTINUE   } },;
                  { K_CTRL_END   , {| o | o:PanEnd()  , TBR_CONTINUE   } },;
                  { K_CTRL_HOME  , {| o | o:PanHome() , TBR_CONTINUE   } },;
                  { K_CTRL_LEFT  , {| o | o:PanLeft() , TBR_CONTINUE   } },;
                  { K_CTRL_RIGHT , {| o | o:PanRight(), TBR_CONTINUE   } },;
                  { K_RIGHT      , {| o | o:Right()   , TBR_CONTINUE   } },;
                  { K_UP         , {| o | o:Up()      , TBR_CONTINUE   } },;
                  { K_ESC        , {|   |               TBR_EXIT       } },;
                  { K_LBUTTONDOWN, {| o | TBMouse( o, MRow(), MCol() ) } } }

      #ifndef HB_CLP_STRICT
         AAdd( ::keys, { K_MWFORWARD  , {| o | o:Up()      , TBR_CONTINUE   } } )
         AAdd( ::keys, { K_MWBACKWARD , {| o | o:Down()    , TBR_CONTINUE   } } )
      #endif
   ENDIF

   IF ( nPos := AScan( ::keys, {| x | x[ _TBC_SETKEY_KEY ] == nKey } ) ) == 0
      IF ISBLOCK( bBlock )
         AAdd( ::keys, { nKey, bBlock } )
      ENDIF
      bReturn := bBlock
   ELSEIF ISBLOCK( bBlock )
      ::keys[ nPos ][ _TBC_SETKEY_BLOCK ] := bBlock
      bReturn := bBlock
   ELSEIF PCount() == 1
      bReturn := ::keys[ nPos ][ _TBC_SETKEY_BLOCK ]
   ELSE
      bReturn := ::keys[ nPos ][ _TBC_SETKEY_BLOCK ]
      IF PCount() == 2 .AND. bBlock == NIL .AND. nKey != 0
         ADel( ::keys, nPos )
         ASize( ::keys, Len( ::keys ) - 1 )
      ENDIF
   ENDIF

   RETURN bReturn


METHOD setStyle( nStyle, lNewValue ) CLASS TBROWSE

   /* NOTE: CA-Cl*pper 5.3 will initialize this var on the first
            :setStyle() method call. [vszakats] */

   DEFAULT ::styles TO { .F., .F., .F., .F., .F., NIL }

   /* NOTE: CA-Cl*pper 5.3 does no checks on the value of nStyle, so in case
            it is zero or non-numeric, a regular RTE will happen. [vszakats] */

   IF nStyle > Len( ::styles ) .AND. ;
      nStyle <= 4096 /* some reasonable limit for maximum number of styles */
      ASize( ::styles, nStyle )
   ENDIF

   IF ISLOGICAL( lNewValue )
      ::styles[ nStyle ] := lNewValue
   ENDIF

   RETURN ::styles[ nStyle ]


FUNCTION TBMouse( oBrw, nMRow, nMCol )

   LOCAL n

   IF oBrw:hitTest( nMRow, nMCol ) == HTCELL

      IF ( n := oBrw:mRowPos - oBrw:rowPos ) < 0
         DO WHILE ++n <= 0
            oBrw:up()
         ENDDO
      ELSEIF n > 0
         DO WHILE --n >= 0
            oBrw:down()
         ENDDO
      ENDIF

      IF ( n := oBrw:mColPos - oBrw:colPos ) < 0
         DO WHILE ++n <= 0
            oBrw:left()
         ENDDO
      ELSEIF n > 0
         DO WHILE --n >= 0
            oBrw:right()
         ENDDO
      ENDIF

      RETURN TBR_CONTINUE
   ENDIF

   RETURN TBR_EXCEPTION
#endif
