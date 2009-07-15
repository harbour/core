/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://www.harbour-project.org
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                    Xbase++ Compatible XbpRtf Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              10Jul2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "gra.ch"
#include "appevent.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS XbpBrowse INHERIT XbpWindow

   METHOD   init()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()
   METHOD   exeBlock()

   DATA     cursorMode                            INIT      XBPBRW_CURSOR_CELL
   DATA     hScroll                               INIT      .T.
   DATA     sizeCols                              INIT      .T.
   DATA     softTrack                             INIT      .T.
   DATA     vScroll                               INIT      .T.

   DATA     colCount
   DATA     colPos
   DATA     rowCount
   DATA     rowPos

   DATA     firstPosBlock
   DATA     goBottomBlock
   DATA     goPosBlock
   DATA     goTopBlock
   DATA     hitBottomBlock
   DATA     hitTopBlock
   DATA     lastPosBlock
   DATA     phyPosBlock
   DATA     posBlock
   DATA     skipBlock
   DATA     stableBlock

   METHOD   addColumn()
   METHOD   delColumn( nColPos )
   METHOD   getColumn( nColPos )
   METHOD   insColumn( nColPos, oXbpColumn )
   METHOD   setColumn( nColPos, oNewXbpColumn )
   METHOD   setLeftFrozen( aColumnIndex )
   METHOD   setRightFrozen( aColumnIndex )

   METHOD   down()
   METHOD   firstCol()
   METHOD   goBottom()
   METHOD   goTop()
   METHOD   lastCol()
   METHOD   left()
   METHOD   pageDown()
   METHOD   pageUp()
   METHOD   pageEnd()
   METHOD   panHome()
   METHOD   panLeft()
   METHOD   panRight()
   METHOD   right()
   METHOD   up()

   METHOD   deHilite()
//   METHOD   forceStable()
   METHOD   hiLite()
   METHOD   refreshAll()
   METHOD   refreshCurrent()

   METHOD   getData()

   METHOD   footerRbDown()                        SETGET
   METHOD   headerRbDown()                        SETGET
   METHOD   itemMarked()                          SETGET
   METHOD   itemRbDown()                          SETGET
   METHOD   itemSelected()                        SETGET

   METHOD   forceStable()                         SETGET
   METHOD   navigate()                            SETGET
   METHOD   pan()                                 SETGET

   DATA     sl_xbeBRW_FooterRbDown
   DATA     sl_xbeBRW_HeaderRbDown
   DATA     sl_xbeBRW_ItemMarked
   DATA     sl_xbeBRW_ItemRbDown
   DATA     sl_xbeBRW_ItemSelected
   DATA     sl_xbeBRW_ForceStable
   DATA     sl_xbeBRW_Navigate
   DATA     sl_xbeBRW_Pan

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:init()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:configure()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:exeBlock()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:addColumn()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:delColumn( nColPos )

   HB_SYMBOL_UNUSED( nColPos )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:getColumn( nColPos )

   HB_SYMBOL_UNUSED( nColPos )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:insColumn( nColPos, oXbpColumn )

   HB_SYMBOL_UNUSED( nColPos )
   HB_SYMBOL_UNUSED( oXbpColumn )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:setColumn( nColPos, oNewXbpColumn )

   HB_SYMBOL_UNUSED( nColPos )
   HB_SYMBOL_UNUSED( oNewXbpColumn )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:setLeftFrozen( aColumnIndex )

   HB_SYMBOL_UNUSED( aColumnIndex )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:setRightFrozen( aColumnIndex )

   HB_SYMBOL_UNUSED( aColumnIndex )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:down()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:firstCol()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:goBottom()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:goTop()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:lastCol()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:left()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:pageDown()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:pageUp()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:pageEnd()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:panHome()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:panLeft()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:panRight()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:right()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:up()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:deHilite()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:forceStable()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:hiLite()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:refreshAll()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:refreshCurrent()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:getData()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:footerRbDown()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:headerRbDown()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:itemMarked()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:itemRbDown()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:itemSelected()

   RETURN Self

/*----------------------------------------------------------------------*/
#if 0
METHOD XbpBrowse:forceStable()

   RETURN Self
#endif
/*----------------------------------------------------------------------*/

METHOD XbpBrowse:navigate()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpBrowse:pan()

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
//
//                              XbpColumn
//
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS XbpColumn INHERIT XbpWindow, XbpDataRef

   METHOD   new()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()

   DATA     colorBlock
   DATA     dataLink                              INIT      XBPCOL_TYPE_TEXT
   DATA     type

   DATA     dataArea
   DATA     footing
   DATA     heading
   DATA     picture
   DATA     valType

   METHOD   dataRow( nRowPos, lRepaint )
   METHOD   getRow( nRowPos )
   METHOD   hiliteRow()
   METHOD   refreshRows( nFirstRow, nLastRow )
   METHOD   rowCount()
   METHOD   scrollDown()
   METHOD   scrollUp()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpColumn:new()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpColumn:create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpColumn:configure()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpColumn:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpColumn:dataRow( nRowPos, lRepaint )

   HB_SYMBOL_UNUSED( nRowPos )
   HB_SYMBOL_UNUSED( lRepaint )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpColumn:getRow( nRowPos )

   HB_SYMBOL_UNUSED( nRowPos )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpColumn:hiliteRow()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpColumn:refreshRows( nFirstRow, nLastRow )

   HB_SYMBOL_UNUSED( nFirstRow )
   HB_SYMBOL_UNUSED( nLastRow )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpColumn:rowCount()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpColumn:scrollDown()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpColumn:scrollUp()

   RETURN Self

/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
//
//                              XbpCellGroup
//
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

CLASS XbpCellGroup INHERIT XbpWindow

   METHOD   new()
   METHOD   create()
   METHOD   configure()
   METHOD   destroy()

   DATA     clipParent                            INIT      .T.
   DATA     maxRow                                INIT      XBP_AUTOSIZE
   DATA     referenceString                       INIT      "W"
   DATA     type                                  INIT      XBPCOL_TYPE_TEXT

   METHOD   cellFromPos( aPos )
   METHOD   cellRect( nRowPos, lInnerRect )
   METHOD   drawCell( naRowPos, lRepaint )
   METHOD   getCell( nRowPos )
   METHOD   getCellColor()
   METHOD   hiliteCell()
   METHOD   rowCount()
   METHOD   scrollDown()
   METHOD   scrollUp()
   METHOD   setCellColor()
   METHOD   itemMarked()                          SETGET
   METHOD   itemSelected()                        SETGET

   DATA     sl_xbeCELLGR_ItemMarked
   DATA     sl_xbeCELLGR_ItemSelected

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:new()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:create()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:configure()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:cellFromPos( aPos )

   HB_SYMBOL_UNUSED( aPos )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:cellRect( nRowPos, lInnerRect )

   HB_SYMBOL_UNUSED( nRowPos )
   HB_SYMBOL_UNUSED( lInnerRect )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:drawCell( naRowPos, lRepaint )

   HB_SYMBOL_UNUSED( naRowPos )
   HB_SYMBOL_UNUSED( lRepaint )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:getCell( nRowPos )

   HB_SYMBOL_UNUSED( nRowPos )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:getCellColor()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:hiliteCell()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:rowCount()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:scrollDown()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:scrollUp()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:setCellColor()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:itemMarked()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpCellGroup:itemSelected()

   RETURN Self

/*----------------------------------------------------------------------*/



