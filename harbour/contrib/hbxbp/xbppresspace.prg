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
 *                               EkOnkar
 *                         ( The LORD is ONE )
 *
 *                Xbase++ Compatible xbpPresSpace Class
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                              08Jul2009
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"

#include "xbp.ch"
#include "appevent.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

CLASS XbpPresSpace

   DATA     mode
   DATA     d

   METHOD   new()
   METHOD   create()
   METHOD   configure()                           VIRTUAL
   METHOD   destroy()                             VIRTUAL

   METHOD   drawMode( nDrawMode )                 VIRTUAL
   METHOD   device()                              VIRTUAL
   METHOD   lastError()                           VIRTUAL
   METHOD   mapPoint()                            VIRTUAL
   METHOD   mapColor()                            VIRTUAL
   METHOD   mapColorIndex( lCompatible )          VIRTUAL
   METHOD   setAttrArea( aAttributes )            VIRTUAL
   METHOD   setAttrLine( aAttributes )            VIRTUAL
   METHOD   setAttrMarker( aAttributes )          VIRTUAL
   METHOD   setAttrString( aAttributes )          VIRTUAL
   METHOD   setColor( nForeground, nBackground )  INLINE    ::d := { ::nColorFG, ::nColorBG }, ;
                                                            IF( !empty( nForeground ), ::nColorFG := nForeground, ), ;
                                                            IF( !empty( nBackground ), ::nColorBG := nBackground, ), ::d
   METHOD   setColorIndex()                       VIRTUAL
   METHOD   setFont( oXbpFont )                   VIRTUAL
   METHOD   setGraTransform( aMatrix, nMode )     VIRTUAL
   METHOD   setPageSize( aPageSize, nUnits )      VIRTUAL
   METHOD   setViewPort( aViewPort )              VIRTUAL

   PROTECTED:

   DATA     aAttrArea
   DATA     aAttrLine
   DATA     aAttrMarker
   DATA     aAttrString
   DATA     nDrawMode
   DATA     lMapColorIndex
   DATA     nColorFG                              INIT      GraMakeRGBColor( { 0,0,0 } )
   DATA     nColorBG                              INIT      GraMakeRGBColor( { 255,255,255 } )
   DATA     oXbpFont
   DATA     aGraTransMatrix
   DATA     nGraTransMode
   DATA     aPageSize
   DATA     nUnits
   DATA     aViewPort

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:new()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPresSpace:create()
   RETURN Self

/*----------------------------------------------------------------------*/
