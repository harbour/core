/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * Source file for the Xbp*Classes
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://harbour-project.org
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
 *                Xbase++ Compatible xbpPrintDialog Class
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

/*----------------------------------------------------------------------*/

CLASS XbpPrintDialog INHERIT XbpWindow

   DATA     enableCollate                         INIT      .F.
   DATA     enableMark                            INIT      .F.
   DATA     enableNumCopies                       INIT      .F.
   DATA     enablePrintToFile                     INIT      .F.
   DATA     mode                                  INIT      XBPPDLG_MODE_DRIVER
   DATA     pageRange                             INIT      { 0,0 }

   DATA     collate                               INIT      .F.
   DATA     numCopies                             INIT      1
   DATA     pageRangeSelected                     INIT      {}
   DATA     printRange                            INIT      XBPPDLG_PRINT_ALLPAGES
   DATA     printToFile                           INIT      .F.

   METHOD   new( oParent, oOwner )
   METHOD   create( oParent, oOwner )
   METHOD   destroy()
   METHOD   display( oXbpPrinter )

   DATA     pPrinter                               PROTECTED
   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD XbpPrintDialog:new( oParent, oOwner )

   ::xbpWindow:init( oParent, oOwner )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPrintDialog:create( oParent, oOwner )

   ::xbpWindow:create( oParent, oOwner )

   ::oWidget := QPrintDialog()

   ::oWidget:connect( "accepted(QPrinter)", {|p| ::pPrinter := p } )

   ::postCreate()
   RETURN Self

/*----------------------------------------------------------------------*/

METHOD XbpPrintDialog:destroy()

   IF len( ::aConnections ) > 0
      ::aConnections := {}
   ENDIF

   ::oWidget := NIL

   RETURN nil

/*----------------------------------------------------------------------*/

METHOD XbpPrintDialog:display( oXbpPrinter )
   LOCAL nResult, nOpt, n

   IF hb_isObject( oXbpPrinter )
      // Parameters be based onto that

   ENDIF

   ::oWidget:setOption( QAbstractPrintDialog_None               , .T.                 )
   ::oWidget:setOption( QAbstractPrintDialog_PrintToFile        , ::enablePrintToFile )
   ::oWidget:setOption( QAbstractPrintDialog_PrintSelection     , ::enableMark        )
   ::oWidget:setOption( QAbstractPrintDialog_PrintPageRange     , ::pageRange[ 1 ] > 0 .and. ::pageRange[ 2 ] > 0 )
   ::oWidget:setOption( QAbstractPrintDialog_PrintCollateCopies , ::enableCollate     )

   IF ::pageRange[ 1 ] > 0 .and. ::pageRange[ 2 ] > 0
      ::oWidget:setMinMax( ::pageRange[ 1 ], ::pageRange[ 2 ] )
      ::oWidget:setFromTo( ::pageRange[ 1 ], ::pageRange[ 2 ] )
   ENDIF

   DO CASE
   CASE ::printRange == XBPPDLG_PRINT_ALLPAGES
      ::oWidget:setPrintRange( QAbstractPrintDialog_AllPages  )
   CASE ::printRange == XBPPDLG_PRINT_MARK
      ::oWidget:setPrintRange( QAbstractPrintDialog_Selection )
   CASE ::printRange == XBPPDLG_PRINT_PAGERANGE
      ::oWidget:setPrintRange( QAbstractPrintDialog_PageRange )
   ENDCASE

   nResult := ::oWidget:exec()

   IF nResult == QDialog_Accepted
      IF !hb_isObject( oXbpPrinter )
         oXbpPrinter := XbpPrinter():new()
         oXbpPrinter:oWidget := QPrinter()
      ENDIF
      oXbpPrinter:oWidget           := QPrinter( ::pPrinter )
      oXbpPrinter:oPrintEngine      := QPrintEngine()
      oXbpPrinter:oPrintEngine      := oXbpPrinter:oWidget:printEngine()

      oXbpPrinter:setDevName( oXbpPrinter:oWidget:printerName() )

      ::numCopies   := oXbpPrinter:setNumCopies()
      ::collate     := oXbpPrinter:setCollationMode()

      nOpt := ::oWidget:options()
      ::printToFile := hb_bitAnd( nOpt, QAbstractPrintDialog_PrintToFile ) == QAbstractPrintDialog_PrintToFile

      n := oXbpPrinter:oWidget:setPrintRange()
      ::printRange := IF( n == 0, XBPPDLG_PRINT_ALLPAGES, IF( n == 1, XBPPDLG_PRINT_MARK, XBPPDLG_PRINT_PAGERANGE ) )

      ::pageRangeSelected := { oXbpPrinter:oWidget:fromPage(), oXbpPrinter:oWidget:toPage() }
   ENDIF

   ::destroy()

   RETURN oXbpPrinter

/*----------------------------------------------------------------------*/
