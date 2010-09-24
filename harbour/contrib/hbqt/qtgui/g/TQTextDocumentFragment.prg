/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project source code:
 * QT wrapper main header
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


FUNCTION QTextDocumentFragment( ... )
   RETURN HB_QTextDocumentFragment():new( ... )


CREATE CLASS QTextDocumentFragment INHERIT HbQtObjectHandler FUNCTION HB_QTextDocumentFragment

   METHOD  new( ... )

   METHOD  isEmpty()
   METHOD  toHtml( ... )
   METHOD  toPlainText()
   METHOD  fromHtml( ... )
   METHOD  fromPlainText( cPlainText )

   ENDCLASS


METHOD QTextDocumentFragment:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextDocumentFragment( ... )
   RETURN Self


METHOD QTextDocumentFragment:isEmpty()
   RETURN Qt_QTextDocumentFragment_isEmpty( ::pPtr )


METHOD QTextDocumentFragment:toHtml( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "PO"
                // QString toHtml ( const QByteArray & encoding ) const
                // PO p QByteArray
         RETURN Qt_QTextDocumentFragment_toHtml( ::pPtr, ... )
      ENDCASE
   CASE nP == 0
             // QString toHtml () const
      RETURN Qt_QTextDocumentFragment_toHtml_1( ::pPtr, ... )
   ENDCASE
   RETURN NIL


METHOD QTextDocumentFragment:toPlainText()
   RETURN Qt_QTextDocumentFragment_toPlainText( ::pPtr )


METHOD QTextDocumentFragment:fromHtml( ... )
   LOCAL p, aP, nP, aV := {}
   aP := hb_aParams()
   nP := len( aP )
   ::valtypes( aP, aV )
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   DO CASE
   CASE nP == 2
      DO CASE
      CASE aV[ 1 ] $ "C" .AND. aV[ 2 ] $ "PO"
                // QTextDocumentFragment fromHtml ( const QString & text, const QTextDocument * resourceProvider )
                // C c QString, PO p QTextDocument
         RETURN QTextDocumentFragment():from( Qt_QTextDocumentFragment_fromHtml_1( ::pPtr, ... ) )
      ENDCASE
   CASE nP == 1
      DO CASE
      CASE aV[ 1 ] $ "C"
                // QTextDocumentFragment fromHtml ( const QString & text )
                // C c QString
         RETURN QTextDocumentFragment():from( Qt_QTextDocumentFragment_fromHtml( ::pPtr, ... ) )
      ENDCASE
   ENDCASE
   RETURN NIL


METHOD QTextDocumentFragment:fromPlainText( cPlainText )
   RETURN Qt_QTextDocumentFragment_fromPlainText( ::pPtr, cPlainText )

