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


FUNCTION QTextDocumentWriter( ... )
   RETURN HB_QTextDocumentWriter():new( ... )


CREATE CLASS QTextDocumentWriter INHERIT HbQtObjectHandler FUNCTION HB_QTextDocumentWriter

   METHOD  new( ... )

   METHOD  codec()
   METHOD  device()
   METHOD  fileName()
   METHOD  format()
   METHOD  setCodec( pCodec )
   METHOD  setDevice( pDevice )
   METHOD  setFileName( cFileName )
   METHOD  setFormat( pFormat )
   METHOD  write( pDocument )
   METHOD  write_1( pFragment )

   ENDCLASS


METHOD QTextDocumentWriter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTextDocumentWriter( ... )
   RETURN Self


METHOD QTextDocumentWriter:codec()
   RETURN Qt_QTextDocumentWriter_codec( ::pPtr )


METHOD QTextDocumentWriter:device()
   RETURN Qt_QTextDocumentWriter_device( ::pPtr )


METHOD QTextDocumentWriter:fileName()
   RETURN Qt_QTextDocumentWriter_fileName( ::pPtr )


METHOD QTextDocumentWriter:format()
   RETURN Qt_QTextDocumentWriter_format( ::pPtr )


METHOD QTextDocumentWriter:setCodec( pCodec )
   RETURN Qt_QTextDocumentWriter_setCodec( ::pPtr, hbqt_ptr( pCodec ) )


METHOD QTextDocumentWriter:setDevice( pDevice )
   RETURN Qt_QTextDocumentWriter_setDevice( ::pPtr, hbqt_ptr( pDevice ) )


METHOD QTextDocumentWriter:setFileName( cFileName )
   RETURN Qt_QTextDocumentWriter_setFileName( ::pPtr, cFileName )


METHOD QTextDocumentWriter:setFormat( pFormat )
   RETURN Qt_QTextDocumentWriter_setFormat( ::pPtr, hbqt_ptr( pFormat ) )


METHOD QTextDocumentWriter:write( pDocument )
   RETURN Qt_QTextDocumentWriter_write( ::pPtr, hbqt_ptr( pDocument ) )


METHOD QTextDocumentWriter:write_1( pFragment )
   RETURN Qt_QTextDocumentWriter_write_1( ::pPtr, hbqt_ptr( pFragment ) )

