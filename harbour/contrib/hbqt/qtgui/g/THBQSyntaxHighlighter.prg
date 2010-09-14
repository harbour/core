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


FUNCTION HBQSyntaxHighlighter( ... )
   RETURN HB_HBQSyntaxHighlighter():new( ... )


CREATE CLASS HBQSyntaxHighlighter INHERIT HbQtObjectHandler, HB_QSyntaxHighlighter FUNCTION HB_HBQSyntaxHighlighter

   METHOD  new( ... )

   METHOD  hbSetMultiLineCommentFormat( pFormat )
   METHOD  hbSetSingleLineCommentFormat( pFormat )
   METHOD  hbSetRule( cName, cPattern, pFormat )
   METHOD  hbSetFormat( cName, pFormat )
   METHOD  hbSetFormatColumnSelection( nStart, nCount, pColor )
   METHOD  hbSetRuleWithRegExp( cName, pReg, pFormat )

   ENDCLASS


METHOD HBQSyntaxHighlighter:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_HBQSyntaxHighlighter( ... )
   RETURN Self


METHOD HBQSyntaxHighlighter:hbSetMultiLineCommentFormat( pFormat )
   RETURN Qt_HBQSyntaxHighlighter_hbSetMultiLineCommentFormat( ::pPtr, hbqt_ptr( pFormat ) )


METHOD HBQSyntaxHighlighter:hbSetSingleLineCommentFormat( pFormat )
   RETURN Qt_HBQSyntaxHighlighter_hbSetSingleLineCommentFormat( ::pPtr, hbqt_ptr( pFormat ) )


METHOD HBQSyntaxHighlighter:hbSetRule( cName, cPattern, pFormat )
   RETURN Qt_HBQSyntaxHighlighter_hbSetRule( ::pPtr, cName, cPattern, hbqt_ptr( pFormat ) )


METHOD HBQSyntaxHighlighter:hbSetFormat( cName, pFormat )
   RETURN Qt_HBQSyntaxHighlighter_hbSetFormat( ::pPtr, cName, hbqt_ptr( pFormat ) )


METHOD HBQSyntaxHighlighter:hbSetFormatColumnSelection( nStart, nCount, pColor )
   RETURN Qt_HBQSyntaxHighlighter_hbSetFormatColumnSelection( ::pPtr, nStart, nCount, hbqt_ptr( pColor ) )


METHOD HBQSyntaxHighlighter:hbSetRuleWithRegExp( cName, pReg, pFormat )
   RETURN Qt_HBQSyntaxHighlighter_hbSetRuleWithRegExp( ::pPtr, cName, hbqt_ptr( pReg ), hbqt_ptr( pFormat ) )

