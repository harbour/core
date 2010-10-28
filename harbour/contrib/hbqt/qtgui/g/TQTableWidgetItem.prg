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
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*                            C R E D I T S                             */
/*----------------------------------------------------------------------*/
/*
 * Marcos Antonio Gambeta
 *    for providing first ever prototype parsing methods. Though the current
 *    implementation is diametrically different then what he proposed, still
 *    current code shaped on those footsteps.
 *
 * Viktor Szakats
 *    for directing the project with futuristic vision;
 *    for designing and maintaining a complex build system for hbQT, hbIDE;
 *    for introducing many constructs on PRG and C++ levels;
 *    for streamlining signal/slots and events management classes;
 *
 * Istvan Bisz
 *    for introducing QPointer<> concept in the generator;
 *    for testing the library on numerous accounts;
 *    for showing a way how a GC pointer can be detached;
 *
 * Francesco Perillo
 *    for taking keen interest in hbQT development and peeking the code;
 *    for providing tips here and there to improve the code quality;
 *    for hitting bulls eye to describe why few objects need GC detachment;
 *
 * Carlos Bacco
 *    for implementing HBQT_TYPE_Q*Class enums;
 *    for peeking into the code and suggesting optimization points;
 *
 * Przemyslaw Czerpak
 *    for providing tips and trick to manipulate HVM internals to the best
 *    of its use and always showing a path when we get stuck;
 *    A true tradition of a MASTER...
*/
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


REQUEST __HBQTGUI


FUNCTION QTableWidgetItem( ... )
   RETURN HB_QTableWidgetItem():new( ... )

FUNCTION QTableWidgetItemFromPointer( ... )
   RETURN HB_QTableWidgetItem():fromPointer( ... )


CREATE CLASS QTableWidgetItem INHERIT HbQtObjectHandler FUNCTION HB_QTableWidgetItem

   METHOD  new( ... )

   METHOD  background                    // (  )                                               -> oQBrush
   METHOD  checkState                    // (  )                                               -> nQt_CheckState
   METHOD  clone                         // (  )                                               -> oQTableWidgetItem
   METHOD  column                        // (  )                                               -> nInt
   METHOD  data                          // ( nRole )                                          -> oQVariant
   METHOD  flags                         // (  )                                               -> nQt_ItemFlags
   METHOD  font                          // (  )                                               -> oQFont
   METHOD  foreground                    // (  )                                               -> oQBrush
   METHOD  icon                          // (  )                                               -> oQIcon
   METHOD  isSelected                    // (  )                                               -> lBool
   METHOD  read                          // ( oQDataStream )                                   -> NIL
   METHOD  row                           // (  )                                               -> nInt
   METHOD  setBackground                 // ( oQBrush )                                        -> NIL
   METHOD  setCheckState                 // ( nState )                                         -> NIL
   METHOD  setData                       // ( nRole, oQVariant )                               -> NIL
   METHOD  setFlags                      // ( nFlags )                                         -> NIL
   METHOD  setFont                       // ( oQFont )                                         -> NIL
   METHOD  setForeground                 // ( oQBrush )                                        -> NIL
   METHOD  setIcon                       // ( coQIcon )                                        -> NIL
   METHOD  setSelected                   // ( lSelect )                                        -> NIL
   METHOD  setSizeHint                   // ( oQSize )                                         -> NIL
   METHOD  setStatusTip                  // ( cStatusTip )                                     -> NIL
   METHOD  setText                       // ( cText )                                          -> NIL
   METHOD  setTextAlignment              // ( nAlignment )                                     -> NIL
   METHOD  setToolTip                    // ( cToolTip )                                       -> NIL
   METHOD  setWhatsThis                  // ( cWhatsThis )                                     -> NIL
   METHOD  sizeHint                      // (  )                                               -> oQSize
   METHOD  statusTip                     // (  )                                               -> cQString
   METHOD  tableWidget                   // (  )                                               -> oQTableWidget
   METHOD  text                          // (  )                                               -> cQString
   METHOD  textAlignment                 // (  )                                               -> nInt
   METHOD  toolTip                       // (  )                                               -> cQString
   METHOD  type                          // (  )                                               -> nInt
   METHOD  whatsThis                     // (  )                                               -> cQString
   METHOD  write                         // ( oQDataStream )                                   -> NIL

   ENDCLASS


METHOD QTableWidgetItem:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QTableWidgetItem( ... )
   RETURN Self


METHOD QTableWidgetItem:background( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QTableWidgetItem_background( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:checkState( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_checkState( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:clone( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTableWidgetItemFromPointer( Qt_QTableWidgetItem_clone( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:column( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_column( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:data( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN QVariantFromPointer( Qt_QTableWidgetItem_data( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:flags( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_flags( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:font( ... )
   SWITCH PCount()
   CASE 0
      RETURN QFontFromPointer( Qt_QTableWidgetItem_font( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:foreground( ... )
   SWITCH PCount()
   CASE 0
      RETURN QBrushFromPointer( Qt_QTableWidgetItem_foreground( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:icon( ... )
   SWITCH PCount()
   CASE 0
      RETURN QIconFromPointer( Qt_QTableWidgetItem_icon( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:isSelected( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_isSelected( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:read( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_read( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:row( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_row( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setBackground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setBackground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setCheckState( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setCheckState( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setData( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isObject( hb_pvalue( 2 ) )
         RETURN Qt_QTableWidgetItem_setData( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setFlags( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setFlags( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setFont( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setFont( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setForeground( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setForeground( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setIcon( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE ( hb_isObject( hb_pvalue( 1 ) ) .OR. hb_isChar( hb_pvalue( 1 ) ) )
         RETURN Qt_QTableWidgetItem_setIcon( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setSelected( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setSelected( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setSizeHint( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setSizeHint( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setStatusTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setStatusTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setText( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setText( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setTextAlignment( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setTextAlignment( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setToolTip( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setToolTip( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:setWhatsThis( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_setWhatsThis( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:sizeHint( ... )
   SWITCH PCount()
   CASE 0
      RETURN QSizeFromPointer( Qt_QTableWidgetItem_sizeHint( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:statusTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_statusTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:tableWidget( ... )
   SWITCH PCount()
   CASE 0
      RETURN QTableWidgetFromPointer( Qt_QTableWidgetItem_tableWidget( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:text( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_text( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:textAlignment( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_textAlignment( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:toolTip( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_toolTip( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:type( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_type( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:whatsThis( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QTableWidgetItem_whatsThis( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QTableWidgetItem:write( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QTableWidgetItem_write( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

