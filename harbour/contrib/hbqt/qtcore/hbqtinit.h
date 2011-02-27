/*
 * $Id$
 */

/*
 * Harbour Project source code:
 * QT wrapper init helper macros
 *
 * Copyright 2011 Viktor Szakats (harbour.01 syenar.hu)
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

#define HBQT_SLOT_CALLBACK_OBJ( _name_ ) \
   static void hbqt_SlotsExec##_name_( PHB_ITEM * codeBlock, void ** arguments, QStringList pList ) \
   { \
      Q_UNUSED( pList ); \
      \
      hb_vmPushEvalSym(); \
      hb_vmPush( codeBlock ); \
      hb_vmPush( hbqt_create_object( hbqt_gcAllocate_##_name_( new _name_( ( *reinterpret_cast< _name_( * ) >( arguments[ 1 ] ) ) ), true ), HB_MACRO2STRING( _name_ ) ) ); \
      hb_vmSend( 1 ); \
   }

#define HBQT_SLOT_CALLBACK_OBJ_INT( _name_ ) \
   static void hbqt_SlotsExec##_name_##Int( PHB_ITEM * codeBlock, void ** arguments, QStringList pList ) \
   { \
      Q_UNUSED( pList ); \
      \
      hb_vmPushEvalSym(); \
      hb_vmPush( codeBlock ); \
      hb_vmPush( hbqt_create_object( hbqt_gcAllocate_##_name_( new _name_( ( *reinterpret_cast< _name_( * ) >( arguments[ 1 ] ) ) ), true ), HB_MACRO2STRING( _name_ ) ) ); \
      hb_vmPushInteger( *reinterpret_cast< int( * ) >( arguments[ 2 ] ) ); \
      hb_vmSend( 2 ); \
   }

#define HBQT_SLOT_CALLBACK_OBJ_OBJ( _name1_, _name2_ ) \
   static void hbqt_SlotsExec##_name1_##_name2_( PHB_ITEM * codeBlock, void ** arguments, QStringList pList ) \
   { \
      Q_UNUSED( pList ); \
      \
      hb_vmPushEvalSym(); \
      hb_vmPush( codeBlock ); \
      hb_vmPush( hbqt_create_object( hbqt_gcAllocate_##_name1_( new _name1_( ( *reinterpret_cast< _name1_( * ) >( arguments[ 1 ] ) ) ), true ), HB_MACRO2STRING( _name1_ ) ) ); \
      hb_vmPush( hbqt_create_object( hbqt_gcAllocate_##_name2_( new _name2_( ( *reinterpret_cast< _name2_( * ) >( arguments[ 2 ] ) ) ), true ), HB_MACRO2STRING( _name2_ ) ) ); \
      hb_vmSend( 2 ); \
   }
