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
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 *
 * Copyright 2009 Marcos Antonio Gambeta <marcosgambeta at gmail dot com>
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
/*----------------------------------------------------------------------*/


#include "hbclass.ch"


CREATE CLASS QMenuBar INHERIT QWidget

   VAR     pParent
   VAR     pPtr

   METHOD  New()
   METHOD  Configure( xObject )

   METHOD  activeAction()                      INLINE  Qt_QMenuBar_activeAction( ::pPtr )
   METHOD  addAction( cText )                  INLINE  Qt_QMenuBar_addAction( ::pPtr, cText )
   METHOD  addAction_1( cText, pReceiver, pMember )  INLINE  Qt_QMenuBar_addAction_1( ::pPtr, cText, pReceiver, pMember )
   METHOD  addAction_2( pAction )              INLINE  Qt_QMenuBar_addAction_2( ::pPtr, pAction )
   METHOD  addMenu( pMenu )                    INLINE  Qt_QMenuBar_addMenu( ::pPtr, pMenu )
   METHOD  addMenu_1( cTitle )                 INLINE  Qt_QMenuBar_addMenu_1( ::pPtr, cTitle )
   METHOD  addMenu_2( cIcon, cTitle )          INLINE  Qt_QMenuBar_addMenu_2( ::pPtr, cIcon, cTitle )
   METHOD  addSeparator()                      INLINE  Qt_QMenuBar_addSeparator( ::pPtr )
   METHOD  clear()                             INLINE  Qt_QMenuBar_clear( ::pPtr )
   METHOD  insertMenu( pBefore, pMenu )        INLINE  Qt_QMenuBar_insertMenu( ::pPtr, pBefore, pMenu )
   METHOD  insertSeparator( pBefore )          INLINE  Qt_QMenuBar_insertSeparator( ::pPtr, pBefore )
   METHOD  isDefaultUp()                       INLINE  Qt_QMenuBar_isDefaultUp( ::pPtr )
   METHOD  setActiveAction( pAct )             INLINE  Qt_QMenuBar_setActiveAction( ::pPtr, pAct )
   METHOD  setDefaultUp( lBool )               INLINE  Qt_QMenuBar_setDefaultUp( ::pPtr, lBool )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD New( pParent ) CLASS QMenuBar

   ::pParent := pParent

   ::pPtr := Qt_QMenuBar( pParent )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD Configure( xObject ) CLASS QMenuBar

   IF hb_isObject( xObject )
      ::pPtr := xObject:pPtr
   ELSEIF hb_isPointer( xObject )
      ::pPtr := xObject
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/
