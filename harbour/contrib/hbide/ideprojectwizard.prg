/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2012 Pritpal Bedi <bedipritpal@hotmail.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               14Jul2012
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "hbqtgui.ch"
#include "common.ch"
#include "hbclass.ch"

/*----------------------------------------------------------------------*/
//
//                          Class IdeProjectWizard
//
/*----------------------------------------------------------------------*/

CREATE CLASS IdeProjectWizard INHERIT IdeObject

   METHOD  new( oIde )
   METHOD  create( oIde )
   METHOD  destroy()
   METHOD  show()
   METHOD  execEvent( xEvent, p, p1 )

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:new( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:destroy()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:show()

   IF empty( ::oUI )
      ::oUI := ui_projectWizard():new()
      ::oUI:oWidget:connect( QEvent_Close, {|| ::oUI:oWidget:done( 0 ) } )
      ::oUI:setWindowFlags( Qt_Sheet )
      ::oUI:setWindowIcon( QIcon( hbide_image( "hbide" ) ) )

      ::oUI:btnNext:connect( "clicked()", {|| ::execEvent( "btnNext_clicked" ) } )
      ::oUI:btnBack:connect( "clicked()", {|| ::execEvent( "btnBack_clicked" ) } )
   ENDIF

   ::oUI:exec()
   ::oUI:oWidget:hide()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeProjectWizard:execEvent( xEvent, p, p1 )

   HB_SYMBOL_UNUSED( p )
   HB_SYMBOL_UNUSED( p1 )

   SWITCH xEvent
   CASE "btnNext_clicked"
      IF ::oUI:stackedWidget:currentIndex() == 0
         ::oUI:stackedWidget:setcurrentIndex( 1 )
      ENDIF
      EXIT
   CASE "btnBack_clicked"
      IF ::oUI:stackedWidget:currentIndex() == 1
         ::oUI:stackedWidget:setcurrentIndex( 0 )
      ENDIF
      EXIT
   ENDSWITCH

   RETURN NIL

/*----------------------------------------------------------------------*/

