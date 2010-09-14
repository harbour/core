/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2010 Pritpal Bedi <bedipritpal@hotmail.com>
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
 *                 Pritpal Bedi <bedipritpal@hotmail.com>
 *                               18Mar2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"
#include "hbqt.ch"

/*----------------------------------------------------------------------*/

#define PAGE_INTRO                                1
#define PAGE_X                                    2

/*----------------------------------------------------------------------*/

FUNCTION hbide_startOpenWizard()
   STATIC oWz

   oWz := IdeWizard():new():create()

   RETURN NIL

/*----------------------------------------------------------------------*/

CLASS IdeWizard INHERIT IdeObject

   DATA   aPages                                  INIT {}

   METHOD new( oIde )
   METHOD create( oIde )
   METHOD destroy()
   METHOD execEvent( nMode, p )
   METHOD addIntroPage()
   METHOD addDescPage()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeWizard:new( oIde )

   ::oIde := oIde

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeWizard:create( oIde )

   DEFAULT oIde TO ::oIde
   ::oIde := oIde

   ::oUI := QWizard()
   ::oUI:setWindowTitle( "Open" )
   ::oUI:setWindowIcon( hbide_image( "hbide" ) )

   ::addIntroPage()
   ::addDescPage()

   ::oUI:show()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeWizard:destroy()
   LOCAL a_, obj

   IF !empty( ::oUI )
      FOR EACH a_ IN ::aPages
         FOR EACH obj IN a_ DESCEND
            obj := NIL
         NEXT
      NEXT
      ::oUI := NIL
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeWizard:execEvent( nMode, p )

   HB_SYMBOL_UNUSED( nMode )
   HB_SYMBOL_UNUSED( p )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeWizard:addIntroPage()
   LOCAL page, label, layout

   page := QWizardPage()
   page:setTitle( "Introduction" )

   label := QLabel( "This wizard will help you register your copy " + ;
                                                   "of Super Product Two." )
   label:setWordWrap( .t. )

   layout := QVBoxLayout()
   layout:addWidget( label )
   page:setLayout( layout )
   page:setTitle( "This is waizard" )
   page:setSubTitle( "So the ?" )

   aadd( ::aPages, { PAGE_INTRO, page, layout, label } )

   ::oUI:setPage( len( ::aPages ), page )

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD IdeWizard:addDescPage()

   RETURN Self

/*----------------------------------------------------------------------*/
