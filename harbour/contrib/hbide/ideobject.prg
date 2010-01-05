/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009-2010 Pritpal Bedi <pritpal@vouchcac.com>
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
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*
 *                                EkOnkar
 *                          ( The LORD is ONE )
 *
 *                            Harbour-Qt IDE
 *
 *                  Pritpal Bedi <pritpal@vouchcac.com>
 *                               03Jan2010
 */
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*/

#include "hbide.ch"
#include "common.ch"
#include "hbclass.ch"

/*----------------------------------------------------------------------*/

CLASS IdeObject

   ACCESS pSlots                                  INLINE hbxbp_getSlotsPtr()
   ACCESS pEvents                                 INLINE hbxbp_getEventsPtr()

   DATA   oIde
   DATA   oUI

   ACCESS oFR                                     INLINE ::oIde:oFR
   ACCESS oED                                     INLINE ::oIde:oED
   ACCESS oPM                                     INLINE ::oIde:oPM
   ACCESS oDK                                     INLINE ::oIde:oDK

   ACCESS qCurEdit                                INLINE ::oIde:qCurEdit
   ACCESS oCurEditor                              INLINE ::oIde:oCurEditor
   ACCESS qTabWidget                              INLINE ::oIde:oDA:oTabWidget:oWidget

   ACCESS qBrushWrkProject                        INLINE ::oIde:qBrushWrkProject

   ACCESS cWrkProject                             INLINE ::oIde:cWrkProject
   ACCESS cWrkTheme                               INLINE ::oIde:cWrkTheme
   ACCESS aProjects                               INLINE ::oIde:aProjects
   ACCESS aINI                                    INLINE ::oIde:aINI

   ACCESS oDlg                                    INLINE ::oIde:oDlg
   ACCESS oDA                                     INLINE ::oIde:oDA

   ACCESS oDockPT                                 INLINE ::oIde:oDockPT
   ACCESS oProjTree                               INLINE ::oIde:oProjTree
   ACCESS oProjRoot                               INLINE ::oIde:oProjRoot
   ACCESS aProjData                               INLINE ::oIde:aProjData

   ACCESS oDockED                                 INLINE ::oIde:oDockED
   ACCESS oEditTree                               INLINE ::oIde:oEditTree
   ACCESS oOpenedSources                          INLINE ::oIde:oOpenedSources

   ACCESS oDockR                                  INLINE ::oIde:oDockR
   ACCESS oFuncList                               INLINE ::oIde:oFuncList

   ACCESS oDockB                                  INLINE ::oIde:oDockB
   ACCESS oCompileResult                          INLINE ::oIde:oCompileResult

   ACCESS oDockB1                                 INLINE ::oIde:oDockB1
   ACCESS oLinkResult                             INLINE ::oIde:oLinkResult

   ACCESS oDockB2                                 INLINE ::oIde:oDockB2
   ACCESS oOutputResult                           INLINE ::oIde:oOutputResult

   ACCESS resPath                                 INLINE ::oIde:resPath
   ACCESS pathSep                                 INLINE ::oIde:pathSep
   ACCESS oFont                                   INLINE ::oIde:oFont
   ACCESS oThemes                                 INLINE ::oIde:oThemes
   ACCESS aTabs                                   INLINE ::oIde:aTabs

   ACCESS lProjTreeVisible                        INLINE ::oIde:lProjTreeVisible
   ACCESS lDockRVisible                           INLINE ::oIde:lDockRVisible
   ACCESS lDockBVisible                           INLINE ::oIde:lDockBVisible
   ACCESS lTabCloseRequested                      INLINE ::oIde:lTabCloseRequested
   ACCESS oSBar                                   INLINE ::oIde:oSBar

   METHOD new()                                   VIRTUAL
   METHOD create()                                VIRTUAL
   METHOD destroy()                               VIRTUAL

   ERROR HANDLER OnError()

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD IdeObject:onError( ... )
   LOCAL cMsg

   cMsg := __GetMessage()
   IF SubStr( cMsg, 1, 1 ) == "_"
      cMsg := SubStr( cMsg, 2 )
   ENDIF

   IF left( cMsg, 2 ) == "Q_"
      RETURN ::oUI:&cMsg( ... )
   ELSE
      RETURN ::oIde:&cMsg( ... )
   ENDIF

   RETURN nil

/*----------------------------------------------------------------------*/
