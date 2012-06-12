/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2012 Carlos Bacco <carlosbacco at gmail.com>
 * www - http://harbour-project.org
 *
 */

#include "hbqtgui.ch"

PROCEDURE Main()

   LOCAL oWid
   LOCAL oLabel1
   LOCAL oLabel2
   LOCAL oLay1

   oWid := QWidget()
   oWid:setWindowTitle( "Draggable window" )
   oWId:setAttribute( Qt_WA_TranslucentBackground )
   oWid:setWindowFlags( Qt_FramelessWindowHint )

   oLabel1 := QLabel()
   oLabel1:setPixmap( QPixmap( hb_DirBase() + "harbour-logo.png" ) )

   oLabel2 := QLabel( "Drag-me with the mouse. Double-click to close." )

   oLay1 := QVBoxLayout( oWid )
   oLay1:addWidget( oLabel1 )
   oLay1:addWidget( oLabel2 )

   oWid:connect( QEvent_MouseButtonPress  , {| oMouseEvent | WinDrag( oMouseEvent, oWid ) } )
   oWid:connect( QEvent_MouseButtonRelease, {| oMouseEvent | WinDrag( oMouseEvent, oWid ) } )
   oWid:connect( QEvent_MouseMove, {| oMouseEvent | WinDrag( oMouseEvent, oWid ) } )

   oWid:connect( QEvent_MouseButtonDblClick, {|| oWid:close() } )

   oWid:show()
   QApplication():exec()

   oWid:disconnect( QEvent_MouseButtonPress  , {| oMouseEvent | WinDrag( oMouseEvent, oWid ) } )
   oWid:disconnect( QEvent_MouseButtonRelease, {| oMouseEvent | WinDrag( oMouseEvent, oWid ) } )
   oWid:disconnect( QEvent_MouseMove, {| oMouseEvent | WinDrag( oMouseEvent, oWid ) } )

   oWid:disconnect( QEvent_MouseButtonDblClick, {|| oWid:close() } )
   
   RETURN

PROCEDURE WinDrag( oMouseEvent, oWid )

   STATIC s_nXOffset
   STATIC s_nYOffset
   STATIC s_lOnMove := .F.

   LOCAL nType

   nType := oMouseEvent:Type()

   IF s_lOnMove .AND. nType == QEvent_MouseMove
      oWid:move( oMouseEvent:globalX() - s_nXOffset, oMouseEvent:globalY() - s_nYOffset )
      oMouseEvent:accept()

   ELSEIF nType == QEvent_MouseButtonPress .AND. oMouseEvent:button() == Qt_LeftButton
      s_nXOffset := oMouseEvent:globalX() - oWid:x()
      s_nYOffset := oMouseEvent:globalY() - oWid:y()
      s_lOnMove := .T.
      QApplication():setOverrideCursor( QCursor( Qt_ClosedHandCursor ) )
      oMouseEvent:accept()

   ELSEIF nType == QEvent_MouseButtonRelease .AND. oMouseEvent:button() == Qt_LeftButton
      s_lOnMove := .F.
      QApplication():restoreOverrideCursor()
      oMouseEvent:accept()

   ELSE
      oMouseEvent:ignore()

   ENDIF

   RETURN
