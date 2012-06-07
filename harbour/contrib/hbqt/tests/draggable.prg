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

Procedure Main()
   Local oWid
   Local oLabel1
   Local oLabel2
   Local oLay1

   oWid := QWidget()
   oWid:setWindowTitle( 'Draggable window' )
   oWId:setAttribute( Qt_WA_TranslucentBackground )
   oWid:setWindowFlags(Qt_FramelessWindowHint)

   oLabel1:= QLabel()
   oLabel1:setPixmap( QPixmap( "harbour-logo.png" ) )

   oLabel2:= QLabel( "Drag-me with the mouse. Double-click to close." )

   oLay1 := QVBoxLayout( oWid )
   oLay1:addWidget( oLabel1 )
   oLay1:addWidget( oLabel2 )

   oWid:connect( QEvent_MouseButtonPress  , {|oMouseEvent| WinDrag( oMouseEvent, oWid ) } )
   oWid:connect( QEvent_MouseButtonRelease, {|oMouseEvent| WinDrag( oMouseEvent, oWid ) } )
   oWid:connect( QEvent_MouseMove, {|oMouseEvent| WinDrag( oMouseEvent, oWid ) } )
   
   oWid:connect( QEvent_MouseButtonDblClick, {|| oWid:close() } )

   oWid:show()
   QApplication():exec()
   Return



Procedure WinDrag( oMouseEvent, oWid )
   Static nXOffset
   Static nYOffset
   Static lOnMove := .F.
   Local  nType

   nType := oMouseEvent:type()

   If lOnMove .and. ( nType == QEvent_MouseMove )
      oWid:move( oMouseEvent:globalX() - nXOffset, oMouseEvent:globalY() - nYOffset )
      oMouseEvent:accept()

   ElseIf ( nType == QEvent_MouseButtonPress .and. oMouseEvent:button() == Qt_LeftButton )
      nXOffset := oMouseEvent:globalX() - oWid:x()
      nYOffset := oMouseEvent:globalY() - oWid:y()
      lOnMove := .T.
      QApplication():setOverrideCursor( QCursor( Qt_ClosedHandCursor ) )
      oMouseEvent:accept()

   ElseIf ( nType == QEvent_MouseButtonRelease .and. oMouseEvent:button() == Qt_LeftButton )
      lOnMove := .F.
      QApplication():restoreOverrideCursor()
      oMouseEvent:accept()

   Else
      oMouseEvent:ignore()

   EndIf
   Return

