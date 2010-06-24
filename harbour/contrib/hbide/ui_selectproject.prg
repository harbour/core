/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .ui file,                       */
/*          with Qt Generator, and run hbqtui.exe.                      */
/* -------------------------------------------------------------------- */
/*                                                                      */
/*               Pritpal Bedi <bedipritpal@hotmail.com>                 */
/*                                                                      */
/* -------------------------------------------------------------------- */

FUNCTION uiSelectproject( qParent )
   LOCAL oUI
   LOCAL oWidget
   LOCAL qObj := {=>}

   hb_hCaseMatch( qObj, .f. )

   oWidget := QDialog():new( qParent )
  
   oWidget:setObjectName( "Dialog" )
  
   qObj[ "Dialog"             ] := oWidget
  
   qObj[ "cbProjects"         ] := QComboBox():new(qObj[ "Dialog" ])
   qObj[ "label"              ] := QLabel():new(qObj[ "Dialog" ])
   qObj[ "btnOk"              ] := QPushButton():new(qObj[ "Dialog" ])
   qObj[ "btnCancel"          ] := QPushButton():new(qObj[ "Dialog" ])
   
   qObj[ "Dialog"             ]:resize(218, 108)
   qObj[ "cbProjects"         ]:setGeometry(QRect():new(20, 40, 181, 22))
   qObj[ "label"              ]:setGeometry(QRect():new(20, 10, 191, 16))
   qObj[ "label"              ]:setWordWrap(.F.)
   qObj[ "btnOk"              ]:setGeometry(QRect():new(45, 70, 75, 23))
   qObj[ "btnCancel"          ]:setGeometry(QRect():new(125, 70, 75, 23))
   qObj[ "label"              ]:setText( [Select a project to make it the current.] )
   qObj[ "btnOk"              ]:setText( [Ok] )
   qObj[ "btnCancel"          ]:setText( [Cancel] )

   oUI         := HbQtUI():new()
   oUI:qObj    := qObj
   oUI:oWidget := oWidget

   RETURN oUI

/*----------------------------------------------------------------------*/

