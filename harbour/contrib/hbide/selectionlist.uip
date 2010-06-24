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

FUNCTION uiSelectionlist( qParent )
   LOCAL oUI
   LOCAL oWidget
   LOCAL qObj := {=>}

   hb_hCaseMatch( qObj, .f. )

   oWidget := QDialog():new( qParent )
  
   oWidget:setObjectName( "Dialog" )
  
   qObj[ "Dialog"             ] := oWidget
  
   qObj[ "gridLayout"         ] := QGridLayout():new(qObj[ "Dialog" ])
   qObj[ "listOptions"        ] := QListView():new(qObj[ "Dialog" ])
   qObj[ "horizontalSpacer"   ] := QSpacerItem():new(40, 20, 7, 1)
   qObj[ "buttonOk"           ] := QPushButton():new(qObj[ "Dialog" ])
   qObj[ "buttonCancel"       ] := QPushButton():new(qObj[ "Dialog" ])
   
   qObj[ "Dialog"             ]:resize(223, 285)
   qObj[ "listOptions"        ]:setEditTriggers(0)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "listOptions" ], 0, 0, 1, 3)
   qObj[ "gridLayout"         ]:addItem(qObj[ "horizontalSpacer" ], 1, 0, 1, 1)
   qObj[ "buttonOk"           ]:setMaximumSize(QSize():new(77, 25))
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonOk" ], 1, 1, 1, 1)
   qObj[ "buttonCancel"       ]:setMaximumSize(QSize():new(77, 25))
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonCancel" ], 1, 2, 1, 1)
   qObj[ "Dialog"             ]:setWindowTitle(q__tr("Dialog", "Select an Option", 0, "UTF8"))
   qObj[ "buttonOk"           ]:setText( [OK] )
   qObj[ "buttonCancel"       ]:setText( [Cancel] )

   oUI         := HbQtUI():new()
   oUI:qObj    := qObj
   oUI:oWidget := oWidget

   RETURN oUI

/*----------------------------------------------------------------------*/

