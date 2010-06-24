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

FUNCTION uiFunclist( qParent )
   LOCAL oUI
   LOCAL oWidget
   LOCAL qObj := {=>}

   hb_hCaseMatch( qObj, .f. )

   oWidget := QWidget():new( qParent )
  
   oWidget:setObjectName( "Form" )
  
   qObj[ "Form"               ] := oWidget
  
   qObj[ "gridLayout"         ] := QGridLayout():new(qObj[ "Form" ])
   qObj[ "editFunction"       ] := QLineEdit():new(qObj[ "Form" ])
   qObj[ "tableFuncList"      ] := QTableWidget():new(qObj[ "Form" ])
   qObj[ "label"              ] := QLabel():new(qObj[ "Form" ])
   qObj[ "editSyntax"         ] := QLineEdit():new(qObj[ "Form" ])
   qObj[ "labelEntries"       ] := QLabel():new(qObj[ "Form" ])
   qObj[ "line"               ] := QFrame():new(qObj[ "Form" ])
   qObj[ "buttonMark"         ] := QPushButton():new(qObj[ "Form" ])
   qObj[ "buttonLoad"         ] := QPushButton():new(qObj[ "Form" ])
   qObj[ "buttonTag"          ] := QPushButton():new(qObj[ "Form" ])
   qObj[ "buttonClose"        ] := QPushButton():new(qObj[ "Form" ])
   qObj[ "listProjects"       ] := QListWidget():new(qObj[ "Form" ])
   
   qObj[ "Form"               ]:resize(400, 486)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "editFunction" ], 1, 0, 1, 5)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "tableFuncList" ], 2, 0, 1, 5)
   qObj[ "label"              ]:setMaximumSize(QSize():new(46, 16777215))
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "label" ], 3, 0, 1, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "editSyntax" ], 3, 1, 1, 3)
   qObj[ "labelEntries"       ]:setMaximumSize(QSize():new(70, 16777215))
   qObj[ "labelEntries"       ]:setAlignment(hb_bitOR(hb_bitOR(2,2),128))
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "labelEntries" ], 3, 4, 1, 1)
   qObj[ "line"               ]:setFrameShape(4)
   qObj[ "line"               ]:setFrameShadow(48)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "line" ], 5, 0, 1, 5)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonMark" ], 6, 0, 1, 2)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonLoad" ], 6, 2, 1, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonTag" ], 6, 3, 1, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonClose" ], 6, 4, 1, 1)
   qObj[ "listProjects"       ]:setMaximumSize(QSize():new(16777215, 100))
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "listProjects" ], 4, 0, 1, 5)
   qObj[ "Form"               ]:setWindowTitle(q__tr("Form", "Form", 0, "UTF8"))
   qObj[ "label"              ]:setText( [Syntax:] )
   qObj[ "labelEntries"       ]:setText( [] )
   qObj[ "buttonMark"         ]:setText( [Mark Projects] )
   qObj[ "buttonLoad"         ]:setText( [Load Tags] )
   qObj[ "buttonTag"          ]:setText( [Re-Tag] )
   qObj[ "buttonClose"        ]:setText( [Close] )

   oUI         := HbQtUI():new()
   oUI:qObj    := qObj
   oUI:oWidget := oWidget

   RETURN oUI

/*----------------------------------------------------------------------*/

