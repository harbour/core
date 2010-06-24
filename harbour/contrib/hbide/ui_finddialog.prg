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

FUNCTION uiFinddialog( qParent )
   LOCAL oUI
   LOCAL oWidget
   LOCAL qObj := {=>}

   hb_hCaseMatch( qObj, .f. )

   oWidget := QDialog():new( qParent )
  
   oWidget:setObjectName( "DialogFind" )
  
   qObj[ "DialogFind"         ] := oWidget
  
   qObj[ "buttonFind"         ] := QPushButton():new(qObj[ "DialogFind" ])
   qObj[ "buttonReplace"      ] := QPushButton():new(qObj[ "DialogFind" ])
   qObj[ "buttonClose"        ] := QPushButton():new(qObj[ "DialogFind" ])
   qObj[ "comboReplaceWith"   ] := QComboBox():new(qObj[ "DialogFind" ])
   qObj[ "checkGlobal"        ] := QCheckBox():new(qObj[ "DialogFind" ])
   qObj[ "checkNoPrompting"   ] := QCheckBox():new(qObj[ "DialogFind" ])
   qObj[ "label_2"            ] := QLabel():new(qObj[ "DialogFind" ])
   qObj[ "groupBox_2"         ] := QGroupBox():new(qObj[ "DialogFind" ])
   qObj[ "radioFromCursor"    ] := QRadioButton():new(qObj[ "groupBox_2" ])
   qObj[ "radioEntire"        ] := QRadioButton():new(qObj[ "groupBox_2" ])
   qObj[ "groupBox_3"         ] := QGroupBox():new(qObj[ "DialogFind" ])
   qObj[ "radioUp"            ] := QRadioButton():new(qObj[ "groupBox_3" ])
   qObj[ "radioDown"          ] := QRadioButton():new(qObj[ "groupBox_3" ])
   qObj[ "checkMatchCase"     ] := QCheckBox():new(qObj[ "DialogFind" ])
   qObj[ "checkListOnly"      ] := QCheckBox():new(qObj[ "DialogFind" ])
   qObj[ "comboFindWhat"      ] := QComboBox():new(qObj[ "DialogFind" ])
   qObj[ "label"              ] := QLabel():new(qObj[ "DialogFind" ])
   
   qObj[ "DialogFind"         ]:resize(415, 166)
   qObj[ "buttonFind"         ]:setGeometry(QRect():new(332, 8, 75, 24))
   qObj[ "buttonFind"         ]:setAutoDefault(.F.)
   qObj[ "buttonFind"         ]:setDefault(.T.)
   qObj[ "buttonReplace"      ]:setGeometry(QRect():new(332, 106, 75, 24))
   qObj[ "buttonReplace"      ]:setAutoDefault(.F.)
   qObj[ "buttonReplace"      ]:setFlat(.F.)
   qObj[ "buttonClose"        ]:setGeometry(QRect():new(332, 136, 77, 24))
   qObj[ "comboReplaceWith"   ]:setGeometry(QRect():new(70, 108, 253, 22))
   qObj[ "comboReplaceWith"   ]:setEditable(.T.)
   qObj[ "checkGlobal"        ]:setGeometry(QRect():new(70, 136, 71, 19))
   qObj[ "checkNoPrompting"   ]:setGeometry(QRect():new(192, 136, 89, 19))
   qObj[ "label_2"            ]:setGeometry(QRect():new(12, 108, 53, 20))
   qObj[ "groupBox_2"         ]:setGeometry(QRect():new(70, 60, 161, 41))
   qObj[ "radioFromCursor"    ]:setGeometry(QRect():new(8, 4, 141, 19))
   qObj[ "radioEntire"        ]:setGeometry(QRect():new(8, 20, 141, 19))
   qObj[ "groupBox_3"         ]:setGeometry(QRect():new(240, 60, 81, 41))
   qObj[ "radioUp"            ]:setGeometry(QRect():new(10, 4, 41, 19))
   qObj[ "radioDown"          ]:setGeometry(QRect():new(10, 20, 51, 19))
   qObj[ "checkMatchCase"     ]:setGeometry(QRect():new(70, 36, 81, 19))
   qObj[ "checkListOnly"      ]:setGeometry(QRect():new(192, 36, 71, 19))
   qObj[ "comboFindWhat"      ]:setGeometry(QRect():new(70, 10, 253, 22))
   qObj[ "comboFindWhat"      ]:setEditable(.T.)
   qObj[ "label"              ]:setGeometry(QRect():new(10, 10, 51, 20))
   qObj[ "DialogFind"         ]:setWindowTitle(q__tr("DialogFind", "Find and Replace", 0, "UTF8"))
   qObj[ "buttonFind"         ]:setText( [Find] )
   qObj[ "buttonReplace"      ]:setText( [Replace] )
   qObj[ "buttonClose"        ]:setText( [Close] )
   qObj[ "checkGlobal"        ]:setText( [Global] )
   qObj[ "checkNoPrompting"   ]:setText( [No prompting] )
   qObj[ "label_2"            ]:setText( [Replace:] )
   qObj[ "groupBox_2"         ]:setTitle("")
   qObj[ "radioFromCursor"    ]:setText( [Start from cursor position] )
   qObj[ "radioEntire"        ]:setText( [Entire file] )
   qObj[ "groupBox_3"         ]:setTitle("")
   qObj[ "radioUp"            ]:setText( [up] )
   qObj[ "radioDown"          ]:setText( [Down] )
   qObj[ "checkMatchCase"     ]:setText( [Match case] )
   qObj[ "checkListOnly"      ]:setText( [List only] )
   qObj[ "label"              ]:setText( [Find what:] )

   oUI         := HbQtUI():new()
   oUI:qObj    := qObj
   oUI:oWidget := oWidget

   RETURN oUI

/*----------------------------------------------------------------------*/

