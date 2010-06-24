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

FUNCTION uiThemes( qParent )
   LOCAL oUI
   LOCAL oWidget
   LOCAL qObj := {=>}

   hb_hCaseMatch( qObj, .f. )

   oWidget := QDialog():new( qParent )
  
   oWidget:setObjectName( "dialogThemes" )
  
   qObj[ "dialogThemes"       ] := oWidget
  
   qObj[ "verticalLayout_2"   ] := QVBoxLayout():new(qObj[ "dialogThemes" ])
   qObj[ "verticalLayout"     ] := QVBoxLayout():new()
   qObj[ "horizontalLayout_6" ] := QHBoxLayout():new()
   qObj[ "labelTheme"         ] := QLabel():new(qObj[ "dialogThemes" ])
   qObj[ "comboThemes"        ] := QComboBox():new(qObj[ "dialogThemes" ])
   qObj[ "horizontalLayout_5" ] := QHBoxLayout():new()
   qObj[ "plainTextEdit"      ] := QPlainTextEdit():new(qObj[ "dialogThemes" ])
   qObj[ "horizontalLayout_4" ] := QHBoxLayout():new()
   qObj[ "labelItem"          ] := QLabel():new(qObj[ "dialogThemes" ])
   qObj[ "comboItems"         ] := QComboBox():new(qObj[ "dialogThemes" ])
   qObj[ "buttonColor"        ] := QPushButton():new(qObj[ "dialogThemes" ])
   qObj[ "horizontalLayout_3" ] := QHBoxLayout():new()
   qObj[ "horizontalSpacer"   ] := QSpacerItem():new(50, 20, 0, 1)
   qObj[ "checkBold"          ] := QCheckBox():new(qObj[ "dialogThemes" ])
   qObj[ "checkItalic"        ] := QCheckBox():new(qObj[ "dialogThemes" ])
   qObj[ "checkUnderline"     ] := QCheckBox():new(qObj[ "dialogThemes" ])
   qObj[ "horizontalSpacer_2" ] := QSpacerItem():new(120, 20, 0, 1)
   qObj[ "horizontalLayout_2" ] := QHBoxLayout():new()
   qObj[ "line"               ] := QFrame():new(qObj[ "dialogThemes" ])
   qObj[ "horizontalLayout"   ] := QHBoxLayout():new()
   qObj[ "buttonSave"         ] := QPushButton():new(qObj[ "dialogThemes" ])
   qObj[ "buttonSaveAs"       ] := QPushButton():new(qObj[ "dialogThemes" ])
   qObj[ "buttonCopy"         ] := QPushButton():new(qObj[ "dialogThemes" ])
   qObj[ "buttonApply"        ] := QPushButton():new(qObj[ "dialogThemes" ])
   qObj[ "buttonClose"        ] := QPushButton():new(qObj[ "dialogThemes" ])
   
   qObj[ "dialogThemes"       ]:resize(430, 385)
   qObj[ "verticalLayout"     ]:setSizeConstraint(0)
   qObj[ "horizontalLayout_6" ]:setSpacing(10)
   qObj[ "labelTheme"         ]:setMaximumSize(QSize():new(40, 16777215))
   qObj[ "horizontalLayout_6" ]:addWidget(qObj[ "labelTheme" ])
   qObj[ "horizontalLayout_6" ]:addWidget(qObj[ "comboThemes" ])
   qObj[ "verticalLayout"     ]:addLayout(qObj[ "horizontalLayout_6" ])
   qObj[ "horizontalLayout_5" ]:addWidget(qObj[ "plainTextEdit" ])
   qObj[ "verticalLayout"     ]:addLayout(qObj[ "horizontalLayout_5" ])
   qObj[ "horizontalLayout_4" ]:setSpacing(10)
   qObj[ "labelItem"          ]:setMaximumSize(QSize():new(40, 16777215))
   qObj[ "horizontalLayout_4" ]:addWidget(qObj[ "labelItem" ])
   qObj[ "horizontalLayout_4" ]:addWidget(qObj[ "comboItems" ])
   qObj[ "buttonColor"        ]:setMaximumSize(QSize():new(120, 16777215))
   qObj[ "horizontalLayout_4" ]:addWidget(qObj[ "buttonColor" ])
   qObj[ "verticalLayout"     ]:addLayout(qObj[ "horizontalLayout_4" ])
   qObj[ "horizontalLayout_3" ]:addItem(qObj[ "horizontalSpacer" ])
   qObj[ "horizontalLayout_3" ]:addWidget(qObj[ "checkBold" ])
   qObj[ "horizontalLayout_3" ]:addWidget(qObj[ "checkItalic" ])
   qObj[ "horizontalLayout_3" ]:addWidget(qObj[ "checkUnderline" ])
   qObj[ "horizontalLayout_3" ]:addItem(qObj[ "horizontalSpacer_2" ])
   qObj[ "verticalLayout"     ]:addLayout(qObj[ "horizontalLayout_3" ])
   qObj[ "line"               ]:setFrameShape(4)
   qObj[ "line"               ]:setFrameShadow(48)
   qObj[ "horizontalLayout_2" ]:addWidget(qObj[ "line" ])
   qObj[ "verticalLayout"     ]:addLayout(qObj[ "horizontalLayout_2" ])
   qObj[ "horizontalLayout"   ]:setSpacing(10)
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonSave" ])
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonSaveAs" ])
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonCopy" ])
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonApply" ])
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonClose" ])
   qObj[ "verticalLayout"     ]:addLayout(qObj[ "horizontalLayout" ])
   qObj[ "verticalLayout"     ]:setStretch(1, 1)
   qObj[ "verticalLayout_2"   ]:addLayout(qObj[ "verticalLayout" ])
   qObj[ "dialogThemes"       ]:setWindowTitle(q__tr("dialogThemes", "HBIDE - Source Syntax Highlighting", 0, "UTF8"))
   qObj[ "labelTheme"         ]:setText( [Theme] )
   qObj[ "labelItem"          ]:setText( [Item] )
   qObj[ "buttonColor"        ]:setText( [Color] )
   qObj[ "checkBold"          ]:setText( [Bold] )
   qObj[ "checkItalic"        ]:setText( [Italic] )
   qObj[ "checkUnderline"     ]:setText( [Underline] )
   qObj[ "buttonSave"         ]:setText( [Save] )
   qObj[ "buttonSaveAs"       ]:setText( [Save As] )
   qObj[ "buttonCopy"         ]:setText( [Copy] )
   qObj[ "buttonApply"        ]:setText( [Apply] )
   qObj[ "buttonClose"        ]:setText( [Close] )

   oUI         := HbQtUI():new()
   oUI:qObj    := qObj
   oUI:oWidget := oWidget

   RETURN oUI

/*----------------------------------------------------------------------*/

