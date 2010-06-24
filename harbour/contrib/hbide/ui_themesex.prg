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

FUNCTION uiThemesex( qParent )
   LOCAL oUI
   LOCAL oWidget
   LOCAL qObj := {=>}

   hb_hCaseMatch( qObj, .f. )

   oWidget := QWidget():new( qParent )
  
   oWidget:setObjectName( "FormThemes" )
  
   qObj[ "FormThemes"         ] := oWidget
  
   qObj[ "gridLayout"         ] := QGridLayout():new(qObj[ "FormThemes" ])
   qObj[ "labelTheme"         ] := QLabel():new(qObj[ "FormThemes" ])
   qObj[ "sizePolicy"         ] := QSizePolicy():new(5, 0)
   qObj[ "__qsizePolicy104"   ] := QSizePolicy():configure(qObj[ "labelTheme" ]:sizePolicy())
   qObj[ "labelItems"         ] := QLabel():new(qObj[ "FormThemes" ])
   qObj[ "__qsizePolicy105"   ] := QSizePolicy():configure(qObj[ "labelItems" ]:sizePolicy())
   qObj[ "listThemes"         ] := QListWidget():new(qObj[ "FormThemes" ])
   qObj[ "listItems"          ] := QListWidget():new(qObj[ "FormThemes" ])
   qObj[ "checkBold"          ] := QCheckBox():new(qObj[ "FormThemes" ])
   qObj[ "checkItalic"        ] := QCheckBox():new(qObj[ "FormThemes" ])
   qObj[ "checkUnderline"     ] := QCheckBox():new(qObj[ "FormThemes" ])
   qObj[ "buttonColor"        ] := QPushButton():new(qObj[ "FormThemes" ])
   qObj[ "label"              ] := QLabel():new(qObj[ "FormThemes" ])
   qObj[ "__qsizePolicy106"   ] := QSizePolicy():configure(qObj[ "label" ]:sizePolicy())
   qObj[ "plainThemeText"     ] := QPlainTextEdit():new(qObj[ "FormThemes" ])
   qObj[ "buttonSave"         ] := QPushButton():new(qObj[ "FormThemes" ])
   qObj[ "buttonSaveAs"       ] := QPushButton():new(qObj[ "FormThemes" ])
   qObj[ "buttonCopy"         ] := QPushButton():new(qObj[ "FormThemes" ])
   qObj[ "buttonDefault"      ] := QPushButton():new(qObj[ "FormThemes" ])
   qObj[ "buttonApply"        ] := QPushButton():new(qObj[ "FormThemes" ])
   qObj[ "buttonApplyAll"     ] := QPushButton():new(qObj[ "FormThemes" ])
   qObj[ "buttonClose"        ] := QPushButton():new(qObj[ "FormThemes" ])
   
   qObj[ "FormThemes"         ]:resize(423, 451)
   qObj[ "sizePolicy"         ]:setHorizontalStretch(0)
   qObj[ "sizePolicy"         ]:setVerticalStretch(0)
   qObj[ "sizePolicy"         ]:setHeightForWidth(qObj[ "__qsizePolicy104" ]:hasHeightForWidth())
   qObj[ "labelTheme"         ]:setSizePolicy(qObj[ "sizePolicy" ])
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "labelTheme" ], 0, 0, 1, 1)
   qObj[ "sizePolicy"         ]:setHeightForWidth(qObj[ "__qsizePolicy105" ]:hasHeightForWidth())
   qObj[ "labelItems"         ]:setSizePolicy(qObj[ "sizePolicy" ])
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "labelItems" ], 0, 1, 1, 1)
   qObj[ "listThemes"         ]:setMinimumSize(QSize():new(0, 0))
   qObj[ "listThemes"         ]:setMaximumSize(QSize():new(16777215, 16777215))
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "listThemes" ], 1, 0, 4, 1)
   qObj[ "listItems"          ]:setMinimumSize(QSize():new(0, 0))
   qObj[ "listItems"          ]:setMaximumSize(QSize():new(16777215, 16777215))
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "listItems" ], 1, 1, 4, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "checkBold" ], 1, 2, 1, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "checkItalic" ], 2, 2, 1, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "checkUnderline" ], 3, 2, 1, 1)
   qObj[ "buttonColor"        ]:setMaximumSize(QSize():new(120, 16777215))
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonColor" ], 4, 2, 1, 1)
   qObj[ "sizePolicy"         ]:setHeightForWidth(qObj[ "__qsizePolicy106" ]:hasHeightForWidth())
   qObj[ "label"              ]:setSizePolicy(qObj[ "sizePolicy" ])
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "label" ], 5, 0, 1, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "plainThemeText" ], 6, 0, 7, 2)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonSave" ], 6, 2, 1, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonSaveAs" ], 7, 2, 1, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonCopy" ], 8, 2, 1, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonDefault" ], 9, 2, 1, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonApply" ], 10, 2, 1, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonApplyAll" ], 11, 2, 1, 1)
   qObj[ "gridLayout"         ]:addWidget_1(qObj[ "buttonClose" ], 12, 2, 1, 1)
   qObj[ "FormThemes"         ]:setWindowTitle(q__tr("FormThemes", "Form", 0, "UTF8"))
   qObj[ "labelTheme"         ]:setText( [Themes] )
   qObj[ "labelItems"         ]:setText( [Items] )
   qObj[ "checkBold"          ]:setText( [Bold] )
   qObj[ "checkItalic"        ]:setText( [Italic] )
   qObj[ "checkUnderline"     ]:setText( [Underline] )
   qObj[ "buttonColor"        ]:setText( [Color] )
   qObj[ "label"              ]:setText( [Preview] )
   qObj[ "buttonSave"         ]:setText( [Save] )
   qObj[ "buttonSaveAs"       ]:setText( [Save As] )
   qObj[ "buttonCopy"         ]:setText( [Copy] )
   qObj[ "buttonDefault"      ]:setText( [SetAsDefault] )
   qObj[ "buttonApply"        ]:setText( [ApplyCurrent] )
   qObj[ "buttonApplyAll"     ]:setText( [ApplyAll] )
   qObj[ "buttonClose"        ]:setText( [Close] )

   oUI         := HbQtUI():new()
   oUI:qObj    := qObj
   oUI:oWidget := oWidget

   RETURN oUI

/*----------------------------------------------------------------------*/

