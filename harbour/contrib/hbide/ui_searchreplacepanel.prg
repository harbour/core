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

FUNCTION uiSearchreplacepanel( qParent )
   LOCAL oUI
   LOCAL oWidget
   LOCAL qObj := {=>}

   hb_hCaseMatch( qObj, .f. )

   oWidget := QWidget():new( qParent )
  
   oWidget:setObjectName( "FormSearchReplace" )
  
   qObj[ "FormSearchReplace"  ] := oWidget
  
   qObj[ "horizontalLayout"   ] := QHBoxLayout():new(qObj[ "FormSearchReplace" ])
   qObj[ "label"              ] := QLabel():new(qObj[ "FormSearchReplace" ])
   qObj[ "comboFind"          ] := QComboBox():new(qObj[ "FormSearchReplace" ])
   qObj[ "sizePolicy"         ] := QSizePolicy():new(3, 0)
   qObj[ "__qsizePolicy102"   ] := QSizePolicy():configure(qObj[ "comboFind" ]:sizePolicy())
   qObj[ "buttonNext"         ] := QToolButton():new(qObj[ "FormSearchReplace" ])
   qObj[ "buttonPrev"         ] := QToolButton():new(qObj[ "FormSearchReplace" ])
   qObj[ "buttonTop"          ] := QToolButton():new(qObj[ "FormSearchReplace" ])
   qObj[ "checkMatchCase"     ] := QCheckBox():new(qObj[ "FormSearchReplace" ])
   qObj[ "checkRegEx"         ] := QCheckBox():new(qObj[ "FormSearchReplace" ])
   qObj[ "label_2"            ] := QLabel():new(qObj[ "FormSearchReplace" ])
   qObj[ "checkReplace"       ] := QCheckBox():new(qObj[ "FormSearchReplace" ])
   qObj[ "comboReplace"       ] := QComboBox():new(qObj[ "FormSearchReplace" ])
   qObj[ "__qsizePolicy103"   ] := QSizePolicy():configure(qObj[ "comboReplace" ]:sizePolicy())
   qObj[ "checkGlobal"        ] := QCheckBox():new(qObj[ "FormSearchReplace" ])
   qObj[ "checkNoPrompt"      ] := QCheckBox():new(qObj[ "FormSearchReplace" ])
   qObj[ "horizontalSpacer"   ] := QSpacerItem():new(40, 20, 7, 1)
   qObj[ "buttonClose"        ] := QToolButton():new(qObj[ "FormSearchReplace" ])
   
   qObj[ "FormSearchReplace"  ]:resize(923, 38)
   qObj[ "FormSearchReplace"  ]:setMinimumSize(QSize():new(0, 30))
   qObj[ "FormSearchReplace"  ]:setMaximumSize(QSize():new(16777215, 38))
   qObj[ "FormSearchReplace"  ]:setStyleSheet( [] )
   qObj[ "horizontalLayout"   ]:setContentsMargins(-1, 2, -1, 2)
   qObj[ "label"              ]:setMaximumSize(QSize():new(25, 20))
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "label" ])
   qObj[ "sizePolicy"         ]:setHorizontalStretch(0)
   qObj[ "sizePolicy"         ]:setVerticalStretch(0)
   qObj[ "sizePolicy"         ]:setHeightForWidth(qObj[ "__qsizePolicy102" ]:hasHeightForWidth())
   qObj[ "comboFind"          ]:setSizePolicy(qObj[ "sizePolicy" ])
   qObj[ "comboFind"          ]:setMinimumSize(QSize():new(200, 0))
   qObj[ "comboFind"          ]:setMaximumSize(QSize():new(16777215, 16777215))
   qObj[ "comboFind"          ]:setEditable(.T.)
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "comboFind" ])
   qObj[ "buttonNext"         ]:setAutoRaise(.T.)
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonNext" ])
   qObj[ "buttonPrev"         ]:setAutoRaise(.T.)
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonPrev" ])
   qObj[ "buttonTop"          ]:setAutoRaise(.T.)
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonTop" ])
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "checkMatchCase" ])
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "checkRegEx" ])
   qObj[ "label_2"            ]:setMinimumSize(QSize():new(30, 20))
   qObj[ "label_2"            ]:setMaximumSize(QSize():new(30, 20))
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "label_2" ])
   qObj[ "checkReplace"       ]:setMinimumSize(QSize():new(62, 0))
   qObj[ "checkReplace"       ]:setMaximumSize(QSize():new(62, 16777215))
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "checkReplace" ])
   qObj[ "sizePolicy"         ]:setHeightForWidth(qObj[ "__qsizePolicy103" ]:hasHeightForWidth())
   qObj[ "comboReplace"       ]:setSizePolicy(qObj[ "sizePolicy" ])
   qObj[ "comboReplace"       ]:setMinimumSize(QSize():new(200, 0))
   qObj[ "comboReplace"       ]:setMaximumSize(QSize():new(16777215, 16777215))
   qObj[ "comboReplace"       ]:setEditable(.T.)
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "comboReplace" ])
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "checkGlobal" ])
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "checkNoPrompt" ])
   qObj[ "horizontalLayout"   ]:addItem(qObj[ "horizontalSpacer" ])
   qObj[ "buttonClose"        ]:setAutoRaise(.T.)
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonClose" ])
   qObj[ "FormSearchReplace"  ]:setWindowTitle(q__tr("FormSearchReplace", "Form", 0, "UTF8"))
   qObj[ "label"              ]:setText( [Find:] )
   qObj[ "buttonNext"         ]:setText( [...] )
   qObj[ "buttonPrev"         ]:setText( [...] )
   qObj[ "buttonTop"          ]:setText( [...] )
   qObj[ "checkMatchCase"     ]:setText( [Case] )
   qObj[ "checkRegEx"         ]:setText( [RegEx] )
   qObj[ "label_2"            ]:setText( [] )
   qObj[ "checkReplace"       ]:setText( [Replace:] )
   qObj[ "checkGlobal"        ]:setText( [Global] )
   qObj[ "checkNoPrompt"      ]:setText( [NoPrompt] )
   qObj[ "buttonClose"        ]:setText( [...] )

   oUI         := HbQtUI():new()
   oUI:qObj    := qObj
   oUI:oWidget := oWidget

   RETURN oUI

/*----------------------------------------------------------------------*/

