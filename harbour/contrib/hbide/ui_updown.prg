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

FUNCTION uiUpdown( qParent )
   LOCAL oUI
   LOCAL oWidget
   LOCAL qObj := {=>}

   hb_hCaseMatch( qObj, .f. )

   oWidget := QWidget():new( qParent )
  
   oWidget:setObjectName( "Form" )
  
   qObj[ "Form"               ] := oWidget
  
   qObj[ "horizontalLayout"   ] := QHBoxLayout():new(qObj[ "Form" ])
   qObj[ "buttonNext"         ] := QToolButton():new(qObj[ "Form" ])
   qObj[ "buttonPrev"         ] := QToolButton():new(qObj[ "Form" ])
   qObj[ "buttonLast"         ] := QToolButton():new(qObj[ "Form" ])
   qObj[ "buttonFirst"        ] := QToolButton():new(qObj[ "Form" ])
   qObj[ "buttonAll"          ] := QToolButton():new(qObj[ "Form" ])
   
   qObj[ "Form"               ]:resize(135, 24)
   qObj[ "horizontalLayout"   ]:setSpacing(1)
   qObj[ "horizontalLayout"   ]:setContentsMargins(0, 2, 0, 2)
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonNext" ])
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonPrev" ])
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonLast" ])
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonFirst" ])
   qObj[ "horizontalLayout"   ]:addWidget(qObj[ "buttonAll" ])
   qObj[ "Form"               ]:setWindowTitle(q__tr("Form", "Form", 0, "UTF8"))
   qObj[ "buttonNext"         ]:setText( [...] )
   qObj[ "buttonPrev"         ]:setText( [...] )
   qObj[ "buttonLast"         ]:setText( [...] )
   qObj[ "buttonFirst"        ]:setText( [...] )
   qObj[ "buttonAll"          ]:setText( [...] )

   oUI         := HbQtUI():new()
   oUI:qObj    := qObj
   oUI:oWidget := oWidget

   RETURN oUI

/*----------------------------------------------------------------------*/

