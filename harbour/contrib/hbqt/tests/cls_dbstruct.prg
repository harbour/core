/*
 * $Id$

 <CLASS> . Do not edit lines in this section!
 NAME = ui_dbstruct
 </CLASS>
 */
/*----------------------------------------------------------------------*/

#include "hbclass.ch"
#include "common.ch"
#include "error.ch"
#include "hbqtgui.ch"

/*----------------------------------------------------------------------*/

CLASS ui_dbstruct

   DATA   oParent
   
   /* <METHODSCOMMON> . Do not edit lines in this section! */
   METHOD new( oParent )
   METHOD create( oParent )
   METHOD destroy()
   METHOD connects()
   METHOD disconnects()
   ERROR HANDLER __OnError( ... )
   /* </METHODSCOMMON> */

   /* <METHODSEVENTS> . Do not edit lines in this section! */
   METHOD buttonAdd_Activated( ... )
   /* </METHODSEVENTS> */

PROTECTED:
   DATA   oUI

   ENDCLASS

/*----------------------------------------------------------------------*/

METHOD ui_dbstruct:new( oParent )

   DEFAULT oParent TO ::oParent
   ::oParent := oParent

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD ui_dbstruct:create( oParent )

   DEFAULT oParent TO ::oParent
   ::oParent := oParent

   ::oUI := hbqtui_dbstruct( ::oParent )

   ::connects()

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD ui_dbstruct:destroy()

   IF hb_isObject( ::oUI )
      ::disconnects()
      ::oUI:destroy()
   ENDIF

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD ui_dbstruct:__OnError( ... )
   LOCAL cMsg := __GetMessage()
   LOCAL oError

   IF SubStr( cMsg, 1, 1 ) == "_"
      cMsg := SubStr( cMsg, 2 )
   ENDIF

   IF Left( cMsg, 2 ) == "Q_"
      IF SubStr( cMsg, 3 ) $ ::oUI:qObj
         RETURN ::oUI:qObj[ SubStr( cMsg, 3 ) ]
      ELSE
         oError := ErrorNew()

         oError:severity    := ES_ERROR
         oError:genCode     := EG_ARG
         oError:subSystem   := "HBQT"
         oError:subCode     := 1001
         oError:canRetry    := .F.
         oError:canDefault  := .F.
         oError:Args        := hb_AParams()
         oError:operation   := ProcName()
         oError:Description := "Control <" + substr( cMsg, 3 ) + "> does not exist"

         Eval( ErrorBlock(), oError )
      ENDIF
   ELSEIF ::oUI:oWidget:hasValidPointer()
      RETURN ::oUI:oWidget:&cMsg( ... )
   ENDIF

   RETURN NIL

/*----------------------------------------------------------------------*/

METHOD ui_dbstruct:connects()

   /* <CONNECTS> . Do not edit lines in this section! */
   ::oUI:qObj[ "buttonAdd" ]:connect( "clicked()", {|...| ::buttonAdd_Activated( ... ) } )
   /* </CONNECTS> */

   RETURN Self

/*----------------------------------------------------------------------*/

METHOD ui_dbstruct:disconnects()

   /* <DISCONNECTS> . Do not edit lines in this section! */
   /* </DISCONNECTS> */

   RETURN Self

/*----------------------------------------------------------------------*/
/* <EVENTSMETHODAREA> . Do not edit code in this section! */
METHOD ui_dbstruct:buttonAdd_Activated( ... )
   Local oMsg
   
   oMsg := QMessageBox()
   oMsg:setText( "I am just clicked and fine " )
   oMsg:exec()
   
   RETURN Self
  
/* </EVENTSMETHODAREA> */
/*----------------------------------------------------------------------*/



