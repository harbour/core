/*
 * $Id$
 */

/*
 *    Pritpal Bedi <bedipritpal@hotmail.com>
 */
/*----------------------------------------------------------------------*/

#include "inkey.ch"
#include "common.ch"
#include "wvtwin.ch"
#include "hbgtinfo.ch"
#include "hbgtwvg.ch"
#include "wvgparts.ch"

/*----------------------------------------------------------------------*/

#ifdef __QT__
INIT PROCEDURE Qt_Start()
   qt_qapplication()
   RETURN

EXIT PROCEDURE Qt_End()
   qt_qapplication_exec()
   RETURN
#endif

//-------------------------------------------------------------------//

#ifdef __QT__
FUNCTION ExeQTWidgets()
   LOCAL oPS, oPPrv, oWZ, oCD, oWP

   oPS := QPageSetupDialog():new()
   oPS:setWindowTitle( 'Harbour-QT PageSetup Dialog' )
   oPS:show()

   oPPrv := QPrintPreviewDialog():new()
   oPPrv:setWindowTitle( 'Harbour-QT Preview Preview Dialog' )
   oPPrv:show()

   oWZ := QWizard():new()
   oWZ:setWindowTitle( 'Harbour-QT Wizard to Show Slides etc.' )
   oWZ:show()

   oCD := QColorDialog():new()
   oCD:setWindowTitle( 'Harbour-QT Color Selection Dialog' )
   oCD:show()

   oWP := QWebView():new()
   oWP:setWindowTitle( 'Harbour-QT Web Page Navigator' )
   oWP:show()

   RETURN NIL
#endif
/*----------------------------------------------------------------------*/
