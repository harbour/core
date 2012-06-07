/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2012 Carlos Bacco <carlosbacco at gmail.com>
 * www - http://harbour-project.org
 *
 *
 * This sample demonstrates the embedding of external Qt resources
 * inside harbour executables. The two images used in this sample
 * are listed in the file testres.qrc, and when compiled, the 
 * program will display correctly the images even if you delete
 * the original files used, as a copy of them will be inside the
 * executable.
 *
 * Remember that the Qt resource compiler needs to be in the path,
 * as it will be required to compile the sample correctly.
 *
 */

#include "hbqtgui.ch"

PROCEDURE Main()
   LOCAL oWid
   LOCAL oRes

   oRes := QResource()
   oRes:registerResource_1( HBQTRES_TESTRES() ) // HBQTRES_filename_without_qrc_extension()

   oWid := QLabel()
   oWid:setWindowIcon( QIcon( ":harbour-icon.png" ) )
   oWid:setAlignment( hb_bitOr( Qt_AlignHCenter, Qt_AlignVCenter ) )
   oWid:setPixMap( QPixMap( ":harbour-logo.png" ) )

   oWid:Show()

   QApplication():exec()
   oRes:unregisterResource_1( HBQTRES_TESTRES() )
   RETURN

