/*
 * $Id$
 */

PROCEDURE Main( cZip, ... )

    ? hb_UnzipFile( cZip, NIL, .F., NIL, NIL, hb_AParams() )

    RETURN
