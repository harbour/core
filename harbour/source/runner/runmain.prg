// $Id$
//
#include "external.ch"

PROCEDURE MAIN( cHRBFile )

    IF( ! EMPTY(cHRBFile) )
	HB_RUN( cHRBFile )
    ELSE
	?
	? "Usage:"
	? "./runmain hrbfile"
	?
    ENDIF
  
RETURN