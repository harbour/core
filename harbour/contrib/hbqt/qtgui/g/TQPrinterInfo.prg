/*
 * $Id$
 */

/* -------------------------------------------------------------------- */
/* WARNING: Automatically generated source file. DO NOT EDIT!           */
/*          Instead, edit corresponding .qth file,                      */
/*          or the generator tool itself, and run regenarate.           */
/* -------------------------------------------------------------------- */

/*
 * Harbour Project QT wrapper
 *
 * Copyright 2009-2010 Pritpal Bedi <bedipritpal@hotmail.com>
 * www - http://harbour-project.org
 *
 * For full copyright message and credits, see: CREDITS.txt
 *
 */


#include "hbclass.ch"


REQUEST __HBQTGUI


FUNCTION QPrinterInfo( ... )
   RETURN HB_QPrinterInfo():new( ... )

FUNCTION QPrinterInfoFromPointer( ... )
   RETURN HB_QPrinterInfo():fromPointer( ... )


CREATE CLASS QPrinterInfo INHERIT HbQtObjectHandler FUNCTION HB_QPrinterInfo

   METHOD  new( ... )

   METHOD  isDefault                     // (  )                                               -> lBool
   METHOD  isNull                        // (  )                                               -> lBool
   METHOD  printerName                   // (  )                                               -> cQString
   METHOD  availablePrinters             // (  )                                               -> oQList_QPrinterInfo>
   METHOD  defaultPrinter                // (  )                                               -> oQPrinterInfo

   ENDCLASS


METHOD QPrinterInfo:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QPrinterInfo( ... )
   RETURN Self


METHOD QPrinterInfo:isDefault( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinterInfo_isDefault( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinterInfo:isNull( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinterInfo_isNull( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinterInfo:printerName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QPrinterInfo_printerName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinterInfo:availablePrinters( ... )
   SWITCH PCount()
   CASE 0
      RETURN QListFromPointer( Qt_QPrinterInfo_availablePrinters( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QPrinterInfo:defaultPrinter( ... )
   SWITCH PCount()
   CASE 0
      RETURN QPrinterInfoFromPointer( Qt_QPrinterInfo_defaultPrinter( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()

