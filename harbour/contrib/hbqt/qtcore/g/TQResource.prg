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


REQUEST __HBQTCORE


FUNCTION QResource( ... )
   RETURN HB_QResource():new( ... )

FUNCTION QResourceFromPointer( ... )
   RETURN HB_QResource():fromPointer( ... )


CREATE CLASS QResource INHERIT HbQtObjectHandler FUNCTION HB_QResource

   METHOD  new( ... )

   METHOD  absoluteFilePath              // (  )                                               -> cQString
   METHOD  data                          // (  )                                               -> cUchar
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  isCompressed                  // (  )                                               -> lBool
   METHOD  isValid                       // (  )                                               -> lBool
   METHOD  locale                        // (  )                                               -> oQLocale
   METHOD  setFileName                   // ( cFile )                                          -> NIL
   METHOD  setLocale                     // ( oQLocale )                                       -> NIL
   METHOD  size                          // (  )                                               -> nQint64
   METHOD  registerResource              // ( cRccFileName, cMapRoot )                         -> lBool
   METHOD  registerResource_1            // ( ouchar, cMapRoot )                               -> lBool
   METHOD  searchPaths                   // (  )                                               -> oQStringList
   METHOD  unregisterResource            // ( cRccFileName, cMapRoot )                         -> lBool
   METHOD  unregisterResource_1          // ( ouchar, cMapRoot )                               -> lBool

   ENDCLASS


METHOD QResource:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QResource( ... )
   RETURN Self


METHOD QResource:absoluteFilePath( ... )
   RETURN Qt_QResource_absoluteFilePath( ::pPtr, ... )


METHOD QResource:data( ... )
   RETURN Qt_QResource_data( ::pPtr, ... )


METHOD QResource:fileName( ... )
   RETURN Qt_QResource_fileName( ::pPtr, ... )


METHOD QResource:isCompressed( ... )
   RETURN Qt_QResource_isCompressed( ::pPtr, ... )


METHOD QResource:isValid( ... )
   RETURN Qt_QResource_isValid( ::pPtr, ... )


METHOD QResource:locale( ... )
   RETURN QLocaleFromPointer( Qt_QResource_locale( ::pPtr, ... ) )


METHOD QResource:setFileName( ... )
   RETURN Qt_QResource_setFileName( ::pPtr, ... )


METHOD QResource:setLocale( ... )
   RETURN Qt_QResource_setLocale( ::pPtr, ... )


METHOD QResource:size( ... )
   RETURN Qt_QResource_size( ::pPtr, ... )


METHOD QResource:registerResource( ... )
   RETURN Qt_QResource_registerResource( ::pPtr, ... )


METHOD QResource:registerResource_1( ... )
   RETURN Qt_QResource_registerResource_1( ::pPtr, ... )


METHOD QResource:searchPaths( ... )
   RETURN QStringListFromPointer( Qt_QResource_searchPaths( ::pPtr, ... ) )


METHOD QResource:unregisterResource( ... )
   RETURN Qt_QResource_unregisterResource( ::pPtr, ... )


METHOD QResource:unregisterResource_1( ... )
   RETURN Qt_QResource_unregisterResource_1( ::pPtr, ... )

