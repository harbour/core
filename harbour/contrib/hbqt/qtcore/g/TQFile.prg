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


FUNCTION QFile( ... )
   RETURN HB_QFile():new( ... )

FUNCTION QFileFromPointer( ... )
   RETURN HB_QFile():fromPointer( ... )


CREATE CLASS QFile INHERIT HbQtObjectHandler, HB_QIODevice FUNCTION HB_QFile

   METHOD  new( ... )

   METHOD  atEnd                         // (  )                                               -> lBool
   METHOD  close                         // (  )                                               -> NIL
   METHOD  copy                          // ( cNewName )                                       -> lBool
   METHOD  error                         // (  )                                               -> nFileError
   METHOD  exists                        // (  )                                               -> lBool
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  flush                         // (  )                                               -> lBool
   METHOD  handle                        // (  )                                               -> nInt
   METHOD  isSequential                  // (  )                                               -> lBool
   METHOD  link                          // ( cLinkName )                                      -> lBool
   METHOD  map                           // ( nOffset, nSize, nFlags )                         -> cUchar
   METHOD  open                          // ( nMode )                                          -> lBool
                                         // ( nFd, nMode )                                     -> lBool
   METHOD  permissions                   // (  )                                               -> nPermissions
   METHOD  remove                        // (  )                                               -> lBool
   METHOD  rename                        // ( cNewName )                                       -> lBool
   METHOD  resize                        // ( nSz )                                            -> lBool
   METHOD  setFileName                   // ( cName )                                          -> NIL
   METHOD  setPermissions                // ( nPermissions )                                   -> lBool
   METHOD  size                          // (  )                                               -> nQint64
   METHOD  symLinkTarget                 // (  )                                               -> cQString
   METHOD  unsetError                    // (  )                                               -> NIL
                                         // ( cFileName, cNewName )                            -> lBool
   METHOD  decodeName                    // ( cLocalFileName )                                 -> cQString
   METHOD  encodeName                    // ( cFileName )                                      -> oQByteArray
                                         // ( cFileName )                                      -> lBool
                                         // ( cFileName, cLinkName )                           -> lBool
                                         // ( cFileName )                                      -> nPermissions
                                         // ( cFileName )                                      -> lBool
                                         // ( cOldName, cNewName )                             -> lBool
                                         // ( cFileName, nSz )                                 -> lBool
                                         // ( cFileName, nPermissions )                        -> lBool
                                         // ( cFileName )                                      -> cQString

   ENDCLASS


METHOD QFile:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFile( ... )
   RETURN Self


METHOD QFile:atEnd( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_atEnd( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:close( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_close( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:copy( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFile_copy_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_copy( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:error( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_error( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:exists( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_exists_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFile_exists( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:flush( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_flush( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:handle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_handle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:isSequential( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_isSequential( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:link( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFile_link_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_link( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:map( ... )
   SWITCH PCount()
   CASE 3
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) ) .AND. hb_isNumeric( hb_pvalue( 3 ) )
         RETURN Qt_QFile_map( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFile_map( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:open( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFile_open_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFile_open( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:permissions( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_permissions_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFile_permissions( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:remove( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_remove_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFile_remove( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:rename( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFile_rename_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_rename( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:resize( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFile_resize_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFile_resize( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:setFileName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_setFileName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:setPermissions( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) ) .AND. hb_isNumeric( hb_pvalue( 2 ) )
         RETURN Qt_QFile_setPermissions_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFile_setPermissions( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_size( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:symLinkTarget( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_symLinkTarget_1( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 0
      RETURN Qt_QFile_symLinkTarget( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:unsetError( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFile_unsetError( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:decodeName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFile_decodeName( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFile:encodeName( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN QByteArrayFromPointer( Qt_QFile_encodeName( ::pPtr, ... ) )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()

