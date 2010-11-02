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


FUNCTION QFileInfo( ... )
   RETURN HB_QFileInfo():new( ... )

FUNCTION QFileInfoFromPointer( ... )
   RETURN HB_QFileInfo():fromPointer( ... )


CREATE CLASS QFileInfo INHERIT HbQtObjectHandler FUNCTION HB_QFileInfo

   METHOD  new( ... )

   METHOD  absoluteDir                   // (  )                                               -> oQDir
   METHOD  absoluteFilePath              // (  )                                               -> cQString
   METHOD  absolutePath                  // (  )                                               -> cQString
   METHOD  baseName                      // (  )                                               -> cQString
   METHOD  bundleName                    // (  )                                               -> cQString
   METHOD  caching                       // (  )                                               -> lBool
   METHOD  canonicalFilePath             // (  )                                               -> cQString
   METHOD  canonicalPath                 // (  )                                               -> cQString
   METHOD  completeBaseName              // (  )                                               -> cQString
   METHOD  completeSuffix                // (  )                                               -> cQString
   METHOD  created                       // (  )                                               -> oQDateTime
   METHOD  dir                           // (  )                                               -> oQDir
   METHOD  exists                        // (  )                                               -> lBool
   METHOD  fileName                      // (  )                                               -> cQString
   METHOD  filePath                      // (  )                                               -> cQString
   METHOD  group                         // (  )                                               -> cQString
   METHOD  groupId                       // (  )                                               -> nUint
   METHOD  isAbsolute                    // (  )                                               -> lBool
   METHOD  isBundle                      // (  )                                               -> lBool
   METHOD  isDir                         // (  )                                               -> lBool
   METHOD  isExecutable                  // (  )                                               -> lBool
   METHOD  isFile                        // (  )                                               -> lBool
   METHOD  isHidden                      // (  )                                               -> lBool
   METHOD  isReadable                    // (  )                                               -> lBool
   METHOD  isRelative                    // (  )                                               -> lBool
   METHOD  isRoot                        // (  )                                               -> lBool
   METHOD  isSymLink                     // (  )                                               -> lBool
   METHOD  isWritable                    // (  )                                               -> lBool
   METHOD  lastModified                  // (  )                                               -> oQDateTime
   METHOD  lastRead                      // (  )                                               -> oQDateTime
   METHOD  makeAbsolute                  // (  )                                               -> lBool
   METHOD  owner                         // (  )                                               -> cQString
   METHOD  ownerId                       // (  )                                               -> nUint
   METHOD  path                          // (  )                                               -> cQString
   METHOD  permission                    // ( nPermissions )                                   -> lBool
   METHOD  permissions                   // (  )                                               -> nQFile_Permissions
   METHOD  refresh                       // (  )                                               -> NIL
   METHOD  setCaching                    // ( lEnable )                                        -> NIL
   METHOD  setFile                       // ( cFile )                                          -> NIL
                                         // ( oQFile )                                         -> NIL
                                         // ( oQDir, cFile )                                   -> NIL
   METHOD  size                          // (  )                                               -> nQint64
   METHOD  suffix                        // (  )                                               -> cQString
   METHOD  symLinkTarget                 // (  )                                               -> cQString

   ENDCLASS


METHOD QFileInfo:new( ... )
   LOCAL p
   FOR EACH p IN { ... }
      hb_pvalue( p:__enumIndex(), __hbqt_ptr( p ) )
   NEXT
   ::pPtr := Qt_QFileInfo( ... )
   RETURN Self


METHOD QFileInfo:absoluteDir( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDirFromPointer( Qt_QFileInfo_absoluteDir( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:absoluteFilePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_absoluteFilePath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:absolutePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_absolutePath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:baseName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_baseName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:bundleName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_bundleName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:caching( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_caching( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:canonicalFilePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_canonicalFilePath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:canonicalPath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_canonicalPath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:completeBaseName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_completeBaseName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:completeSuffix( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_completeSuffix( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:created( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QFileInfo_created( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:dir( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDirFromPointer( Qt_QFileInfo_dir( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:exists( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_exists( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:fileName( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_fileName( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:filePath( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_filePath( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:group( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_group( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:groupId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_groupId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:isAbsolute( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isAbsolute( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:isBundle( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isBundle( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:isDir( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isDir( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:isExecutable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isExecutable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:isFile( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isFile( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:isHidden( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isHidden( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:isReadable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isReadable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:isRelative( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isRelative( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:isRoot( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isRoot( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:isSymLink( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isSymLink( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:isWritable( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_isWritable( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:lastModified( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QFileInfo_lastModified( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:lastRead( ... )
   SWITCH PCount()
   CASE 0
      RETURN QDateTimeFromPointer( Qt_QFileInfo_lastRead( ::pPtr, ... ) )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:makeAbsolute( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_makeAbsolute( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:owner( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_owner( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:ownerId( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_ownerId( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:path( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_path( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:permission( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isNumeric( hb_pvalue( 1 ) )
         RETURN Qt_QFileInfo_permission( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:permissions( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_permissions( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:refresh( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_refresh( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:setCaching( ... )
   SWITCH PCount()
   CASE 1
      DO CASE
      CASE hb_isLogical( hb_pvalue( 1 ) )
         RETURN Qt_QFileInfo_setCaching( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:setFile( ... )
   SWITCH PCount()
   CASE 2
      DO CASE
      CASE hb_isObject( hb_pvalue( 1 ) ) .AND. hb_isChar( hb_pvalue( 2 ) )
         RETURN Qt_QFileInfo_setFile_2( ::pPtr, ... )
      ENDCASE
      EXIT
   CASE 1
      DO CASE
      CASE hb_isChar( hb_pvalue( 1 ) )
         RETURN Qt_QFileInfo_setFile( ::pPtr, ... )
      CASE hb_isObject( hb_pvalue( 1 ) )
         RETURN Qt_QFileInfo_setFile_1( ::pPtr, ... )
      ENDCASE
      EXIT
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:size( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_size( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:suffix( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_suffix( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()


METHOD QFileInfo:symLinkTarget( ... )
   SWITCH PCount()
   CASE 0
      RETURN Qt_QFileInfo_symLinkTarget( ::pPtr, ... )
   ENDSWITCH
   RETURN __hbqt_error()

