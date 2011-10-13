/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *
 * Copyright 2009 Pritpal Bedi <pritpal@vouchcac.com>
 * http://harbour-project.org
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; see the file COPYING.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).
 *
 * As a special exception, the Harbour Project gives permission for
 * additional uses of the text contained in its release of Harbour.
 *
 * The exception is that, if you link the Harbour libraries with other
 * files to produce an executable, this does not by itself cause the
 * resulting executable to be covered by the GNU General Public License.
 * Your use of that executable is in no way restricted on account of
 * linking the Harbour library code into it.
 *
 * This exception does not however invalidate any other reasons why
 * the executable file might be covered by the GNU General Public License.
 *
 * This exception applies only to the code released by the Harbour
 * Project under the name Harbour.  If you copy code from other
 * Harbour Project or Free Software Foundation releases into a copy of
 * Harbour, as the General Public License permits, the exception does
 * not apply to the code that you add in this way.  To avoid misleading
 * anyone as to the status of such modified files, you must delete
 * this exception notice from them.
 *
 * If you write modifications of your own for Harbour, it is your choice
 * whether to permit this exception to apply to your modifications.
 * If you do not wish that, delete this exception notice.
 *
 */

#ifndef _HBQTCORE_CH
#define _HBQTCORE_CH

// This enum describes the errors that may be returned by the error() function.
#define QFile_NoError                             0        // No error occurred.
#define QFile_ReadError                           1        // An error occurred when reading from the file.
#define QFile_WriteError                          2        // An error occurred when writing to the file.
#define QFile_FatalError                          3        // A fatal error occurred.
#define QFile_ResourceError                       4        //
#define QFile_OpenError                           5        // The file could not be opened.
#define QFile_AbortError                          6        // The operation was aborted.
#define QFile_TimeOutError                        7        // A timeout occurred.
#define QFile_UnspecifiedError                    8        // An unspecified error occurred.
#define QFile_RemoveError                         9        // The file could not be removed.
#define QFile_RenameError                         10       // The file could not be renamed.
#define QFile_PositionError                       11       // The position in the file could not be changed.
#define QFile_ResizeError                         12       // The file could not be resized.
#define QFile_PermissionsError                    13       // The file could not be accessed.
#define QFile_CopyError                           14       // The file could not be copied.

#define QFile_NoOptions                           0        // No options.

// This enum is used by the permission() function to report the permissions and ownership of a file. The values may be OR-ed together to test multiple permissions and ownership values.
#define QFile_ReadOwner                           0x4000   // The file is readable by the owner of the file.
#define QFile_WriteOwner                          0x2000   // The file is writable by the owner of the file.
#define QFile_ExeOwner                            0x1000   // The file is executable by the owner of the file.
#define QFile_ReadUser                            0x0400   // The file is readable by the user.
#define QFile_WriteUser                           0x0200   // The file is writable by the user.
#define QFile_ExeUser                             0x0100   // The file is executable by the user.
#define QFile_ReadGroup                           0x0040   // The file is readable by the group.
#define QFile_WriteGroup                          0x0020   // The file is writable by the group.
#define QFile_ExeGroup                            0x0010   // The file is executable by the group.
#define QFile_ReadOther                           0x0004   // The file is readable by anyone.
#define QFile_WriteOther                          0x0002   // The file is writable by anyone.
#define QFile_ExeOther                            0x0001   // The file is executable by anyone.


#define QLibraryInfo_PrefixPath                   0  // The default prefix for all paths.
#define QLibraryInfo_DocumentationPath            1  // The location for documentation upon install.
#define QLibraryInfo_HeadersPath                  2  // The location for all headers.
#define QLibraryInfo_LibrariesPath                3  // The location of installed librarires.
#define QLibraryInfo_BinariesPath                 4  // The location of installed Qt binaries (tools and applications).
#define QLibraryInfo_PluginsPath                  5  // The location of installed Qt plugins.
#define QLibraryInfo_DataPath                     6  // The location of general Qt data.
#define QLibraryInfo_TranslationsPath             7  // The location of translation information for Qt strings.
#define QLibraryInfo_SettingsPath                 8  // The location for Qt settings.
#define QLibraryInfo_ExamplesPath                 10 // The location for examples upon install.
#define QLibraryInfo_DemosPath                    9  // The location for demos upon install.

#endif
