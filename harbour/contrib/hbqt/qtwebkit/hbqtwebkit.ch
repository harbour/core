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

#ifndef _HBQTWEBKIT_CH
#define _HBQTWEBKIT_CH

// enum #define QWebPage_Extension
// This enum describes the types of extensions that the page can support. Before using these extensions, you should verify that the extension is supported by calling supportsExtension().
//
#define QWebPage_ChooseMultipleFilesExtension     0    // Whether the web page supports multiple file selection. This extension is invoked when the web content requests one or more file names, for example as a result of the user clicking on a "file upload" button in a HTML form where multiple file selection is allowed.

// enum QWebPage::FindFlag
// flags QWebPage::FindFlags
// This enum describes the options available to QWebPage's findText() function. The options can be OR-ed together from the following list:
//
#define QWebPage_FindBackward                     1    // Searches backwards instead of forwards.
#define QWebPage_FindCaseSensitively              2    // By default findText() works case insensitive. Specifying this option changes the behaviour to a case sensitive find operation.
#define QWebPage_FindWrapsAroundDocument          4    // Makes findText() restart from the beginning of the document if the end was reached and the text was not found.
// The FindFlags type is a typedef for QFlags<FindFlag>. It stores an OR combination of FindFlag values.

// enum QWebPage::LinkDelegationPolicy
// This enum defines the delegation policies a webpage can have when activating links and emitting the linkClicked() signal.
//
#define QWebPage_DontDelegateLinks                0    // No links are delegated. Instead, QWebPage tries to handle them all.
#define QWebPage_DelegateExternalLinks            1    // When activating links that point to documents not stored on the local filesystem or an equivalent - such as the Qt resource system - then linkClicked() is emitted.
#define QWebPage_DelegateAllLinks                 2    // Whenever a link is activated the linkClicked() signal is emitted.

// enum QWebPage::NavigationType
// This enum describes the types of navigation available when browsing through hyperlinked documents.
//
#define QWebPage_NavigationTypeLinkClicked        0    // The user clicked on a link or pressed return on a focused link.
#define QWebPage_NavigationTypeFormSubmitted      1    // The user activated a submit button for an HTML form.
#define QWebPage_NavigationTypeBackOrForward      2    // Navigation to a previously shown document in the back or forward history is requested.
#define QWebPage_NavigationTypeReload             3    // The user activated the reload action.
#define QWebPage_NavigationTypeFormResubmitted    4    // An HTML form was submitted a second time.
#define QWebPage_NavigationTypeOther              5    // A navigation to another document using a method not listed above.

// enum QWebPage::WebAction
// This enum describes the types of action which can be performed on the web page. Actions which are related to text editing, cursor movement, and text selection only have an effect if contentEditable is true.
//
#define QWebPage_NoWebAction                      -1   // No action is triggered.
#define QWebPage_OpenLink                         0    // Open the current link.
#define QWebPage_OpenLinkInNewWindow              1    // Open the current link in a new window.
#define QWebPage_OpenFrameInNewWindow             2    // Replicate the current frame in a new window.
#define QWebPage_DownloadLinkToDisk               3    // Download the current link to the disk.
#define QWebPage_CopyLinkToClipboard              4    // Copy the current link to the clipboard.
#define QWebPage_OpenImageInNewWindow             5    // Open the highlighted image in a new window.
#define QWebPage_DownloadImageToDisk              6    // Download the highlighted image to the disk.
#define QWebPage_CopyImageToClipboard             7    // Copy the highlighted image to the clipboard.
#define QWebPage_Back                             8    // Navigate back in the history of navigated links.
#define QWebPage_Forward                          9    // Navigate forward in the history of navigated links.
#define QWebPage_Stop                             10   // Stop loading the current page.
#define QWebPage_Reload                           11   // Reload the current page.
#define QWebPage_Cut                              12   // Cut the content currently selected into the clipboard.
#define QWebPage_Copy                             13   // Copy the content currently selected into the clipboard.
#define QWebPage_Paste                            14   // Paste content from the clipboard.
#define QWebPage_Undo                             15   // Undo the last editing action.
#define QWebPage_Redo                             16   // Redo the last editing action.
#define QWebPage_MoveToNextChar                   17   // Move the cursor to the next character.
#define QWebPage_MoveToPreviousChar               18   // Move the cursor to the previous character.
#define QWebPage_MoveToNextWord                   19   // Move the cursor to the next word.
#define QWebPage_MoveToPreviousWord               20   // Move the cursor to the previous word.
#define QWebPage_MoveToNextLine                   21   // Move the cursor to the next line.
#define QWebPage_MoveToPreviousLine               22   // Move the cursor to the previous line.
#define QWebPage_MoveToStartOfLine                23   // Move the cursor to the start of the line.
#define QWebPage_MoveToEndOfLine                  24   // Move the cursor to the end of the line.
#define QWebPage_MoveToStartOfBlock               25   // Move the cursor to the start of the block.
#define QWebPage_MoveToEndOfBlock                 26   // Move the cursor to the end of the block.
#define QWebPage_MoveToStartOfDocument            27   // Move the cursor to the start of the document.
#define QWebPage_MoveToEndOfDocument              28   // Move the cursor to the end of the document.
#define QWebPage_SelectNextChar                   29   // Select to the next character.
#define QWebPage_SelectPreviousChar               30   // Select to the previous character.
#define QWebPage_SelectNextWord                   31   // Select to the next word.
#define QWebPage_SelectPreviousWord               32   // Select to the previous word.
#define QWebPage_SelectNextLine                   33   // Select to the next line.
#define QWebPage_SelectPreviousLine               34   // Select to the previous line.
#define QWebPage_SelectStartOfLine                35   // Select to the start of the line.
#define QWebPage_SelectEndOfLine                  36   // Select to the end of the line.
#define QWebPage_SelectStartOfBlock               37   // Select to the start of the block.
#define QWebPage_SelectEndOfBlock                 38   // Select to the end of the block.
#define QWebPage_SelectStartOfDocument            39   // Select to the start of the document.
#define QWebPage_SelectEndOfDocument              40   // Select to the end of the document.
#define QWebPage_DeleteStartOfWord                41   // Delete to the start of the word.
#define QWebPage_DeleteEndOfWord                  42   // Delete to the end of the word.
#define QWebPage_SetTextDirectionDefault          43   // Set the text direction to the default direction.
#define QWebPage_SetTextDirectionLeftToRight      44   // Set the text direction to left-to-right.
#define QWebPage_SetTextDirectionRightToLeft      45   // Set the text direction to right-to-left.
#define QWebPage_ToggleBold                       46   // Toggle the formatting between bold and normal weight.
#define QWebPage_ToggleItalic                     47   // Toggle the formatting between italic and normal style.
#define QWebPage_ToggleUnderline                  48   // Toggle underlining.
#define QWebPage_InspectElement                   49   // Show the Web Inspector with the currently highlighted HTML element.
#define QWebPage_InsertParagraphSeparator         50   // Insert a new paragraph.
#define QWebPage_InsertLineSeparator              51   // Insert a new line.
#define QWebPage_SelectAll                        52   // Selects all content.

// enum QWebPage::WebWindowType
//
#define QWebPage_WebBrowserWindow                 0    // The window is a regular web browser window.
#define QWebPage_WebModalDialog                   1    // The window acts as modal dialog.

#endif
