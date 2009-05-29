/*
 * $Id$
 */

#include "hbwhat.h"

#include <windows.h>
#include <mapi.h>
#include "hbapi.h"

/*
   MapiSendMail( 0,                    ;   // hWnd
               'This is Subject',      ;   // cSubject
               'And this is text',     ;   // cText
               'Pritpal Bedi',         ;   // Sender's Name
               'vouchcac@hotmail.com', ;   // Sender's Address
               'Pritpal',              ;   // Recipient's Name
               'info@vouchcac.com',    ;   // Recipient's Address
               'C:\autoexec.bat'       )   // File attached

*/

HB_FUNC( VWN_MAPISENDMAIL )
{
   MapiRecipDesc orig;
   MapiRecipDesc rcpt;
   MapiFileDesc  file;
   MapiMessage   mssg;

   orig.ulReserved         = 0            ;  // Reserved
   orig.ulRecipClass       = MAPI_ORIG    ;  // Reciepient Class MAPI_TO MAPI_CC MAPI_BCC
   orig.lpszName           = hb_parcx( 4 );  // Originator's Name
   orig.lpszAddress        = hb_parcx( 5 );  // Originators Address
   orig.ulEIDSize          = 0            ;  // Count in bytes of size of pEntryID
   orig.lpEntryID          = NULL         ;  // System-specific Originator reference

   rcpt.ulReserved         = 0            ;  // Reserved
   rcpt.ulRecipClass       = MAPI_TO      ;  // Reciepient Class MAPI_TO MAPI_CC MAPI_BCC
   rcpt.lpszName           = hb_parcx( 6 );  // Reciepient's Name, e.g., vouchcac@hotmail.com
   rcpt.lpszAddress        = hb_parcx( 7 );  // Reciepient's Address
   rcpt.ulEIDSize          = 0            ;  // Count in bytes of size of pEntryID
   rcpt.lpEntryID          = 0            ;  // System-specific Recipient reference

   file.ulReserved         = 0            ;  // Reserved for future usage
   file.flFlags            = 0            ;  // Flags ?
   file.nPosition          = ( ULONG ) -1  ;  // Character of text to be replaced by attachment
   file.lpszPathName       = hb_parcx( 8 );  // Full Path Name with Extension of the attached file
   file.lpszFileName       = NULL         ;  // Original File Name ( optional )
   file.lpFileType         = NULL         ;  // Attachment file type ( can be lpMapiFileTagExt )

   mssg.ulReserved         = 0            ;  // Reserved
   mssg.lpszSubject        = hb_parcx( 2 );  // Message Subject
   mssg.lpszNoteText       = hb_parcx( 3 );  // Message Text
   mssg.lpszMessageType    = NULL         ;  // Message Class
   mssg.lpszDateReceived   = NULL         ;  // in yyyy/mm/dd hh:mm format
   mssg.lpszConversationID = NULL         ;  // Conversation thread ID
   mssg.flFlags            = 0            ;  // unread, return receipt
   mssg.lpOriginator       = &orig        ;  // Originator's descriptor
   mssg.nRecipCount        = 1            ;  // Number of receipients
   mssg.lpRecips           = &rcpt        ;  // Recipient descriptors
   mssg.nFileCount         = 1            ;  // Number of file attachments
   mssg.lpFiles            = &file        ;  // Attachment descriptors

   // to send the mail direcly and without intervenstion
   hb_retnl( (ULONG) MAPISendMail( 0, 0, &mssg, 0, 0 ) );

   // to open default mail client's dialog box
   // hb_retnl( (ULONG) MAPISendMail( 0, 0, &mssg, 8, 0 ) );
}
