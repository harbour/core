/*
 * $Id$
 */
/*
   Default Settings
*/
#include "sixapi.ch"

PROCEDURE MAIN()

/* Defines the format of date strings returned by sx_GetDateString.
   AMERICAN "MM/DD/YY"
   ANSI      "YY.MM.DD"
   BRITISH "DD/MM/YY"
   FRENCH   "DD/MM/YY"
   GERMAN      "DD.MM.YY"
   ITALIAN "DD-MM-YY"
   SPANISH "DD-MM-YY"
*/

   SET DATE ANSI

/*
   Indicates whether or not the two digits of the year designating
   century are to be returned by sx_GetDateString as part of a date
   string formatted according to the sx_SetDateFormat setting.
*/
   SET CENTURY ON

/*
   Makes deleted records either transparent or visible to record positioning
   functions. Setting Deleted ON incurs certain performance penalties.
   Instead of using Set Deleted ON, consider creating conditional index
   files with a condition of .not. deleted.

   If True, deleted records will be invisible to all record positioning
   functions except GO.
*/
   SET DELETED ON

/* Indicates whether or not Seeks are to return True if a partial key
   match is made. If True, key searches made with sx_Seek must match
   exactly in content and length.  Partial key matches will result in
   False returns from sx_Seek.
*/
   SET EXACT ON

/*
   Change the number of open file handles in the current Windows task.
   255 max.
*/
   SET HANDLE 200

/*
   Sets the number of seconds allowed to retry a lock operation before failing.
   The lock operation will be continuously retried for this number of seconds
   before reporting failure.  The default value is 1 second.
*/
   SET LOCK TIMEOUT 5

/*
   Indicates whether or not index seeks that result in failure
   (i.e., the requested key value does not match any key in the index
   order either partially or exactly) should result in a successful
   conclusion if a key is found that is immediately greater than the
   requested key.

   If ON, then soft seeks are performed for all files in the current task.
   The soft seek setting is global in this respect.
   sx_SetSoftSeek is normally only turned on when necessary, and then
   turned off immediately after performing sx_Seek.
   See sx_Seek for details as to its behavior when sx_SetSoftSeek is set
   to True.

   NOTE: This is a global system setting that affects all index seeks once
   it is turned on.
*/
   SET SOFTSEEK ON
