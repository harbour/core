/*
 * $Id$
 */

#ifndef _FILEIO_CH
#define _FILEIO_CH

/* File create flags */
#define FC_NORMAL     0x0000   /* No file attributes are set      */
#define FC_READONLY   0x0001   /* Read-only file attribute is set */
#define FC_HIDDEN     0x0002   /* Hidden file attribute is set    */
#define FC_SYSTEM     0x0004   /* System file attribute is set    */

/* File locking flags */
#define FL_LOCK       0x0000   /* Lock a region   */
#define FL_UNLOCK     0x0001   /* Unlock a region */

/* File access flags */
#define FO_READ       0x0000   /* File is opened for reading             */
#define FO_WRITE      0x0001   /* File is opened for writing             */
#define FO_READWRITE  0x0002   /* File is opened for reading and writing */

/* File sharing flags */
#define FO_COMPAT     0x0000   /* No sharing specified                               */
#define FO_EXCLUSIVE  0x0010   /* Deny further attempts to open the file             */
#define FO_DENYWRITE  0x0020   /* Deny further attempts to open the file for writing */
#define FO_DENYREAD   0x0030   /* Deny further attempts to open the file for reading */
#define FO_DENYNONE   0x0040   /* Do not deny any further attempts to open the file  */
#define FO_SHARED     FO_DENYNONE

/* File inheritance flags */
#define FO_INHERITED  0x0000   /* Spawned processes can inherit this file handle     */
#define FO_PRIVATE    0x0080   /* Spawned processes can not inherit this file handle */

/* File mode flags */
#define FS_SET        0x0000   /* Seek from beginning of file    */
#define FS_RELATIVE   0x0001   /* Seek from current file poitner */
#define FS_END        0x0002   /* Seek from end of file          */

/* File system error codes */
#define FS_ERROR      -1       /* Unspecified error */

/* Extended file open mode flags */
#define FXO_TRUNCATE  0x0100   /* Create (truncate if exists) */
#define FXO_APPEND    0x0200   /* Create (append if exists)   */
#define FXO_FORCEEXT  0x0800   /* Force default extension     */
#define FXO_DEFAULTS  0x1000   /* Use SET command defaults    */
#define FXO_DEVICERAW 0x2000   /* Open devices in raw mode    */

#endif /* _FILEIO_CH */
