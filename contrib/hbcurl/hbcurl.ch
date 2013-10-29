/*
 * Harbour Project source code:
 * libcurl 'easy' API - Harbour header.
 *
 * Copyright 2008 Viktor Szakats (vszakats.net/harbour)
 * originally based on:
 * Copyright 2005 Luiz Rafael Culik Guimaraes <luiz at xharbour.com.br>
 * www - http://harbour-project.org
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
 * along with this software; see the file COPYING.txt.  If not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
 * Boston, MA 02111-1307 USA (or visit the web site https://www.gnu.org/).
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

/* NOTE: This file is also used by C code. */

#ifndef HBCURL_CH_
#define HBCURL_CH_

/* curl_easy_setopt() parameters.
   NOTE: The actual values may be different from the libcurl equivalent. */

#define HB_CURLOPT_FILE                       1
#define HB_CURLOPT_URL                        2
#define HB_CURLOPT_PORT                       3
#define HB_CURLOPT_PROXY                      4
#define HB_CURLOPT_USERPWD                    5
#define HB_CURLOPT_PROXYUSERPWD               6
#define HB_CURLOPT_RANGE                      7
#define HB_CURLOPT_INFILE                     9
#define HB_CURLOPT_ERRORBUFFER                10
#define HB_CURLOPT_WRITEFUNCTION              11
#define HB_CURLOPT_READFUNCTION               12
#define HB_CURLOPT_TIMEOUT                    13
#define HB_CURLOPT_INFILESIZE                 14
#define HB_CURLOPT_POSTFIELDS                 15
#define HB_CURLOPT_REFERER                    16
#define HB_CURLOPT_FTPPORT                    17
#define HB_CURLOPT_USERAGENT                  18
#define HB_CURLOPT_LOW_SPEED_LIMIT            19
#define HB_CURLOPT_LOW_SPEED_TIME             20
#define HB_CURLOPT_RESUME_FROM                21
#define HB_CURLOPT_COOKIE                     22
#define HB_CURLOPT_HTTPHEADER                 23
#define HB_CURLOPT_HTTPPOST                   24
#define HB_CURLOPT_SSLCERT                    25
#define HB_CURLOPT_KEYPASSWD                  26
#define HB_CURLOPT_SSLCERTPASSWD              HB_CURLOPT_KEYPASSWD
#define HB_CURLOPT_SSLKEYPASSWD               HB_CURLOPT_KEYPASSWD
#define HB_CURLOPT_CRLF                       27
#define HB_CURLOPT_QUOTE                      28
#define HB_CURLOPT_WRITEHEADER                29
#define HB_CURLOPT_COOKIEFILE                 31
#define HB_CURLOPT_SSLVERSION                 32
#define HB_CURLOPT_TIMECONDITION              33
#define HB_CURLOPT_TIMEVALUE                  34
#define HB_CURLOPT_CUSTOMREQUEST              36
#define HB_CURLOPT_STDERR                     37
#define HB_CURLOPT_POSTQUOTE                  39
#define HB_CURLOPT_WRITEINFO                  40
#define HB_CURLOPT_VERBOSE                    41  /* talk a lot */
#define HB_CURLOPT_HEADER                     42  /* throw the header out too */
#define HB_CURLOPT_NOPROGRESS                 43  /* shut off the progress meter */
#define HB_CURLOPT_NOBODY                     44  /* use HEAD to get http document */
#define HB_CURLOPT_FAILONERROR                45  /* no output on http error codes >= 300 */
#define HB_CURLOPT_UPLOAD                     46  /* this is an upload */
#define HB_CURLOPT_POST                       47  /* HTTP POST method */
#define HB_CURLOPT_DIRLISTONLY                48  /* Use NLST when listing ftp dir */
#define HB_CURLOPT_FTPLISTONLY                HB_CURLOPT_DIRLISTONLY
#define HB_CURLOPT_APPEND                     50  /* Append instead of overwrite on upload! */
#define HB_CURLOPT_FTPAPPEND                  HB_CURLOPT_APPEND
#define HB_CURLOPT_NETRC                      51
#define HB_CURLOPT_FOLLOWLOCATION             52  /* use Location: Luke! */
#define HB_CURLOPT_TRANSFERTEXT               53  /* transfer data in text/ASCII format */
#define HB_CURLOPT_PUT                        54  /* HTTP PUT */
#define HB_CURLOPT_PROGRESSFUNCTION           56
#define HB_CURLOPT_PROGRESSDATA               57
#define HB_CURLOPT_AUTOREFERER                58
#define HB_CURLOPT_PROXYPORT                  59
#define HB_CURLOPT_POSTFIELDSIZE              60
#define HB_CURLOPT_HTTPPROXYTUNNEL            61
#define HB_CURLOPT_INTERFACE                  62
#define HB_CURLOPT_KRBLEVEL                   63
#define HB_CURLOPT_KRB4LEVEL                  HB_CURLOPT_KRBLEVEL
#define HB_CURLOPT_SSL_VERIFYPEER             64
#define HB_CURLOPT_CAINFO                     65
#define HB_CURLOPT_MAXREDIRS                  68
#define HB_CURLOPT_FILETIME                   69
#define HB_CURLOPT_TELNETOPTIONS              70
#define HB_CURLOPT_MAXCONNECTS                71
#define HB_CURLOPT_CLOSEPOLICY                72
#define HB_CURLOPT_FRESH_CONNECT              74
#define HB_CURLOPT_FORBID_REUSE               75
#define HB_CURLOPT_RANDOM_FILE                76
#define HB_CURLOPT_EGDSOCKET                  77
#define HB_CURLOPT_CONNECTTIMEOUT             78
#define HB_CURLOPT_HEADERFUNCTION             79
#define HB_CURLOPT_HTTPGET                    80
#define HB_CURLOPT_SSL_VERIFYHOST             81
#define HB_CURLOPT_COOKIEJAR                  82
#define HB_CURLOPT_SSL_CIPHER_LIST            83
#define HB_CURLOPT_HTTP_VERSION               84
#define HB_CURLOPT_FTP_USE_EPSV               85
#define HB_CURLOPT_SSLCERTTYPE                86
#define HB_CURLOPT_SSLKEY                     87
#define HB_CURLOPT_SSLKEYTYPE                 88
#define HB_CURLOPT_SSLENGINE                  89
#define HB_CURLOPT_SSLENGINE_DEFAULT          90
#define HB_CURLOPT_DNS_USE_GLOBAL_CACHE       91  /* To become OBSOLETE soon */
#define HB_CURLOPT_DNS_CACHE_TIMEOUT          92
#define HB_CURLOPT_PREQUOTE                   93
#define HB_CURLOPT_DEBUGFUNCTION              94
#define HB_CURLOPT_DEBUGDATA                  95
#define HB_CURLOPT_COOKIESESSION              96
#define HB_CURLOPT_CAPATH                     97
#define HB_CURLOPT_BUFFERSIZE                 98
#define HB_CURLOPT_NOSIGNAL                   99
#define HB_CURLOPT_SHARE                      100
#define HB_CURLOPT_PROXYTYPE                  101
#define HB_CURLOPT_ACCEPT_ENCODING            102
#define HB_CURLOPT_ENCODING                   HB_CURLOPT_ACCEPT_ENCODING
#define HB_CURLOPT_PRIVATE                    103
#define HB_CURLOPT_HTTP200ALIASES             104
#define HB_CURLOPT_UNRESTRICTED_AUTH          105
#define HB_CURLOPT_FTP_USE_EPRT               106
#define HB_CURLOPT_HTTPAUTH                   107
#define HB_CURLOPT_SSL_CTX_FUNCTION           108
#define HB_CURLOPT_SSL_CTX_DATA               109
#define HB_CURLOPT_FTP_CREATE_MISSING_DIRS    110
#define HB_CURLOPT_PROXYAUTH                  111
#define HB_CURLOPT_FTP_RESPONSE_TIMEOUT       112
#define HB_CURLOPT_IPRESOLVE                  113
#define HB_CURLOPT_MAXFILESIZE                114
#define HB_CURLOPT_INFILESIZE_LARGE           115
#define HB_CURLOPT_RESUME_FROM_LARGE          116
#define HB_CURLOPT_MAXFILESIZE_LARGE          117
#define HB_CURLOPT_NETRC_FILE                 118
#define HB_CURLOPT_USE_SSL                    119
#define HB_CURLOPT_FTP_SSL                    HB_CURLOPT_USE_SSL
#define HB_CURLOPT_POSTFIELDSIZE_LARGE        120
#define HB_CURLOPT_TCP_NODELAY                121
#define HB_CURLOPT_SOURCE_USERPWD             123
#define HB_CURLOPT_SOURCE_PREQUOTE            127
#define HB_CURLOPT_SOURCE_POSTQUOTE           128
#define HB_CURLOPT_FTPSSLAUTH                 129
#define HB_CURLOPT_IOCTLFUNCTION              130
#define HB_CURLOPT_IOCTLDATA                  131
#define HB_CURLOPT_SOURCE_URL                 132
#define HB_CURLOPT_SOURCE_QUOTE               133
#define HB_CURLOPT_FTP_ACCOUNT                134
#define HB_CURLOPT_COOKIELIST                 135
#define HB_CURLOPT_IGNORE_CONTENT_LENGTH      136
#define HB_CURLOPT_FTP_SKIP_PASV_IP           137
#define HB_CURLOPT_FTP_FILEMETHOD             138
#define HB_CURLOPT_LOCALPORT                  139
#define HB_CURLOPT_LOCALPORTRANGE             140
#define HB_CURLOPT_CONNECT_ONLY               141
#define HB_CURLOPT_CONV_FROM_NETWORK_FUNCTION 142
#define HB_CURLOPT_CONV_TO_NETWORK_FUNCTION   143
#define HB_CURLOPT_CONV_FROM_UTF8_FUNCTION    144
#define HB_CURLOPT_MAX_SEND_SPEED_LARGE       145
#define HB_CURLOPT_MAX_RECV_SPEED_LARGE       146
#define HB_CURLOPT_FTP_ALTERNATIVE_TO_USER    147
#define HB_CURLOPT_SOCKOPTFUNCTION            148
#define HB_CURLOPT_SOCKOPTDATA                149
#define HB_CURLOPT_SSL_SESSIONID_CACHE        150
#define HB_CURLOPT_SSH_AUTH_TYPES             151
#define HB_CURLOPT_SSH_PUBLIC_KEYFILE         152
#define HB_CURLOPT_SSH_PRIVATE_KEYFILE        153
#define HB_CURLOPT_FTP_SSL_CCC                154
#define HB_CURLOPT_TIMEOUT_MS                 155
#define HB_CURLOPT_CONNECTTIMEOUT_MS          156
#define HB_CURLOPT_HTTP_TRANSFER_DECODING     157
#define HB_CURLOPT_HTTP_CONTENT_DECODING      158
#define HB_CURLOPT_NEW_FILE_PERMS             159
#define HB_CURLOPT_NEW_DIRECTORY_PERMS        160
#define HB_CURLOPT_POST301                    161
#define HB_CURLOPT_SSH_HOST_PUBLIC_KEY_MD5    162
#define HB_CURLOPT_OPENSOCKETFUNCTION         163
#define HB_CURLOPT_OPENSOCKETDATA             164
#define HB_CURLOPT_COPYPOSTFIELDS             165
#define HB_CURLOPT_PROXY_TRANSFER_MODE        166
#define HB_CURLOPT_SEEKFUNCTION               167
#define HB_CURLOPT_SEEKDATA                   168
#define HB_CURLOPT_CRLFILE                    169
#define HB_CURLOPT_ISSUERCERT                 170
#define HB_CURLOPT_ADDRESS_SCOPE              171
#define HB_CURLOPT_CERTINFO                   172
#define HB_CURLOPT_POSTREDIR                  HB_CURLOPT_POST301
#define HB_CURLOPT_USERNAME                   173
#define HB_CURLOPT_PASSWORD                   174
#define HB_CURLOPT_PROXYUSERNAME              175
#define HB_CURLOPT_PROXYPASSWORD              176
#define HB_CURLOPT_NOPROXY                    177
#define HB_CURLOPT_TFTP_BLKSIZE               178
#define HB_CURLOPT_SOCKS5_GSSAPI_SERVICE      179
#define HB_CURLOPT_SOCKS5_GSSAPI_NEC          180
#define HB_CURLOPT_PROTOCOLS                  181
#define HB_CURLOPT_REDIR_PROTOCOLS            182
#define HB_CURLOPT_SSH_KNOWNHOSTS             183
#define HB_CURLOPT_MAIL_FROM                  186
#define HB_CURLOPT_MAIL_RCPT                  187
#define HB_CURLOPT_FTP_USE_PRET               188
#define HB_CURLOPT_RTSP_REQUEST               189
#define HB_CURLOPT_RTSP_SESSION_ID            190
#define HB_CURLOPT_RTSP_STREAM_URI            191
#define HB_CURLOPT_RTSP_TRANSPORT             192
#define HB_CURLOPT_RTSP_HEADER                HB_CURLOPT_HTTPHEADER
#define HB_CURLOPT_RTSP_CLIENT_CSEQ           193
#define HB_CURLOPT_RTSP_SERVER_CSEQ           194
#define HB_CURLOPT_WILDCARDMATCH              197
#define HB_CURLOPT_RESOLVE                    198
#define HB_CURLOPT_TRANSFER_ENCODING          199
#define HB_CURLOPT_GSSAPI_DELEGATION          200
#define HB_CURLOPT_DNS_SERVERS                201
#define HB_CURLOPT_ACCEPTTIMEOUT_MS           202
#define HB_CURLOPT_SSL_OPTIONS                203
#define HB_CURLOPT_TCP_KEEPALIVE              204
#define HB_CURLOPT_TCP_KEEPIDLE               205
#define HB_CURLOPT_TCP_KEEPINTVL              206
#define HB_CURLOPT_MAIL_AUTH                  207
#define HB_CURLOPT_BEARER                     208
#define HB_CURLOPT_DOWNLOAD                   1001  /* Harbour special ones */
#define HB_CURLOPT_XFERINFOBLOCK              1002
#define HB_CURLOPT_UL_FILE_SETUP              1003
#define HB_CURLOPT_UL_FILE_CLOSE              1004
#define HB_CURLOPT_DL_FILE_SETUP              1005
#define HB_CURLOPT_DL_FILE_CLOSE              1006
#define HB_CURLOPT_UL_BUFF_SETUP              1007
#define HB_CURLOPT_DL_BUFF_SETUP              1008
#define HB_CURLOPT_DL_BUFF_GET                1009
#define HB_CURLOPT_UL_NULL_SETUP              1010
#define HB_CURLOPT_UL_FHANDLE_SETUP           1011
#define HB_CURLOPT_DL_FHANDLE_SETUP           1012
#define HB_CURLOPT_PROGRESSBLOCK              HB_CURLOPT_XFERINFOBLOCK
/* Compatibility ones. Please don't use these. */
#define HB_CURLOPT_SETUPLOADFILE              HB_CURLOPT_UL_FILE_SETUP
#define HB_CURLOPT_CLOSEUPLOADFILE            HB_CURLOPT_UL_FILE_CLOSE
#define HB_CURLOPT_SETDOWNLOADFILE            HB_CURLOPT_DL_FILE_SETUP
#define HB_CURLOPT_CLOSEDOWNLOADFILE          HB_CURLOPT_DL_FILE_CLOSE
#define HB_CURLOPT_SETPROGRESS                HB_CURLOPT_PROGRESSBLOCK

/* HB_CURLOPT_PROXYTYPE option */
#define HB_CURLPROXY_HTTP                     0  /* added in 7.10 */
#define HB_CURLPROXY_HTTP_1_0                 1  /* added in 7.19.4, force to use CONNECT HTTP/1.0 */
#define HB_CURLPROXY_SOCKS4                   4  /* support added in 7.15.2, enum existed already in 7.10 */
#define HB_CURLPROXY_SOCKS5                   5  /* added in 7.10 */
#define HB_CURLPROXY_SOCKS4A                  6  /* added in 7.18.0 */
#define HB_CURLPROXY_SOCKS5_HOSTNAME          7  /* Use the SOCKS5 protocol but pass along the host name rather than the IP address. added in 7.18.0 */

/* HB_CURLOPT_NETRC option */
#define HB_CURL_NETRC_IGNORED                 0  /* The .netrc will never be read. */
#define HB_CURL_NETRC_OPTIONAL                1  /* A user:password in the URL will be preferred */
#define HB_CURL_NETRC_REQUIRED                2  /* A user:password in the URL will be ignored. */

/* HB_CURLOPT_SSL_OPTIONS values */
#define HB_CURLSSLOPT_ALLOW_BEAST             hb_bitShift( 1, 0 )

/* HB_CURLOPT_HTTPAUTH option */
#define HB_CURLAUTH_NONE                      0                    /* nothing */
#define HB_CURLAUTH_BASIC                     1                    /* Basic (default) */
#define HB_CURLAUTH_DIGEST                    2                    /* Digest */
#define HB_CURLAUTH_GSSNEGOTIATE              4                    /* GSS-Negotiate */
#define HB_CURLAUTH_NTLM                      8                    /* NTLM */
#define HB_CURLAUTH_DIGEST_IE                 hb_bitShift( 1, 4 )  /* Digest with IE flavour */
#define HB_CURLAUTH_NTLM_WB                   hb_bitShift( 1, 5 )  /* NTLM delegating to winbind helper */
#define HB_CURLAUTH_ONLY                      hb_bitShift( 1, 31 ) /* used together with a single other type to force no auth or just that single type */
#define HB_CURLAUTH_ANY                       hb_bitNot( 0 )       /* all types set */
#define HB_CURLAUTH_ANYSAFE                   hb_bitNot( hb_bitOr( HB_CURLAUTH_BASIC, HB_CURLAUTH_DIGEST_IE ) )

/* HB_CURLOPT_HTTP_VERSION option */
#define HB_CURL_HTTP_VERSION_NONE             0  /* setting this means we don't care, and that we'd like the library to choose the best possible for us! */
#define HB_CURL_HTTP_VERSION_1_0              1  /* please use HTTP 1.0 in the request */
#define HB_CURL_HTTP_VERSION_1_1              2  /* please use HTTP 1.1 in the request */

/* HB_CURLOPT_USE_SSL option */
#define HB_CURLUSESSL_NONE                    0  /* do not attempt to use SSL */
#define HB_CURLUSESSL_TRY                     1  /* try using SSL, proceed anyway otherwise */
#define HB_CURLUSESSL_CONTROL                 2  /* SSL for the control connection or fail */
#define HB_CURLUSESSL_ALL                     3  /* SSL for all communication or fail */

/* HB_CURLOPT_FTPSSLAUTH option */
#define HB_CURLFTPAUTH_DEFAULT                0  /* let libcurl decide */
#define HB_CURLFTPAUTH_SSL                    1  /* use "AUTH SSL" */
#define HB_CURLFTPAUTH_TLS                    2  /* use "AUTH TLS" */

/* HB_CURLOPT_FTP_SSL_CCC option */
#define HB_CURLFTPSSL_CCC_NONE                0  /* do not send CCC */
#define HB_CURLFTPSSL_CCC_PASSIVE             1  /* Let the server initiate the shutdown */
#define HB_CURLFTPSSL_CCC_ACTIVE              2  /* Initiate the shutdown */

/* HB_CURLOPT_FTP_FILEMETHOD option */
#define HB_CURLFTPMETHOD_DEFAULT              0  /* let libcurl pick */
#define HB_CURLFTPMETHOD_MULTICWD             1  /* single CWD operation for each path part */
#define HB_CURLFTPMETHOD_NOCWD                2  /* no CWD at all */
#define HB_CURLFTPMETHOD_SINGLECWD            3  /* one CWD to full dir, then work on file */

/* HB_CURLOPT_FTP_CREATE_MISSING_DIRS option */
#define HB_CURLFTP_CREATE_DIR_NONE            0
#define HB_CURLFTP_CREATE_DIR                 1
#define HB_CURLFTP_CREATE_DIR_RETRY           2

/* HB_CURLOPT_RTSP_REQUEST option */
#define HB_CURL_RTSPREQ_NONE                  0
#define HB_CURL_RTSPREQ_OPTIONS               1
#define HB_CURL_RTSPREQ_DESCRIBE              2
#define HB_CURL_RTSPREQ_ANNOUNCE              3
#define HB_CURL_RTSPREQ_SETUP                 4
#define HB_CURL_RTSPREQ_PLAY                  5
#define HB_CURL_RTSPREQ_PAUSE                 6
#define HB_CURL_RTSPREQ_TEARDOWN              7
#define HB_CURL_RTSPREQ_GET_PARAMETER         8
#define HB_CURL_RTSPREQ_SET_PARAMETER         9
#define HB_CURL_RTSPREQ_RECORD                10
#define HB_CURL_RTSPREQ_RECEIVE               11
#define HB_CURL_RTSPREQ_LAST                  12

/* HB_CURLOPT_TIMECONDITION option */
#define HB_CURL_TIMECOND_NONE                 0
#define HB_CURL_TIMECOND_IFMODSINCE           1
#define HB_CURL_TIMECOND_IFUNMODSINCE         2
#define HB_CURL_TIMECOND_LASTMOD              3

/* HB_CURLOPT_IPRESOLVE option */
#define HB_CURL_IPRESOLVE_WHATEVER            0  /* default, resolves addresses to all IP versions that your system allows */
#define HB_CURL_IPRESOLVE_V4                  1  /* resolve to ipv4 addresses */
#define HB_CURL_IPRESOLVE_V6                  2  /* resolve to ipv6 addresses */

/* HB_CURLOPT_SSLVERSION option */
#define HB_CURL_SSLVERSION_DEFAULT            0
#define HB_CURL_SSLVERSION_TLSv1              1
#define HB_CURL_SSLVERSION_SSLv2              2
#define HB_CURL_SSLVERSION_SSLv3              3

/*  HB_CURLOPT_SSH_AUTH_TYPES option */
#define HB_CURL_CURLSSH_AUTH_ANY              hb_bitNot( 0 )      /* all types supported by the server */
#define HB_CURL_CURLSSH_AUTH_NONE             0                   /* none allowed, silly but complete */
#define HB_CURL_CURLSSH_AUTH_PUBLICKEY        1                   /* public/private key files */
#define HB_CURL_CURLSSH_AUTH_PASSWORD         2                   /* password */
#define HB_CURL_CURLSSH_AUTH_HOST             4                   /* host key files */
#define HB_CURL_CURLSSH_AUTH_KEYBOARD         8                   /* keyboard interactive */
#define HB_CURL_CURLSSH_AUTH_DEFAULT          HB_CURLSSH_AUTH_ANY

/* CURLOPT_*PROTOCOLS options */
#define HB_CURLPROTO_HTTP                     hb_bitShift( 1, 0 )
#define HB_CURLPROTO_HTTPS                    hb_bitShift( 1, 1 )
#define HB_CURLPROTO_FTP                      hb_bitShift( 1, 2 )
#define HB_CURLPROTO_FTPS                     hb_bitShift( 1, 3 )
#define HB_CURLPROTO_SCP                      hb_bitShift( 1, 4 )
#define HB_CURLPROTO_SFTP                     hb_bitShift( 1, 5 )
#define HB_CURLPROTO_TELNET                   hb_bitShift( 1, 6 )
#define HB_CURLPROTO_LDAP                     hb_bitShift( 1, 7 )
#define HB_CURLPROTO_LDAPS                    hb_bitShift( 1, 8 )
#define HB_CURLPROTO_DICT                     hb_bitShift( 1, 9 )
#define HB_CURLPROTO_FILE                     hb_bitShift( 1, 10 )
#define HB_CURLPROTO_TFTP                     hb_bitShift( 1, 11 )
#define HB_CURLPROTO_IMAP                     hb_bitShift( 1, 12 )
#define HB_CURLPROTO_IMAPS                    hb_bitShift( 1, 13 )
#define HB_CURLPROTO_POP3                     hb_bitShift( 1, 14 )
#define HB_CURLPROTO_POP3S                    hb_bitShift( 1, 15 )
#define HB_CURLPROTO_SMTP                     hb_bitShift( 1, 16 )
#define HB_CURLPROTO_SMTPS                    hb_bitShift( 1, 17 )
#define HB_CURLPROTO_RTSP                     hb_bitShift( 1, 18 )
#define HB_CURLPROTO_RTMP                     hb_bitShift( 1, 19 )
#define HB_CURLPROTO_RTMPT                    hb_bitShift( 1, 20 )
#define HB_CURLPROTO_RTMPE                    hb_bitShift( 1, 21 )
#define HB_CURLPROTO_RTMPTE                   hb_bitShift( 1, 22 )
#define HB_CURLPROTO_RTMPS                    hb_bitShift( 1, 23 )
#define HB_CURLPROTO_RTMPTS                   hb_bitShift( 1, 24 )
#define HB_CURLPROTO_ALL                      hb_bitNot( 0 )

/* curl_easy_pause() parameters. They can be combined with hb_bitOr(). */
#define HB_CURLPAUSE_RECV                     1
#define HB_CURLPAUSE_RECV_CONT                0
#define HB_CURLPAUSE_SEND                     4
#define HB_CURLPAUSE_SEND_CONT                0
#define HB_CURLPAUSE_ALL                      hb_bitOr( HB_CURLPAUSE_RECV, HB_CURLPAUSE_SEND )
#define HB_CURLPAUSE_CONT                     hb_bitOr( HB_CURLPAUSE_RECV_CONT, HB_CURLPAUSE_SEND_CONT )

/* curl_global_init() parameters. */
#define HB_CURL_GLOBAL_SSL                    1
#define HB_CURL_GLOBAL_WIN32                  2
#define HB_CURL_GLOBAL_ALL                    hb_bitOr( HB_CURL_GLOBAL_SSL, HB_CURL_GLOBAL_WIN32 )
#define HB_CURL_GLOBAL_NOTHING                0
#define HB_CURL_GLOBAL_DEFAULT                HB_CURL_GLOBAL_ALL

/* curl_easy_getinfo() parameters.
   NOTE: The actual values may be different from the libcurl equivalent. */

#define HB_CURLINFO_EFFECTIVE_URL             1
#define HB_CURLINFO_RESPONSE_CODE             2
#define HB_CURLINFO_HTTP_CONNECTCODE          3
#define HB_CURLINFO_FILETIME                  4
#define HB_CURLINFO_TOTAL_TIME                5
#define HB_CURLINFO_NAMELOOKUP_TIME           6
#define HB_CURLINFO_CONNECT_TIME              7
#define HB_CURLINFO_PRETRANSFER_TIME          8
#define HB_CURLINFO_STARTTRANSFER_TIME        9
#define HB_CURLINFO_REDIRECT_TIME             10
#define HB_CURLINFO_REDIRECT_COUNT            11
#define HB_CURLINFO_REDIRECT_URL              12
#define HB_CURLINFO_SIZE_UPLOAD               13
#define HB_CURLINFO_SIZE_DOWNLOAD             14
#define HB_CURLINFO_SPEED_DOWNLOAD            15
#define HB_CURLINFO_SPEED_UPLOAD              16
#define HB_CURLINFO_HEADER_SIZE               17
#define HB_CURLINFO_REQUEST_SIZE              18
#define HB_CURLINFO_SSL_VERIFYRESULT          19
#define HB_CURLINFO_SSL_ENGINES               20
#define HB_CURLINFO_CONTENT_LENGTH_DOWNLOAD   21
#define HB_CURLINFO_CONTENT_LENGTH_UPLOAD     22
#define HB_CURLINFO_CONTENT_TYPE              23
#define HB_CURLINFO_PRIVATE                   24
#define HB_CURLINFO_HTTPAUTH_AVAIL            25
#define HB_CURLINFO_PROXYAUTH_AVAIL           26
#define HB_CURLINFO_OS_ERRNO                  27
#define HB_CURLINFO_NUM_CONNECTS              28
#define HB_CURLINFO_COOKIELIST                29
#define HB_CURLINFO_LASTSOCKET                30
#define HB_CURLINFO_FTP_ENTRY_PATH            31
#define HB_CURLINFO_PRIMARY_IP                32
#define HB_CURLINFO_APPCONNECT_TIME           33
#define HB_CURLINFO_CERTINFO                  34
#define HB_CURLINFO_CONDITION_UNMET           35
#define HB_CURLINFO_RTSP_SESSION_ID           36
#define HB_CURLINFO_RTSP_CLIENT_CSEQ          37
#define HB_CURLINFO_RTSP_SERVER_CSEQ          38
#define HB_CURLINFO_RTSP_CSEQ_RECV            39
#define HB_CURLINFO_PRIMARY_PORT              40
#define HB_CURLINFO_LOCAL_IP                  41
#define HB_CURLINFO_LOCAL_PORT                42

/* curl result codes. */

#define HB_CURLE_ERROR                        -1 /* request not passed to libcurl (libcurl not initialized or unknown parameter) */
#define HB_CURLE_OK                           0
#define HB_CURLE_UNSUPPORTED_PROTOCOL         1  /* */
#define HB_CURLE_FAILED_INIT                  2  /* */
#define HB_CURLE_URL_MALFORMAT                3  /* */
#define HB_CURLE_NOT_BUILT_IN                 4  /* */
#define HB_CURLE_COULDNT_RESOLVE_PROXY        5  /* */
#define HB_CURLE_COULDNT_RESOLVE_HOST         6  /* */
#define HB_CURLE_COULDNT_CONNECT              7  /* */
#define HB_CURLE_FTP_WEIRD_SERVER_REPLY       8  /* */
#define HB_CURLE_REMOTE_ACCESS_DENIED         9  /* a service was denied by the server due to lack of access - when login fails this is not returned. */
#define HB_CURLE_OBSOLETE10                   10 /* NOT USED */
#define HB_CURLE_FTP_WEIRD_PASS_REPLY         11 /* */
#define HB_CURLE_OBSOLETE12                   12 /* NOT USED */
#define HB_CURLE_FTP_WEIRD_PASV_REPLY         13 /* */
#define HB_CURLE_FTP_WEIRD_227_FORMAT         14 /* */
#define HB_CURLE_FTP_CANT_GET_HOST            15 /* */
#define HB_CURLE_OBSOLETE16                   16 /* NOT USED */
#define HB_CURLE_FTP_COULDNT_SET_TYPE         17 /* */
#define HB_CURLE_PARTIAL_FILE                 18 /* */
#define HB_CURLE_FTP_COULDNT_RETR_FILE        19 /* */
#define HB_CURLE_OBSOLETE20                   20 /* NOT USED */
#define HB_CURLE_QUOTE_ERROR                  21 /* quote command failure */
#define HB_CURLE_HTTP_RETURNED_ERROR          22 /* */
#define HB_CURLE_WRITE_ERROR                  23 /* */
#define HB_CURLE_OBSOLETE24                   24 /* NOT USED */
#define HB_CURLE_UPLOAD_FAILED                25 /* failed upload "command" */
#define HB_CURLE_READ_ERROR                   26 /* could open/read from file */
#define HB_CURLE_OUT_OF_MEMORY                27 /* */
#define HB_CURLE_OPERATION_TIMEDOUT           28 /* the timeout time was reached */
#define HB_CURLE_OBSOLETE29                   29 /* NOT USED */
#define HB_CURLE_FTP_PORT_FAILED              30 /* FTP PORT operation failed */
#define HB_CURLE_FTP_COULDNT_USE_REST         31 /* the REST command failed */
#define HB_CURLE_OBSOLETE32                   32 /* NOT USED */
#define HB_CURLE_RANGE_ERROR                  33 /* RANGE "command" didn't work */
#define HB_CURLE_HTTP_POST_ERROR              34 /* */
#define HB_CURLE_SSL_CONNECT_ERROR            35 /* wrong when connecting with SSL */
#define HB_CURLE_BAD_DOWNLOAD_RESUME          36 /* couldn't resume download */
#define HB_CURLE_FILE_COULDNT_READ_FILE       37 /* */
#define HB_CURLE_LDAP_CANNOT_BIND             38 /* */
#define HB_CURLE_LDAP_SEARCH_FAILED           39 /* */
#define HB_CURLE_OBSOLETE40                   40 /* NOT USED */
#define HB_CURLE_FUNCTION_NOT_FOUND           41 /* */
#define HB_CURLE_ABORTED_BY_CALLBACK          42 /* */
#define HB_CURLE_BAD_FUNCTION_ARGUMENT        43 /* */
#define HB_CURLE_OBSOLETE44                   44 /* NOT USED */
#define HB_CURLE_INTERFACE_FAILED             45 /* CURLOPT_INTERFACE failed */
#define HB_CURLE_OBSOLETE46                   46 /* NOT USED */
#define HB_CURLE_TOO_MANY_REDIRECTS           47 /* catch endless re-direct loops */
#define HB_CURLE_UNKNOWN_OPTION               48 /* User specified an unknown option */
#define HB_CURLE_UNKNOWN_TELNET_OPTION        HB_CURLE_UNKNOWN_OPTION
#define HB_CURLE_TELNET_OPTION_SYNTAX         49 /* Malformed telnet option */
#define HB_CURLE_OBSOLETE50                   50 /* NOT USED */
#define HB_CURLE_PEER_FAILED_VERIFICATION     51 /* peer's certificate or fingerprint wasn't verified fine */
#define HB_CURLE_GOT_NOTHING                  52 /* when this is a specific error */
#define HB_CURLE_SSL_ENGINE_NOTFOUND          53 /* SSL crypto engine not found */
#define HB_CURLE_SSL_ENGINE_SETFAILED         54 /* can not set SSL crypto engine as default */
#define HB_CURLE_SEND_ERROR                   55 /* failed sending network data */
#define HB_CURLE_RECV_ERROR                   56 /* failure in receiving network data */
#define HB_CURLE_OBSOLETE57                   57 /* NOT IN USE */
#define HB_CURLE_SSL_CERTPROBLEM              58 /* problem with the local certificate */
#define HB_CURLE_SSL_CIPHER                   59 /* couldn't use specified cipher */
#define HB_CURLE_SSL_CACERT                   60 /* problem with the CA cert (path?) */
#define HB_CURLE_BAD_CONTENT_ENCODING         61 /* Unrecognized transfer encoding */
#define HB_CURLE_LDAP_INVALID_URL             62 /* Invalid LDAP URL */
#define HB_CURLE_FILESIZE_EXCEEDED            63 /* Maximum file size exceeded */
#define HB_CURLE_USE_SSL_FAILED               64 /* Requested FTP SSL level failed */
#define HB_CURLE_SEND_FAIL_REWIND             65 /* Sending the data requires a rewind that failed */
#define HB_CURLE_SSL_ENGINE_INITFAILED        66 /* failed to initialise ENGINE */
#define HB_CURLE_LOGIN_DENIED                 67 /* user, password or similar was not accepted and we failed to login */
#define HB_CURLE_TFTP_NOTFOUND                68 /* file not found on server */
#define HB_CURLE_TFTP_PERM                    69 /* permission problem on server */
#define HB_CURLE_REMOTE_DISK_FULL             70 /* out of disk space on server */
#define HB_CURLE_TFTP_ILLEGAL                 71 /* Illegal TFTP operation */
#define HB_CURLE_TFTP_UNKNOWNID               72 /* Unknown transfer ID */
#define HB_CURLE_REMOTE_FILE_EXISTS           73 /* File already exists */
#define HB_CURLE_TFTP_NOSUCHUSER              74 /* No such user */
#define HB_CURLE_CONV_FAILED                  75 /* conversion failed */
#define HB_CURLE_CONV_REQD                    76 /* caller must register conversion callbacks using curl_easy_setopt options CURLOPT_CONV_FROM_NETWORK_FUNCTION, CURLOPT_CONV_TO_NETWORK_FUNCTION, and CURLOPT_CONV_FROM_UTF8_FUNCTION */
#define HB_CURLE_SSL_CACERT_BADFILE           77 /* could not load CACERT file, missing or wrong format */
#define HB_CURLE_REMOTE_FILE_NOT_FOUND        78 /* remote file not found */
#define HB_CURLE_SSH                          79 /* error from the SSH layer, somewhat generic so the error message will be of interest when this has happened */
#define HB_CURLE_SSL_SHUTDOWN_FAILED          80 /* Failed to shut down the SSL connection */
#define HB_CURLE_AGAIN                        81 /* socket is not ready for send/recv, wait till it's ready and try again */
#define HB_CURLE_SSL_CRL_BADFILE              82 /* could not load CRL file, missing or wrong format (Added in 7.19.0) */
#define HB_CURLE_SSL_ISSUER_ERROR             83 /* Issuer check failed. (Added in 7.19.0) */
#define HB_CURLE_FTP_PRET_FAILED              84 /* a PRET command failed */
#define HB_CURLE_RTSP_CSEQ_ERROR              85 /* mismatch of RTSP CSeq numbers */
#define HB_CURLE_RTSP_SESSION_ERROR           86 /* mismatch of RTSP Session Identifiers */
#define HB_CURLE_FTP_BAD_FILE_LIST            87 /* unable to parse FTP file list */
#define HB_CURLE_CHUNK_FAILED                 88 /* chunk callback reported error */

#endif /* HBCURL_CH_ */
