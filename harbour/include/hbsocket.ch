/*
 * $Id$
 */

/*
 * Harbour Project source code:
 *    socket related constant values
 *
 * Copyright 2009 Przemyslaw Czerpak <druzus / at / priv.onet.pl>
 * www - http://www.harbour-project.org
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

#ifndef HB_SOCK_CH_
#define HB_SOCK_CH_

/* Harbour socket error codes */
#define HB_SOCKET_ERR_PIPE                1
#define HB_SOCKET_ERR_TIMEOUT             2
#define HB_SOCKET_ERR_WRONGADDR           3
#define HB_SOCKET_ERR_AFNOSUPPORT         4
#define HB_SOCKET_ERR_PFNOSUPPORT         5
#define HB_SOCKET_ERR_PROTONOSUPPORT      6
#define HB_SOCKET_ERR_PARAMVALUE          7
#define HB_SOCKET_ERR_NOSUPPORT           8
#define HB_SOCKET_ERR_NORESOURCE          9
#define HB_SOCKET_ERR_ACCESS              10
#define HB_SOCKET_ERR_ADDRINUSE           11
#define HB_SOCKET_ERR_INTERRUPT           12
#define HB_SOCKET_ERR_ALREADYCONNECTED    13
#define HB_SOCKET_ERR_CONNREFUSED         14
#define HB_SOCKET_ERR_CONNABORTED         15
#define HB_SOCKET_ERR_CONNRESET           16
#define HB_SOCKET_ERR_NETUNREACH          17
#define HB_SOCKET_ERR_NETDOWN             18
#define HB_SOCKET_ERR_NETRESET            19
#define HB_SOCKET_ERR_INPROGRESS          20
#define HB_SOCKET_ERR_ALREADY             21
#define HB_SOCKET_ERR_ADDRNOTAVAIL        22
#define HB_SOCKET_ERR_READONLY            23
#define HB_SOCKET_ERR_AGAIN               24
#define HB_SOCKET_ERR_INVALIDHANDLE       25
#define HB_SOCKET_ERR_INVAL               26
#define HB_SOCKET_ERR_PROTO               27
#define HB_SOCKET_ERR_PROTOTYPE           28
#define HB_SOCKET_ERR_NOFILE              29
#define HB_SOCKET_ERR_NOBUFS              30
#define HB_SOCKET_ERR_NOMEM               31
#define HB_SOCKET_ERR_FAULT               32
#define HB_SOCKET_ERR_NAMETOOLONG         33
#define HB_SOCKET_ERR_NOENT               34
#define HB_SOCKET_ERR_NOTDIR              35
#define HB_SOCKET_ERR_LOOP                36
#define HB_SOCKET_ERR_MSGSIZE             37
#define HB_SOCKET_ERR_DESTADDRREQ         38
#define HB_SOCKET_ERR_NOPROTOOPT          39
#define HB_SOCKET_ERR_NOTCONN             40
#define HB_SOCKET_ERR_SHUTDOWN            41
#define HB_SOCKET_ERR_TOOMANYREFS         42
#define HB_SOCKET_ERR_RESTARTSYS          43
#define HB_SOCKET_ERR_NOSR                44
#define HB_SOCKET_ERR_HOSTDOWN            45
#define HB_SOCKET_ERR_HOSTUNREACH         46
#define HB_SOCKET_ERR_NOTEMPTY            47
#define HB_SOCKET_ERR_USERS               48
#define HB_SOCKET_ERR_DQUOT               49
#define HB_SOCKET_ERR_STALE               50
#define HB_SOCKET_ERR_REMOTE              51
#define HB_SOCKET_ERR_PROCLIM             52
#define HB_SOCKET_ERR_DISCON              53
#define HB_SOCKET_ERR_NOMORE              54
#define HB_SOCKET_ERR_CANCELLED           55
#define HB_SOCKET_ERR_INVALIDPROCTABLE    56
#define HB_SOCKET_ERR_INVALIDPROVIDER     57
#define HB_SOCKET_ERR_PROVIDERFAILEDINIT  58
#define HB_SOCKET_ERR_REFUSED             59
#define HB_SOCKET_ERR_SYSNOTREADY         60
#define HB_SOCKET_ERR_VERNOTSUPPORTED     61
#define HB_SOCKET_ERR_NOTINITIALISED      62
#define HB_SOCKET_ERR_TRYAGAIN            63
#define HB_SOCKET_ERR_HOSTNOTFOUND        64
#define HB_SOCKET_ERR_NORECOVERY          65
#define HB_SOCKET_ERR_NODATA              66
#define HB_SOCKET_ERR_SYSCALLFAILURE      67
#define HB_SOCKET_ERR_SERVICENOTFOUND     68
#define HB_SOCKET_ERR_TYPENOTFOUND        69
#define HB_SOCKET_ERR_OTHER               70


/* protocol family */
#define HB_SOCK_PF_LOCAL      1
#define HB_SOCK_PF_INET       2
#define HB_SOCK_PF_IPX        4
#define HB_SOCK_PF_INET6      10
#define HB_SOCK_PF_PACKET     17

/* address familly */
#define HB_SOCK_AF_LOCAL      HB_SOCK_PF_LOCAL
#define HB_SOCK_AF_INET       HB_SOCK_PF_INET
#define HB_SOCK_AF_IPX        HB_SOCK_PF_IPX
#define HB_SOCK_AF_INET6      HB_SOCK_PF_INET6
#define HB_SOCK_AF_PACKET     HB_SOCK_PF_PACKET

/* protocol type */
#define HB_SOCK_STREAM        1
#define HB_SOCK_DGRAM         2
#define HB_SOCK_RAW           3
#define HB_SOCK_RDM           4
#define HB_SOCK_SEQPACKET     5

/* IP protocols */
#define HB_SOCK_IPPROTO_IP          0     /* Dummy protocol for TCP */
#define HB_SOCK_IPPROTO_ICMP        1     /* Internet Control Message Protocol */
#define HB_SOCK_IPPROTO_IGMP        2     /* Internet Group Management Protocol */
#define HB_SOCK_IPPROTO_IPIP        4     /* IPIP tunnels (older KA9Q tunnels use 94) */
#define HB_SOCK_IPPROTO_TCP         6     /* Transmission Control Protocol */
#define HB_SOCK_IPPROTO_EGP         8     /* Exterior Gateway Protocol */
#define HB_SOCK_IPPROTO_PUP         12    /* PUP protocol */
#define HB_SOCK_IPPROTO_UDP         17    /* User Datagram Protocol */
#define HB_SOCK_IPPROTO_IDP         22    /* XNS IDP protocol */
#define HB_SOCK_IPPROTO_DCCP        23    /* DCCP protocol */
#define HB_SOCK_IPPROTO_RDP         29    /* RDP */
#define HB_SOCK_IPPROTO_TP          29    /* SO Transport Protocol Class 4 */
#define HB_SOCK_IPPROTO_IPV6        41    /* IPv6 header */
#define HB_SOCK_IPPROTO_ROUTING     43    /* IPv6 routing header */
#define HB_SOCK_IPPROTO_FRAGMENT    44    /* IPv6 fragmentation header */
#define HB_SOCK_IPPROTO_RSVP        46    /* Reservation Protocol */
#define HB_SOCK_IPPROTO_GRE         47    /* General Routing Encapsulation */
#define HB_SOCK_IPPROTO_ESP         50    /* Encapsulating security payload */
#define HB_SOCK_IPPROTO_AH          51    /* Authentication header */
#define HB_SOCK_IPPROTO_ICMPV6      58    /* ICMP v6 */
#define HB_SOCK_IPPROTO_NONE        59    /* IPv6 no next header */
#define HB_SOCK_IPPROTO_DSTOPTS     60    /* IPv6 destination options */
#define HB_SOCK_IPPROTO_ND          77    /* ND */
#define HB_SOCK_IPPROTO_ICLFXBM     78    /* ICLFXBM */
#define HB_SOCK_IPPROTO_EON         80    /* EON */
#define HB_SOCK_IPPROTO_MTP         92    /* Multicast Transport Protocol */
#define HB_SOCK_IPPROTO_ENCAP       98    /* Encapsulation Header */
#define HB_SOCK_IPPROTO_PIM         103   /* Protocol Independent Multicast */
#define HB_SOCK_IPPROTO_COMP        108   /* Compression Header Protocol */
#define HB_SOCK_IPPROTO_PGM         113   /* PGM */
#define HB_SOCK_IPPROTO_L2TP        115   /* L2TP */
#define HB_SOCK_IPPROTO_SCTP        132   /* Stream Control Transmission Protocol */
#define HB_SOCK_IPPROTO_RAW         255   /* Raw IP packets */

/* shutdown actions */
#define HB_SOCK_SHUT_RD       0
#define HB_SOCK_SHUT_WR       1
#define HB_SOCK_SHUT_RDWR     2


#endif /* HB_SOCK_CH_ */
