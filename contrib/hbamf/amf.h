/*
 * $Id$
 */

/*******
 *
 *  amf.h
 *
 *  Aleksander Czajczynski <hb/at/fki.pl> 2011-2012
 *
 *  amf.h - AMF3 headers
 *
 *  Based on a AmFast C library for Python by Dave Thompson
 *
 ********/

#ifndef __HBAMF_H
#define __HBAMF_H

/* Things used by both the encoder and the decoder. */

/* Use to test for endianness at run time. */
/* #define is_bigendian() ((*(char*)&endian_test) == 0) */

/* ---- Harbour support */

#define OBJAMF_VER         0

#define OBJAMF_VAR_COUNT   5
#define OBJAMF_VAR_VER     1
#define OBJAMF_VAR_NAME    2
#define OBJAMF_VAR_HASH    3

/* ---- AMF3 */

/* Valid AMF3 integer range */
#define MIN_INT            -268435457
#define MAX_INT            268435456

/* Reference bit */
#define REFERENCE_BIT      0x01

/* Empty string */
#define EMPTY_STRING_TYPE  0x01

/* Object Headers */
#define STATIC             0x03
#define DYNAMIC            0x0B
#define EXTERNALIZABLE     0x07

/* Type markers */
#define UNDEFINED_TYPE     0x00
#define NULL_TYPE          0x01
#define FALSE_TYPE         0x02
#define TRUE_TYPE          0x03
#define INT_TYPE           0x04
#define DOUBLE_TYPE        0x05
#define STRING_TYPE        0x06
#define XML_DOC_TYPE       0x07
#define DATE_TYPE          0x08
#define ARRAY_TYPE         0x09
#define OBJECT_TYPE        0x0A
#define XML_TYPE           0x0B
#define BYTE_ARRAY_TYPE    0x0C
#define AMF3_AMF0          0x11

#endif
