/*
 * $Id$
 */

/* NOTE: This is a stub to plug the Harbour ZLIB library of Harbour, 
         without the need to modify ZipArchive sources. Notice that 
         ZipArchive uses a modified ZLIB sources for large file support,
         but this is available only in the commercial version of the 
         lib anyway. [vszakats] */

#include "hbzlib.h"

//#  define zarch_deflateInit		deflateInit
#  define zarch_deflateInit_		deflateInit_
#  define zarch_deflate			deflate
#  define zarch_deflateEnd		deflateEnd
//#  define zarch_deflateInit2	deflateInit2
#  define zarch_deflateInit2_		deflateInit2_
#  define zarch_deflateSetDictionary deflateSetDictionary
#  define zarch_deflateCopy		deflateCopy
#  define zarch_deflateReset		deflateReset
#  define zarch_deflateParams		deflateParams
#  define zarch_deflateBound		deflateBound
#  define zarch_deflatePrime      deflatePrime
#  define zarch_deflateSetHeader	deflateSetHeader
#  define zarch_deflateTune		deflateTune
//#  define zarch_inflateInit		inflateInit
//#  define zarch_inflateInit2	inflateInit2
#  define zarch_inflateInit2_		inflateInit2_
#  define zarch_inflateInit_		inflateInit_
#  define zarch_inflate			inflate
#  define zarch_inflateEnd		inflateEnd
#  define zarch_inflateSetDictionary inflateSetDictionary
#  define zarch_inflateSync		inflateSync
#  define zarch_inflateSyncPoint inflateSyncPoint
#  define zarch_inflateCopy		inflateCopy
#  define zarch_inflateReset		inflateReset
//#  define zarch_inflateBackInit	inflateBackInit
#  define zarch_inflateBackInit_	inflateBackInit_
#  define zarch_inflateBack       inflateBack
#  define zarch_inflateBackEnd    inflateBackEnd
#  define zarch_inflatePrime		inflatePrime
#  define zarch_inflateGetHeader	inflateGetHeader
#  define zarch_compress			compress
#  define zarch_compress2			compress2
#  define zarch_compressBound		compressBound
#  define zarch_uncompress		uncompress
#  define zarch_adler32			adler32
#  define zarch_adler32_combine	adler32_combine
#  define zarch_crc32_combine		crc32_combine
#  define zarch_deflate_copyright deflate_copyright
#  define zarch_inflate_copyright inflate_copyright
#  define zarch_crc32				crc32
#  define zarch_get_crc_table		get_crc_table
#  define zarch_zError            zError
#  define zarch_z_stream			z_stream
#  define zarch_z_stream_s		z_stream_s
#  define zarch_alloc_func        alloc_func
#  define zarch_free_func         free_func
#  define zarch_in_func           in_func
#  define zarch_out_func          out_func
#  define zarch_Byte				Byte
#  define zarch_uInt				uInt
#  define zarch_uLong				uLong
#  define zarch_uLongLong			uLongLong
#  define zarch_Bytef				Bytef
#  define zarch_charf				charf
#  define zarch_intf				intf
#  define zarch_uIntf				uIntf
#  define zarch_uLongf			uLongf
#  define zarch_voidpf			voidpf
#  define zarch_voidp				voidp
#  define zarch_deflate_state		deflate_state
#  define zarch_deflate_slow		deflate_slow
#  define zarch_deflate_fast		deflate_fast
#  define zarch_deflate_stored	deflate_stored
#  define zarch_z_streamp			z_streamp
#  define zarch_deflate_rle		deflate_rle
#  define zarch_inflate_state		inflate_state
#  define zarch_inflate_fast		inflate_fast
#  define zarch_inflate_table		inflate_table
#  define zarch_updatewindow		updatewindow
//#  define zarch_inflate_mode	inflate_mode
//#  define zarch_send_bits		send_bits
#  define zarch_zlibVersion		zlibVersion
#  define zarch_zlibCompileFlags	zlibCompileFlags
#  define zarch_zError			zError
#  define zarch_tr_init			_tr_init
#  define zarch_tr_tally			_tr_tally
#  define zarch_tr_flush_block	_tr_flush_block
#  define zarch_tr_align			_tr_align
#  define zarch_tr_stored_block	_tr_stored_block
#  define zarch_dist_code		_dist_code
#  define zarch_length_code		_length_code
