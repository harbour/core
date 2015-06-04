#ifndef CBIND_C_API_H
#define CBIND_C_API_H

/*

\file
  C binding header file - c_api.h

\verbatim
 Copyright (c) 2002-2005 by InterSystems.
 Cambridge, Massachusetts, U.S.A.  All rights reserved.
\endverbatim

\verbatim
Source Control Info:
  Revision path: $Id: //dev/cache2007.1.x/modules/cpp/c/c_api.h#1 $
  Last Change:   $Date: 2006/06/05 $
  Modified by:   $Author: woodfin $
\endverbatim

*/

#include <wchar.h>

typedef unsigned long byte_size_t;
typedef unsigned long char_size_t;
typedef unsigned short bool_t;

/* ============= */

#define NULL_HANDLE 0

typedef int h_connection;
typedef int h_database;
typedef int h_query;
typedef int h_objref;

typedef void* h_variant;
typedef void* h_class_def;
typedef void* h_prop_def;
typedef void* h_mtd_def;
typedef void* h_arg_def;

typedef const wchar_t* const_name_t;

/* make the structures compiler independent */
#ifdef _WIN32
#include <pshpack1.h>
#endif

/* constants for cpp_type */

#define CBIND_VOID -1
#define CBIND_OBJ_ID 0
#define CBIND_INT_ID 1
#define CBIND_DOUBLE_ID 2
#define CBIND_BINARY_ID 3
#define CBIND_STRING_ID 4
#define CBIND_STATUS_ID 5
#define CBIND_TIME_ID 6
#define CBIND_DATE_ID 7
#define CBIND_TIMESTAMP_ID 8
#define CBIND_BOOL_ID 9
#define CBIND_CURRENCY_ID 10
#define CBIND_DLIST_ID 11

/* constants for uni_t*/
/* the following constants are used in string functions and solve the problem that UNICODE on some platforms such as Python is always
   2 bytes regardless of whether UNIX or WINDOWS - and for these we use
 CACHE_UNICODE while on other platforms it is 2 bytes (MS Windows) or 4 bytes (UNIX) and for these we use CPP_UNICODE */
#define MULTIBYTE 0
#define CACHE_UNICODE 1
#define CPP_UNICODE 2

/* options that return information about mtd_def, prop_def or arg_def */
#define OPTION_CPP_TYPE 1
#define OPTION_CACHE_TYPE 2
#define OPTION_NAME 3
#define OPTION_IS_FUNC 4
#define OPTION_IS_CLS_MTD 5
#define OPTION_NUM_ARGS 6
#define OPTION_ARGS_INFO 7
#define OPTION_IS_BY_REF 8
#define OPTION_IS_DEFAULT 9
#define OPTION_DEF_VAL 10
#define OPTION_DEF_VAL_SIZE 11

#define MAXCLASSNAME 8193

/* make the structures compiler independent */
#ifdef _WIN32
#include <poppack.h>
#endif

#define RETURN_STATUS_CODE 0
#define RETURN_SUCCESS 1

enum return_status_mode { ALWAYS_SUCCESS, RETURN_STATUS };

/* ============= */

#if defined(__cplusplus)
extern "C" {
#endif

   int cbind_get_last_err_code();
   const char* cbind_get_last_err_msg();

   const char* cbind_get_client_version();

   void cbind_set_ignore_null(bool_t val);

   /* interface for unicode conversions */

   char* cbind_setlocale(int category, const char *locale);
   int cbind_set_thread_locale(int lcid);

   int cbind_mb_to_uni(const char* src_buf, byte_size_t src_size,
                       wchar_t* dest_buf, char_size_t dest_cap, int* size);

   int cbind_uni_to_mb(const wchar_t* src_buf, char_size_t src_size,
                       char* dest_buf, byte_size_t dest_cap, int* size);

   int cbind_uni_to_cache_uni(const wchar_t* src_buf, char_size_t src_size,
                              char* dest_buf, byte_size_t dest_cap, int* size);

   int cbind_cache_uni_to_uni(const char* src_buf, byte_size_t src_size,
                              wchar_t* dest_buf, char_size_t dest_cap, int* size);

   int cbind_uni_to_utf8(const wchar_t* src_buf, char_size_t src_size,
                         char* dest_buf, byte_size_t dest_cap, int* size);

   int cbind_utf8_to_uni(const char* src_buf, byte_size_t src_size,
                         wchar_t* dest_buf, char_size_t dest_cap, int* size);

   /* interface for d_list */

   int cbind_dlist_calc_num_elems(const char* buf, int size, int* res);

   int cbind_dlist_get_elem_size(const char* buf, int* val);
   int cbind_dlist_get_elem_type(const char* buf, char* val);
   int cbind_dlist_is_elem_int(const char* buf, bool_t* val);
   int cbind_dlist_is_elem_double(const char* buf, bool_t* val);
   int cbind_dlist_is_elem_str(const char* buf, bool_t* val);
   int cbind_dlist_is_elem_null(const char* buf, bool_t* val);

   int cbind_dlist_get_elem_as_int(const char* buf, int* val, int* elem_size);
   int cbind_dlist_get_elem_as_double(const char* buf, double* val, int* elem_size);
   int cbind_dlist_get_str_elem(const char* buf,
                                bool_t* is_uni, const char** pbuf, int* psize,
                                int* elem_size);

   int cbind_dlist_put_int_elem(char* buf, int cap, int val, int* elem_size);
   int cbind_dlist_put_double_elem(char* buf, int cap, double val, int* elem_size);
   int cbind_dlist_put_str_elem(char* buf, int cap,
                                bool_t is_uni, const char* val_buf, int val_size,
                                int* elem_size);
   int cbind_dlist_put_null_elem(char* buf, int cap, int* elem_size);

   /* interface for d_connection */

   int cbind_alloc_conn(const wchar_t* conn_str,
                        const wchar_t* user,
                        const wchar_t* pwd,
                        int timeout,
                        h_connection* res);

   int cbind_alloc_secure_conn(const wchar_t* conn_str,
                               const wchar_t* srv_principal_name,
                               int security_level,
                               int timeout,
                               h_connection* res);

   int cbind_free_conn(h_connection conn);

   /* interface for Database */

   int cbind_alloc_db(h_connection conn, h_database* res);
   int cbind_free_db(h_database db);

   int cbind_is_uni_srv(h_database db, bool_t* res);

   int cbind_tstart(h_database db);
   int cbind_tcommit(h_database db);
   int cbind_trollback(h_database db);
   int cbind_tlevel(h_database db, int* level);

   /* interface for Class_def */

   int cbind_alloc_class_def(h_database db,
                             const wchar_t* name,
                             h_class_def* res);
   int cbind_free_class_def(h_database db, h_class_def cl_def);

   int cbind_alloc_prop_def(h_prop_def* res);
   int cbind_free_prop_def(h_prop_def prop_def);

   int cbind_alloc_mtd_def(h_mtd_def *res);
   int cbind_free_mtd_def(h_mtd_def mtd_def);

   int cbind_alloc_arg_def(h_arg_def* res);
   int cbind_free_arg_def(h_arg_def arg_def);

   int cbind_get_prop_def(h_class_def cl_def,
                          const wchar_t* name,
                          h_prop_def res);
   int cbind_get_dyn_prop_def(h_database db,
                              h_objref oref,
                              const wchar_t* name,
                              h_prop_def res);

   int cbind_get_prop_cpp_type(h_prop_def h_def, short *val);
   int cbind_get_prop_cache_type(h_prop_def h_def, const wchar_t **val);
   int cbind_get_prop_name(h_prop_def h_def, const wchar_t **val);

   int cbind_get_mtd_def(h_class_def cl_def,
                         const wchar_t* name,
                         h_mtd_def h_def);
   int cbind_get_dyn_mtd_def(h_database db,
                             h_objref oref,
                             const wchar_t* name,
                             h_mtd_def res);

   int cbind_get_mtd_is_func(h_mtd_def h_def, bool_t *is_func);
   int cbind_get_mtd_cpp_type(h_mtd_def h_def, short *cpp_type);
   int cbind_get_mtd_cache_type(h_mtd_def h_def, const wchar_t** cache_type);
   int cbind_get_mtd_is_cls_mtd(h_mtd_def h_def, bool_t *is_cls_mtd);
   int cbind_get_mtd_num_args(h_mtd_def h_def, int *num_args);
   int cbind_get_mtd_args_info(h_mtd_def h_def, void ** args_info);
   int cbind_get_mtd_name(h_mtd_def h_def, const wchar_t **name);

   int cbind_get_mtd_info(h_mtd_def h_def, int option, void *p_info);  // p_info is address of variable to hold returned information

   int cbind_mtd_rewind_args(h_mtd_def mtd_def);
   int cbind_get_arg_cpp_type(h_arg_def h_def, short *cpp_type);
   int cbind_get_arg_cache_type(h_arg_def h_def, const wchar_t **cache_type);
   int cbind_get_arg_name(h_arg_def h_def, const wchar_t **name);
   int cbind_get_arg_is_by_ref(h_arg_def h_def, bool_t *is_by_ref);
   int cbind_get_arg_is_default(h_arg_def h_def, bool_t *is_default);
   int cbind_get_arg_def_val(h_arg_def h_def, const char **def_val);
   int cbind_get_arg_def_val_size(h_arg_def h_def, long *def_val_size);

   int cbind_mtd_arg_get(h_mtd_def mtd_def, h_arg_def arg_def);
   int cbind_mtd_arg_next(h_mtd_def mtd_def);

   int cbind_get_next_prop_def(h_class_def h_cl_def, h_prop_def prop_def, bool_t *p_at_end);
   int cbind_get_next_mtd_def(h_class_def h_cl_def, h_mtd_def mtd_def, bool_t *p_at_end);

   int cbind_reset_prop_defs(h_class_def h_cl_def);
   int cbind_reset_mtd_defs(h_class_def h_cl_def);

   /* interface for Dyn_obj */

   int cbind_create_new(h_database db,
                        const wchar_t* type,
                        const wchar_t* init_val,
                        h_objref* res);

   int cbind_open(h_database db,
                  const wchar_t* type,
                  const char* oid,
                  int concurrency,
                  int timeout,
                  h_objref* res);

   int cbind_openid(h_database db,
                    const wchar_t* type,
                    const wchar_t* id,
                    int concurrency,
                    int timeout,
                    h_objref* res);

   int cbind_object_release(h_database db, h_objref oref);

   int cbind_object_add_ref(h_database db, h_objref oref);

   int cbind_set_next_arg_as_obj(h_database h_db, int oref, const_name_t cl_name, bool_t by_ref);

   int cbind_set_next_arg_as_double(h_database h_db, double val, bool_t by_ref);

   int cbind_set_next_arg_as_bin(h_database h_db, const void* buf, byte_size_t size, bool_t by_ref);

   int cbind_set_next_arg_as_status(h_database h_db, const char* buf, byte_size_t size, bool_t by_ref);

   int cbind_set_next_arg_as_time(h_database h_db, int hour, int minute, int second, bool_t by_ref);

   int cbind_set_next_arg_as_date(h_database h_db, int year, int month, int day, bool_t by_ref);

   int cbind_set_next_arg_as_timestamp(h_database h_db, int year, int month, int day, int hour, int minute, int second, int fraction, bool_t by_ref);

   int cbind_set_next_arg_as_bool(h_database h_db, bool_t val, bool_t by_ref);

   int cbind_set_next_arg_as_cy(h_database h_db, double val, bool_t by_ref); // currency

   int cbind_set_next_arg_as_dlist(h_database h_db, const char* buf, byte_size_t size, bool_t by_ref);

   int cbind_set_next_arg_as_null(h_database h_db, int type_id, bool_t by_ref);

   int cbind_set_next_arg_as_int(h_database h_db, int val, bool_t by_ref);

   int cbind_set_next_arg_as_str(h_database h_db, const char* buf, byte_size_t size, int type, bool_t by_ref);

   int cbind_set_next_arg_as_res(h_database h_db, int type_id);

   int cbind_get_arg_as_obj(h_database h_db, int idx, int* oref, const wchar_t** cl_name, char_size_t* len, bool_t *p_is_null);

   int cbind_get_arg_as_double(h_database db, int idx, double* val, bool_t *p_is_null);

   int cbind_get_arg_as_bin(h_database h_db, int idx, char* buf, byte_size_t cap, byte_size_t* p_size, bool_t *p_is_null);

   int cbind_get_arg_as_time(h_database h_db, int idx, int *hour, int *minute, int *second, bool_t *p_is_null);

   int cbind_get_arg_as_date(h_database h_db, int idx, int *month, int *year, int *day, bool_t *p_is_null);

   int cbind_get_arg_as_timestamp(h_database h_db, int idx, int *year, int *month, int *day, int *hour, int *minute, int *second, int *fraction, bool_t *p_is_null);

   int cbind_get_arg_as_bool(h_database h_db, int idx, bool_t* val, bool_t *p_is_null);

   int cbind_get_arg_as_cy(h_database h_db, int idx, double* val, bool_t *p_is_null);  // currency

   int cbind_get_arg_as_dlist(h_database h_db, int idx, char* buf, byte_size_t cap, byte_size_t* size, bool_t *p_is_null);

   int cbind_get_arg_as_int(h_database h_db, int idx, int* val, bool_t *p_is_null);

   int cbind_get_arg_as_str(h_database h_db, int idx, char* buf, byte_size_t cap, int uni_type, byte_size_t *p_size, bool_t *p_is_null);

   int cbind_get_arg_as_status(h_database h_db, int idx, int* code, char* buf, byte_size_t cap, int uni_type, byte_size_t* size, bool_t *p_is_null);

   int cbind_run_method(h_database h_db, int oref, const_name_t cl_name, const_name_t mtd_name);

   int cbind_get_prop(h_database h_db, int oref, const_name_t prop_name);

   int cbind_set_prop(h_database h_db, int oref, const_name_t prop_name);

   int cbind_get_num_args(h_database h_db, int *p_num_args);

   int cbind_get_is_null(h_database h_db, int idx, bool_t *p_is_null);

   /* interface for d_query */

   int cbind_alloc_query(h_database db, h_query* res);
   int cbind_free_query(h_query query);

   int cbind_prepare_gen_query(h_query query, const wchar_t* sql_query, int* sql_code);
   int cbind_prepare_class_query(h_query query,
                                 const wchar_t* cl_name,
                                 const wchar_t* proc_name,
                                 int* sql_code);

   int cbind_query_execute(h_query query, int* sql_code);
   int cbind_query_fetch(h_query query, int* sql_code);
   int cbind_query_close(h_query query);

   int cbind_query_get_num_cols(h_query query, int* res);
   int cbind_query_get_col_sql_type(h_query query, int idx, int* res);
   int cbind_query_get_col_name(h_query query, int idx, const wchar_t** res);
   int cbind_query_get_col_name_len(h_query query, int idx, int* res);

   int cbind_query_get_num_pars(h_query query, int* res);
   int cbind_query_get_par_sql_type(h_query query, int idx, int* res);
   int cbind_query_get_par_col_size(h_query query, int idx, int* res);
   int cbind_query_get_par_num_dec_digits(h_query query, int idx, int* res);
   int cbind_query_is_par_nullable(h_query query, int idx, bool_t* res);
   int cbind_query_is_par_unbound(h_query query, int idx, bool_t* res);

   int cbind_query_set_int_par(h_query query, int idx, int val);
   int cbind_query_set_double_par(h_query query, int idx, double val);
   int cbind_query_set_mb_str_par(h_query query, int idx, const char* buf, int size);
   int cbind_query_set_bin_par(h_query query, int idx, const void* buf, int size);
   int cbind_query_set_uni_str_par(h_query query, int idx, const wchar_t* buf, int size);
   int cbind_query_set_time_par(h_query query, int idx, int hour, int minute, int second);
   int cbind_query_set_date_par(h_query query, int idx, int year, int month, int day);
   int cbind_query_set_timestamp_par(h_query query, int idx, int year, int month, int day, int hour, int minute, int second, int fraction);

   int cbind_query_get_int_data(h_query query, int* res, bool_t* is_null);
   int cbind_query_get_double_data(h_query query, double* res, bool_t* is_null);
   int cbind_query_get_mb_str_data(h_query query, char* buf, int cap, int* psize, bool_t* is_null);
   int cbind_query_get_uni_str_data(h_query query, wchar_t* buf, int cap, int* psize, bool_t* is_null);
   int cbind_query_get_bin_data(h_query query, void* buf, int cap, int *psize, bool_t* is_null);
   int cbind_query_get_time_data(h_query query, int *hour, int *minute, int *second, bool_t* is_null);
   int cbind_query_get_date_data(h_query query, int *year, int *month, int *day, bool_t* is_null);
   int cbind_query_get_timestamp_data(h_query query, int *year, int *month, int *day, int *hour, int *minute, int *second, int *fraction, bool_t* is_null);

   int cbind_query_skip(h_query query, int num_cols);
   int cbind_query_get_cur_idx(h_query query, int* res);

   int cbind_reset_args(h_database h_db);

#ifdef __cplusplus
}
#endif

#endif
