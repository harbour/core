/*
 * $Id$
 */


/* Copyright Abandoned 1996 TCX DataKonsult AB & Monty Program KB & Detron HB
   This file is public domain and comes with NO WARRANTY of any kind */

/*
** Common definition between mysql server & client
*/

#ifndef _mysql_com_h
#define _mysql_com_h
#ifdef	__cplusplus
extern "C" {
#endif

#define NAME_LEN	64		/* Field/table name length */
#define LOCAL_HOST	"localhost"

#define MYSQL_PORT	3306		/* Alloced by ISI for MySQL */
#define MYSQL_UNIX_ADDR "\\socket\\mysql.sock"

enum enum_server_command {COM_SLEEP,COM_QUIT,COM_INIT_DB,COM_QUERY,
			  COM_FIELD_LIST,COM_CREATE_DB,COM_DROP_DB,COM_REFRESH,
			  COM_SHUTDOWN,COM_STATISTICS,
			  COM_PROCESS_INFO,COM_CONNECT,COM_PROCESS_KILL,
			  COM_DEBUG};

#define NOT_NULL_FLAG	1		/* Field can't be NULL */
#define PRI_KEY_FLAG	2		/* Field is part of a primary key */
#define UNIQUE_KEY_FLAG 4		/* Field is part of a unique key */
#define MULTIPLE_KEY_FLAG 8		/* Field is part of a key */
#define BLOB_FLAG	16		/* Field is a blob */
#define UNSIGNED_FLAG	32		/* Field is unsigned */
#define ZEROFILL_FLAG	64		/* Field is zerofill */
#define BINARY_FLAG	128
/* The following are only sent to new clients */
#define ENUM_FLAG	256		/* field is an enum */
#define AUTO_INCREMENT_FLAG 512		/* field is a autoincrement field */
#define TIMESTAMP_FLAG	1024		/* Field is a timestamp */
#define PART_KEY_FLAG	16384		/* Intern; Part of some key */
#define GROUP_FLAG	32768		/* Intern group field */

#define REFRESH_GRANT		1	/* Refresh grant tables */
#define REFRESH_LOG		2	/* Start on new log file */
#define REFRESH_TABLES		4	/* close all tables */
#define REFRESH_HOSTS		8	/* Flush host cache */
#define REFRESH_FAST		32768	/* Intern flag */

#define CLIENT_LONG_PASSWORD	1	/* new more secure passwords */
#define CLIENT_FOUND_ROWS	2	/* Found instead of affected rows */
#define CLIENT_LONG_FLAG	4	/* Get all column flags */
#define CLIENT_CONNECT_WITH_DB	8	/* One can specify db on connect */
#define CLIENT_NO_SCHEMA	16	/* Don't allow database.table.column */

#define MYSQL_ERRMSG_SIZE 200
#define NET_READ_TIMEOUT 30			/* Timeout on read */
#define NET_WRITE_TIMEOUT 60			/* Timeout on write */
#define NET_WAIT_TIMEOUT 8*60*60		/* Wait for new query */

typedef struct st_net {
  Socket fd;
  int fcntl;
  unsigned char *buff,*buff_end,*write_pos;
  char last_error[MYSQL_ERRMSG_SIZE];
  unsigned int last_errno,max_packet,timeout,pkt_nr;
  my_bool error,return_errno;
} NET;

#define packet_error ((unsigned int) -1)

enum enum_field_types { FIELD_TYPE_DECIMAL, FIELD_TYPE_TINY,
			FIELD_TYPE_SHORT,  FIELD_TYPE_LONG,
			FIELD_TYPE_FLOAT,  FIELD_TYPE_DOUBLE,
			FIELD_TYPE_NULL,   FIELD_TYPE_TIMESTAMP,
			FIELD_TYPE_LONGLONG,FIELD_TYPE_INT24,
			FIELD_TYPE_DATE,   FIELD_TYPE_TIME,
			FIELD_TYPE_DATETIME, FIELD_TYPE_YEAR,
			FIELD_TYPE_NEWDATE,
			FIELD_TYPE_ENUM=247,
			FIELD_TYPE_SET=248,
			FIELD_TYPE_TINY_BLOB=249,
			FIELD_TYPE_MEDIUM_BLOB=250,
			FIELD_TYPE_LONG_BLOB=251,
			FIELD_TYPE_BLOB=252,
			FIELD_TYPE_VAR_STRING=253,
			FIELD_TYPE_STRING=254
};

#define FIELD_TYPE_CHAR FIELD_TYPE_TINY		/* For compability */
#define FIELD_TYPE_INTERVAL FIELD_TYPE_ENUM	/* For compability */

extern unsigned long max_allowed_packet;
extern unsigned long net_buffer_length;

#define net_new_transaction(net) ((net)->pkt_nr=0)
int	my_net_init(NET *net,Socket fd);
void	net_end(NET *net);
void	net_clear(NET *net);
int	net_flush(NET *net);
int	my_net_write(NET *net,const byte *packet,unsigned int len);
int	net_write_command(NET *net,unsigned char command,const byte *packet,
			  unsigned int len);
int	net_real_write(NET *net,const byte *packet,unsigned int len);
unsigned int	my_net_read(NET *net);

struct rand_struct {
  unsigned long seed,seed2,max_value;
  double max_value_dbl;
};

  /* The following is for user defined functions */

enum Item_result {STRING_RESULT,REAL_RESULT,INT_RESULT};

typedef struct st_udf_args
{
  unsigned int arg_count;		/* Number of arguments */
  enum Item_result *arg_type;		/* Pointer to item_results */
  char **args;				/* Pointer to argument */
  unsigned long *lengths;		/* Length of string arguments */
} UDF_ARGS;

  /* This holds information about the result */

typedef struct st_udf_init
{
  my_bool maybe_null;			/* 1 if function can return NULL */
  unsigned int decimals;		/* for real functions */
  unsigned int max_length;		/* For string functions */
  char	  *ptr;				/* free pointer for function data */
} UDF_INIT;

  /* Prototypes to password functions */

void randominit(struct rand_struct *rand,unsigned long seed1,
		unsigned long seed2);
double rnd(struct rand_struct *rand);
void make_scrambled_password(char *to,const char *password);
void get_salt_from_password(unsigned long *res,const char *password);
char *scramble(char *to,const char *message,const char *password,
	       my_bool old_ver);
my_bool check_scramble(const char *scramble,const char *message,
		       unsigned long *salt,my_bool old_ver);
char *get_tty_password(char *opt_message);

#define NULL_LENGTH ((unsigned long) ~0) /* For net_store_length */

#ifdef __WIN32__
#define socket_errno WSAGetLastError()
#else
#define socket_errno errno
#endif

#ifdef	__cplusplus
}
#endif
#endif
