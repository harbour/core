#
# $Id$
#

ROOT = ../../

C_SOURCES =\
	hbsqlit2.c \
	attach.c \
	auth.c \
	btree.c \
	btree_rb.c \
	build.c \
	copy.c \
	date.c \
	delete.c \
	encode.c \
	expr.c \
	func.c \
	hash.c \
	insert.c \
	main.c \
	opcodes.c \
	os.c \
	pager.c \
	parse.c \
	pragma.c \
	printf.c \
	random.c \
	select.c \
	table.c \
	tokenize.c \
	trigger.c \
	update.c \
	util.c \
	vacuum.c \
	vdbe.c \
	vdbeaux.c \
	where.c \

PRG_HEADERS=\
	hbsqlit2.ch

LIBNAME=hbsqlit2

include $(TOP)$(ROOT)config/header.cf
INSTALL_RULE_HEADERS := $(INSTALL_RULE)
include $(TOP)$(ROOT)config/lib.cf

install::
	$(INSTALL_RULE_HEADERS)
