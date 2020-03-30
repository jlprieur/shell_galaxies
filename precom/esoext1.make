########################################################################
# .COPYRIGHT:	Copyright (c) 1988 European Southern Observatory,
#						all rights reserved
# .TYPE		make file
# .NAME		$MIDASHOME/$MIDVERS/system/ext//makefile 
# .LANGUAGE	makefile syntax
# .ENVIRONMENT	Unix Systems. 
# .COMMENT	Compiles source files and generates "esoext1" command.
#
# .REMARKS
# .AUTHOR	C. Guirao
# .VERSION      JLP 24-02-2005
# DEFINITIONS:
########################################################################
include $(JLPSRC)/jlp_make.mk
INC = .
LINC = .
LLIB = 

DEBUG = -g 
C_OPT =
SYS = 
CFLAGS = $(C_OPT) $(DEBUG) $(SYS) -I$(LINC) -I$(INC)

OUT = $(EXEC)/esoext1.exe

OBJ = esoext1.o getline.o linetype.o lidtbl.o f77utl.o \
	extutl.o putline.o

# RULES:
.SUFFIXES: .o .c
.c.o:
	$(CC) $(CFLAGS) -c $<

# DEPENDENCIES:
all: $(OUT)

$(OUT): $(OBJ) 
	$(CC) $(OBJ) $(LLIB) -o $@

esoext1.o putline.o linetype.o lidtbl.o: \
	$(LINC)/esoext.h \
	$(INC)/f77stat.h
getline.o extutl.o f77utl.o: \
	$(LINC)/esoext.h

clear_exec:
	rm -f $(OUT) 

clear:
	-rm -f $(OBJ) 
