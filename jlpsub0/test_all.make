include $(JLPSRC)/jlp_make.mk
ESOEXT=/midas/frozen/exec/esoext.exe
INCL=-I. -I../jlp_incl
FFLAGS=-O 
CFLAGS=-O 
#FFLAGS=-g -C 
#sed -e sxjlpsub:x/users/prieur/jlpsub/xgP $*.for > $*.vf 

.SUFFIXES:
.SUFFIXES: .for .c .exe $(SUFFIXES) 

.for.exe:
	$(EXEC)/esoext1.exe -f $*.for
	f77 -c $(FFLAGS) $*.f
	rm $*.f
	f77 $(FFLAGS) -o $(EXEC)/$*.exe $*.o $(JLIB) $(MIDLIB) $(XLIB)
	rm $*.o

.c.exe:
	cc -c $(CFLAGS) $(INCL) $*.c
	cc $(CFLAGS) -o $(EXEC)/$*.exe $*.o $(JLIB) $(MIDLIB) $(XLIB)
	rm $*.o

#all: test_osd.exe test_vax.exe 
all: test_time.exe 

test_osd.exe : test_osd.for

test_vax.exe : test_vax.for

test_time.exe : test_time.c
