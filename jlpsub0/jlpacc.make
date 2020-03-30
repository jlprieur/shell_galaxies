############################################################################
# Makefile to create and update jlpacc.a
# JLP
# Version 06-10-2014
############################################################################
include $(JLPSRC)/jlp_make.mk
MY_DEPENDENCIES=../jlp_incl/jlp_ftoc_proto.h
DESTIN=$(JLPLIB)/jlp
JLPSUB=../jlpsub
# JLP97: I replace jlpsub/jlp_access.for 
# by jlpsub0/jlp0_accc.c and jlpsub/jlp_access2.for:
OBJ=jlp0_accc.o jlp0_begin_c.o jlp_access2.o jlp_begin.o \
	jlp0_vax1.o jlp0_ac_vax.o jlp0_rdtable.o jlp_directory.o \
	jlp0_unix1.o jlp_system.o jlp_string.o \
	jlp_tiff.o readfile1.o readfile2.o eric_format.o
################################################################

.SUFFIXES:
.SUFFIXES: .o .c .for  $(SUFFIXES)

.c.o:
	cc -c $(CFLAGS) $*.c
	ar r $(DESTIN)/jlpacc.a $*.o 

.for.o:
	$(EXEC)/esoext1.exe -I $(JLPSUB) -f $*.for
	$(F77) -c $(FFLAGS) $*.f
	rm $*.f
	ar r $(DESTIN)/jlpacc.a $*.o 
	
all: $(OBJ)
	ranlib $(DESTIN)/jlpacc.a

jlp_access.o : $(JLPSUB)/jlp_access.for 
	$(EXEC)/esoext1.exe -f $(JLPSUB)/jlp_access.for
	mv $(JLPSUB)/jlp_access.f .
	$(F77) -c $(FFLAGS) jlp_access.f
	rm jlp_access.f
	ar r $(DESTIN)/jlpacc.a jlp_access.o 

# New routine to replace jlp_access.o
# should be used with jlp_accc.c
jlp_access2.o : $(JLPSUB)/jlp_access2.for 
	$(EXEC)/esoext1.exe -f $(JLPSUB)/jlp_access2.for
	mv $(JLPSUB)/jlp_access2.f .
	$(F77) -c $(FFLAGS) jlp_access2.f
	rm jlp_access2.f
	ar r $(DESTIN)/jlpacc.a jlp_access2.o 

jlp_begin.o : $(JLPSUB)/jlp_begin.for 
	$(EXEC)/esoext1.exe -f $(JLPSUB)/jlp_begin.for
	mv $(JLPSUB)/jlp_begin.f .
	$(F77) -c $(FFLAGS) jlp_begin.f
	rm jlp_begin.f
	ar r $(DESTIN)/jlpacc.a jlp_begin.o 

jlp0_vax1.o : $(JLPSUB)/jlp0_vax1.for 
	$(EXEC)/esoext1.exe -f $(JLPSUB)/jlp0_vax1.for
	mv $(JLPSUB)/jlp0_vax1.f .
	$(F77) -c $(FFLAGS) jlp0_vax1.f
	rm jlp0_vax1.f
	ar r $(DESTIN)/jlpacc.a jlp0_vax1.o 

readfile1.o: $(JLPSUB)/readfile1.for
	$(EXEC)/esoext1.exe -f $(JLPSUB)/readfile1.for
	mv $(JLPSUB)/readfile1.f .
	$(F77) -c $(FFLAGS) readfile1.f
	rm readfile1.f
	ar r $(DESTIN)/jlpacc.a readfile1.o

readfile2.o: $(JLPSUB)/readfile2.for
	$(EXEC)/esoext1.exe -f $(JLPSUB)/readfile2.for
	mv $(JLPSUB)/readfile2.f .
	$(F77) -c $(FFLAGS) readfile2.f
	rm readfile2.f
	ar r $(DESTIN)/jlpacc.a readfile2.o

# New routine to replace jlp_access.o
# should be used with jlp_access2.for
jlp0_accc.o : jlp0_accc.c $(MY_DEPENDENCIES) 

jlp0_begin_c.o : jlp0_begin_c.c $(MY_DEPENDENCIES) 

jlp0_ac_vax.o : jlp0_ac_vax.for $(MY_DEPENDENCIES) 

jlp0_osd.o : jlp0_osd.c $(MY_DEPENDENCIES) 

jlp0_unix1.o : jlp0_unix1.c $(MY_DEPENDENCIES) 

jlp_system.o : jlp_system.c $(MY_DEPENDENCIES) 

jlp_string.o: jlp_string.c

jlp0_acc_midas.o : jlp0_acc_midas.c $(MY_DEPENDENCIES) 

eric_format.o : eric_format.c $(MY_DEPENDENCIES)

karim_format.o : karim_format.c $(MY_DEPENDENCIES)

jlp0_rdtable.o: jlp0_rdtable.for

jlp_directory.o : $(JLPSUB)/jlp_directory.for
	$(EXEC)/esoext1.exe -f $(JLPSUB)/jlp_directory.for
	mv $(JLPSUB)/jlp_directory.f .
	$(F77) -c $(FFLAGS) jlp_directory.f
	rm jlp_directory.f
	ar r $(DESTIN)/jlpacc.a jlp_directory.o

clean:
clear:
	rm -f $(OBJ)
	rm -f $(DESTIN)/jlpacc.a
