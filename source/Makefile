##############################################################################
# Makefile for SOURCE
# to generate executable version of JLP fortran programs
# Uses jlp_make.mk
# JLP
# Version 13/02/2010
##############################################################################
include $(JLPSRC)/jlp_make.mk
WX_LIB := `wx-config --libs base,core`

.SUFFIXES:
.SUFFIXES:  .o .for .exe $(SUFFIXES) 

.for.exe:
	$(EXEC)/esoext1.exe -I ../jlpsub -f $*.for
	$(F77) -c $(FFLAGS) $*.f
	$(F77) $(FFLAGS) -o $(EXEC)/$*.exe $*.o \
	$(JLIB) $(FITSLIB) $(MATHLIB) $(XLIB) $(WX_LIB) $(LIBC) -lz
	rm $*.o

# For debug (in Merate)
#.for.exe:
#	$(EXEC)/esoext1.exe -I ../jlpsub -f $*.for
#	$(F77) -c $(FFLAGS) $*.f
#	$(F77) $(FFLAGS) -o $(EXEC)/$*.exe $*.o \
#	$(MATHLIB) $(JLIB_NOGRAPHIC) $(FITSLIB) $(XLIB) $(LIBC)
#	rm $*.o

#.o.exe:
#	$(F77) $(FFLAGS) -o $(EXEC)/$*.exe $*.o \
#	$(JLIB) $(MATHLIB) $(FITSLIB) $(XLIB) $(LIBC)
#	rm $*.o

all: $*.exe
$*.exe : $*.for
#$*.exe : $*.o $*.for
