midir=$(JLPLIB)/midas
mylib=$(JLPLIB)/jlp
smgolib=$(JLPLIB)/smgo
LIB=$(mylib)/newplot0.a $(mylib)/jlpacc.a $(mylib)/jlputil.a $(mylib)/jlpacc.a 
#SMGO=$(smgolib)/plotsub.a $(smgolib)/devices.a 
SMGO=
#XLIB= -lXaw -lXmu -lXt -lX11
XLIB=
F77LIB=/usr/lib/libF77.a /usr/lib/libI77.a /usr/lib/libU77.a 
MIDLIB=$(midir)/ftoclib.a $(midir)/tbllib.a $(midir)/stlib.a $(midir)/mathlib.a $(midir)/oslib.a
F77=f77
ESOEXT=/midas/frozen/exec/esoext.exe
INC=.
FFLAGS=-O 
#FFLAGS=-g -C 

.SUFFIXES:
.SUFFIXES: .o .for .exe $(SUFFIXES) 

.for.o:
	$(EXEC)/esoext1.exe -f $*.for
	f77 -c $(FFLAGS) $*.f
	rm $*.f

.for.exe:
	$(EXEC)/esoext1.exe -f $*.for
	f77 -c $(FFLAGS) $*.f
	rm $*.f
	f77 $(FFLAGS) -o $(EXEC)/$*.exe $*.o $(LIB) $(MIDLIB) $(SMGO) $(XLIB)
	rm $*.o

.o.exe:
	f77 $(FFLAGS) -o $(EXEC)/$*.exe $*.o $(LIB) $(MIDLIB) $(SMGO) $(XLIB)
	rm $*.o

all: $*.exe
$*.exe : $*.o $*.for
