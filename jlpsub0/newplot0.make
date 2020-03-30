#####################################################################
# Makefile to create and update newplot0.a
# JLP
# Version 03-10-2014
#####################################################################
include $(JLPSRC)/jlp_make.mk
DESTIN=$(JLPLIB)/jlp
JLPSUB=../jlpsub
OBJ = newplot0_smgo.o newplot0_set1.o newplot0_set2.o str.o \
	display1.o display2.o

.SUFFIXES:
.SUFFIXES: .o .f .for  $(SUFFIXES)

.for.f:
	$(EXEC)/esoext1.exe -I $(JLPSUB) -f $*.for
	
.f.o:
	$(F77) -c $(FFLAGS) $*.f
	rm $*.f
	ar r $(DESTIN)/newplot0.a $*.o 
	
.for.o:
	$(EXEC)/esoext1.exe -I $(JLPSUB) -f $*.for
	$(F77) -c $(FFLAGS) $*.f
	rm $*.f
	ar r $(DESTIN)/newplot0.a $*.o 
	
all: $(OBJ)
	ranlib $(DESTIN)/newplot0.a

display1.o : $(JLPSUB)/display1.for
	$(EXEC)/esoext1.exe -f $(JLPSUB)/display1.for
	mv $(JLPSUB)/display1.f .
	$(F77) -c $(FFLAGS) display1.f
	rm display1.f
	ar r $(DESTIN)/newplot0.a display1.o

display2.o : $(JLPSUB)/display2.for
	$(EXEC)/esoext1.exe -f $(JLPSUB)/display2.for
	mv $(JLPSUB)/display2.f .
	$(F77) -c $(FFLAGS) display2.f
	rm display2.f
	ar r $(DESTIN)/newplot0.a display2.o

newplot0_set1.o : newplot0_set1.for 

newplot0_set2.o : newplot0_set2.for 

newplot0_smgo.o : newplot0_smgo.for 

str.o : $(JLPSUB)/str.for
	cp $(JLPSUB)/str.for .
	$(EXEC)/esoext1.exe -I ../jlpsub -f str.for
	$(F77) -c $(FFLAGS) str.f
	rm -f str.f str.for 
	ar r $(DESTIN)/newplot0.a str.o 

clear:
	rm -f $(OBJ) 
	rm $(DESTIN)/newplot0.a
