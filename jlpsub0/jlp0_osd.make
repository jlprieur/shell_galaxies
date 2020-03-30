#################################################################################
# jlp0_osd.make
# Makefile for programs using osd library (from F. Ochsembein)
# JLP
# Version 14-11-90
#################################################################################
midir=$(JLPLIB)/midas
LIB=$(JLPLIB)/jlp/jlpacc.a 
#LIB=$(JLPLIB)/jlp/newplot0.a $(JLPLIB)/jlp/jlpacc.a $(JLPLIB)/jlp/oslib.a 
#SMGO=$(JLPLIB)/smgo/plotsub.a $(JLPLIB)/smgo/devices.a 
SMGO=
#XLIB= -lXaw -lXmu -lXt -lX11
XLIB=
F77LIB=/usr/lib/libF77.a /usr/lib/libI77.a /usr/lib/libU77.a 
MIDLIB=$(midir)/ftoclib.a $(midir)/stlib.a $(midir)/oslib.a
#NAGLIB=$(JLPLIB)/midas/mathlib.a
NAGLIB=
#ESOEXT=/midas/frozen/exec/esoext.exe
INC=.
CCLAGS=-c -g -C 
# cc -g -o $(EXEC)/$*.exe $*.o $(LIB) $(MIDLIB) $(SMGO) $(NAGLIB) $(XLIB) $(F77LIB) -lm

.SUFFIXES:
.SUFFIXES: .o .c .exe $(SUFFIXES) 

.c.o:
	cc $(CCLAGS) $*.c

.o.exe:
	cc -g -o $(EXEC)/$*.exe $*.o $(LIB) $(MIDLIB) $(SMGO) $(NAGLIB) $(XLIB) $(F77LIB) -lm

.c.exe:
	cc $(CCLAGS) $*.c
	cc -g -o $(EXEC)/$*.exe $*.o $(LIB) $(MIDLIB) $(SMGO) $(NAGLIB) $(XLIB) $(F77LIB) -lm

all: jlp0_osd.exe

jlp0_osd.exe: jlp0_osd.o jlp0_osd.c 
