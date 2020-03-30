$!*********************************************************************
$! Command procedure PLAGAL.COM to remove the galaxy from an image.
$! Version of 09/01/89
$!
$! SYNTAX :        $@PLAGAL 
$!*********************************************************************
$!
$! Open the internal command file
$	OPEN/WRITE INPUTA PLAGAL.AAA
$!
$! Name of the input image
$	WRITE INPUTA "N1210_BL.BDF"
$! Name of the output image
$	WRITE INPUTA "N1210_BL_REM.BDF"
$! Option 1 (smoothed profile) :
$	WRITE INPUTA "1"
$! Scale :
$	WRITE INPUTA "0.492"
$! Position angle :
$	WRITE INPUTA "50.0"
$! Position of the center :
$	WRITE INPUTA "190.5,212.2"
$! Fixed ellipticity? No :
$	WRITE INPUTA "N"
$! Mean radius and axis ratio :
$	WRITE INPUTA "3.,0.60"
$	WRITE INPUTA "6.,0.63"
$	WRITE INPUTA "8.,0.68"
$	WRITE INPUTA "10.,0.74"
$	WRITE INPUTA "15.,0.82"
$	WRITE INPUTA "33.,0.86"
$	WRITE INPUTA "0.,0."
$! Input profile:
$	WRITE INPUTA "N1210_BL.PRO"
$! Profile from PROFILE1:
$	WRITE INPUTA "Y"
$! Min and max radii for the fit :
$	WRITE INPUTA "5.,100."
$! Order of the polynomial :
$	WRITE INPUTA "5"
$! Sky level (approx level minus 3 sigma) :
$	WRITE INPUTA "210."
$	CLOSE INPUTA
$! Define the internal command file PLAGAL.AAA as SYS$COMMAND :
$! and then as SYS$INPUT (internal in "RUNSTAR")
$	DEFINE/PROC/SUPER SYS$COMMAND PLAGAL.AAA
$!
$	RUNSTAR EXEC:PLAGAL
$       DEASSIGN SYS$COMMAND
$	DELETE PLAGAL.AAA;*
$ EXIT
