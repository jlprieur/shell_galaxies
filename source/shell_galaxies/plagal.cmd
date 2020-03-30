$!*********************************************************************
$! Command procedure PLAGAL.COM to remove the galaxy from an image.
$! Version of 09/01/89
$!
$! SYNTAX :        $@PLAGAL
$!*********************************************************************
$JLP_FORMAT="2,2"
$! Name of the input image
$! Name of the output image
$ RUNSTAR PLAGAL
N1210_BL
N1210_BL_REM
1                                ! Option 1 (smoothed profile)
0.492                            ! Scale :
50.0                             ! Position angle :
190.5,212.2                      ! Position of the center 
N                                ! Fixed ellipticity? No :
33.,1.3                          ! Mean radius and ellipticity
10.,2.6                          ! Mean radius and ellipticity
2.7,4.5                          ! Mean radius and ellipticity
0.,0.                            ! Fin and ! Input profile 
N1210_BL.PRO
Y                                ! Yes: profile1 format 
5.,100.                          ! Min and max radii for the fit 
5                                ! Order of the polynomial 
$ EXIT
