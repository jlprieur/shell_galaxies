C++--------------------------------------------------------------------
C Format with the help available for the graphics package:
C
C
C When you get an "xterm" graphic window, you also get a "cursor" facility.
C You can click on the left button to get coordinates within the frame.
C When you click outside the frame (but within the xterm graphic window),
C you close the xterm graphic window...
C
C JLP Version 26-01-96
C--------------------------------------------------------------------
 3333 format(/,'Table of the available graphic devices:',/,
     &'- NONE : no graphic output',/,16x,/,
C     &'- With NEWPLOT: $TEKTRO, and $FILE',/,16x,
C     &'(to plot a file on the laser/versatec use PLOTCOM)',/,
C     &'- With MONGO (Manchester): #CIFER_T5, #CANON,',
C     &' #CANON_L, #CANON_P, #ARGS',/,
C     &'- With SUPER MONGO: &term, &postland, &postport,',
C     &' &postscript (squared)',/,
     &'- With SPLOT: &xterm, &xterm_small, &Xterm, &Xterm_small, &hpgl',
     &/,'   and postscript: &landscape, &postscript,',
     &' &squared, &fullpage',/,'(or &landscape/example.ps, ...)',/)
C     &'- With MONGO/87: %AGFA_L, %AGFA_P, %APPLE_L,',
C     &' %APPLE_P, %TERM, %VERSA',/,
C     &'- With PGPLOT: *TXA1:/TEKTRO, *PGPLOT.PLT/VERSATEC,...',/,16x,
C     &'(to plot a file on the versatec use PGVER)',/)
 3334 format(/,'Table of the available symbols:',/,
     &' (followed by a number between 1 and 99, to increase the size)'
     &,/,5x,' Example:  460=big crosses, 420=tiny crosses)',/,5x,
     &' 0 = Histogram-like',/,5x,' 1 = Small dot ',/,5x,
     &' 2 = Open triangle',/,5x,' 3 = Filled triangle ',/,5x,
     &' 4 = Cross + ',/,5x,' 5 = Cross X ',/,5x,' 6 = Open square'
     &,/,5x,' 7 = Filled square',/,5x,' 8 = Open circle',/,5x,
     &' 9 = Filled circle',/,' For lines:    L=Solid line',/,
     &' With MONGO: L0=solid, L1=dotted, L2=dashed,',/,
     &' With PGPLOT: L2=dashed,',/,
     &' L3 dash-dot,  L4 dot-dot, L5 dash-dot-dot-dot',/,/,
     &' Colors available: Black,Gray,Purple,Aquamarine,R_G_B')
