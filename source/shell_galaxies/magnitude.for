{
C--**********************************************************************
C Program MAGNITUDE(input,output);
C
C  This program calculates the magnitudes of shells and sections of
C   Elliptical Galaxies.    It was written specifically for my Elliptical
C    Galaxy project.
C Steve Meatheringham (Stromlo)
C Language: Pascal
C Version of 01-01-86
C--**********************************************************************
}
 
Program MAGNITUDE(input,output);
 
{        This program calculates the magnitudes of shells and sections of
    Elliptical Galaxies.    It was written specifically for my Elliptical
    Galaxy project.     }
 
 
Type gal_type = (NGC2865,NGC5018,NGC3923,NGC3923N);
    filt_type = (v,rl,ro,b2);
 
Var         gal : gal_type;
           filt : filt_type;
mag,col_const,k : array [filt_type] of double;
av_bck,expos,am : array [gal_type,filt_type] of double;
vm,rlm,rom,b2m,
n,integral      : double;
 
Function Mth$Dlog10(x : double) : double ; extern;
 
Begin
  k[v]  := 0.1;
  k[rl] := 0.045;
  k[ro] := 0.055;
  k[b2] := 0.15;
 
  am[NGC2865,v]  := 0.84;
  am[NGC2865,rl] := 0.82;
  am[NGC2865,ro] := 0.82;
  am[NGC2865,b2] := 0.82;
  am[NGC5018,v]  := 0.78;
  am[NGC5018,rl] := 0.78;
  am[NGC5018,ro] := 0.78;
  am[NGC5018,b2] := 0.78;
  am[NGC3923,v]  := 0.91;
  am[NGC3923,rl] := 0.91;
  am[NGC3923,ro] := 0.91;
  am[NGC3923,b2] := 0.91;
  am[NGC3923N,v]  := 0.91;
  am[NGC3923N,rl] := 0.91;
  am[NGC3923N,ro] := 0.91;
  am[NGC3923N,b2] := 0.91;
 
  expos[NGC2865,v]  := 1200;
  expos[NGC2865,rl] := 1800;
  expos[NGC2865,ro] := 300;
  expos[NGC2865,b2] := 1200;
  expos[NGC5018,v]  := 1200;
  expos[NGC5018,rl] := 1200;
  expos[NGC5018,ro] := 300;
  expos[NGC5018,b2] := 1200;
  expos[NGC3923,v]  := 300;
  expos[NGC3923,rl] := 600;
  expos[NGC3923,ro] := 120;
  expos[NGC3923,b2] := 600;
  expos[NGC3923N,v]  := 1200;
  expos[NGC3923N,rl] := 1800;
  expos[NGC3923N,ro] := 300;
  expos[NGC3923N,b2] := 1800;
 
  av_bck[NGC2865,v]  := 241.02;
  av_bck[NGC2865,rl] := 1420.24;
  av_bck[NGC2865,ro] := 543.69;
  av_bck[NGC2865,b2] := 360.77;
  av_bck[NGC5018,v]  := 268.72;
  av_bck[NGC5018,rl] := 544.48;
  av_bck[NGC5018,ro] := 460.04;
  av_bck[NGC5018,b2] := 279.50;
  av_bck[NGC3923,v]  := 68.23;
  av_bck[NGC3923,rl] := 354.77;
  av_bck[NGC3923,ro] := 232.51;
  av_bck[NGC3923,b2] := 209.76;
  av_bck[NGC3923N,v]  := 258.92;
  av_bck[NGC3923N,rl] := 996.95;
  av_bck[NGC3923N,ro] := 557.87;
  av_bck[NGC3923N,b2] := 545.33;
 
  col_const[v]  := 22.746;
  col_const[rl] := 21.460;
  col_const[ro] := 23.989;
  col_const[b2] := 23.467;
 
  write('Galaxy & Filter : ');
  readln(gal,filt);
  writeln;
 
  repeat
    write('Integral : ');
    readln(integral);
    writeln;
 
    if integral > 0 then
      Begin
 
        n := exp(2.302585093 * k[filt] * am[gal,filt] / 2.5);
        n := n * av_bck[gal,filt] * integral * 10 / expos[gal,filt];
 
        mag[filt] := -2.5 * mth$dlog10(n) + col_const[filt];
 
        writeln(filt:2,' mag. = ',mag[filt]:6:2);
 
      End;
 
  until integral<0;
 
End.
