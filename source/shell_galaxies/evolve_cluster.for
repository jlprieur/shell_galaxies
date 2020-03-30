C++*********************************************************************
C
C Program  to evolve the initial conditions created with
C the cluster-companion SETUP program.
C
C From	Peter Quinn
C JLP
C Version of 24-07-89
C
C@@ JULY 89: Add the routine
C@@          Put a keplerian potential to test some of Nulsen's predictions
C
C@@ to switch from kepler to isothermal potential,
C@@ change "kepler" by "iso" in calling the routines (see comments with @@)
C*********************************************************************
C
C Warning: you should carefully check the file "audit.dat"
C If the energy is not conserved, take a smaller step
C
C--*********************************************************************
	PROGRAM EVOLVE_CLUSTER
	PARAMETER (max_bodies=20000)
	PARAMETER (G=1.,unit_vel=976.6,unit_mass=2.217e+11)
 
	REAL xp(max_bodies),yp(max_bodies),zp(max_bodies)
	REAL vxp(max_bodies),vyp(max_bodies),vzp(max_bodies)
	REAL accx(max_bodies),accy(max_bodies),accz(max_bodies)
	REAL lum_mass_comp,lum_mass_member
	REAL scale2_clu,total_mass_cluster
	REAL scale2_comph,total_mass_comph
 
	LOGICAL is_member
 
	CHARACTER*2 orbit_type
	CHARACTER*40 params_file,model_file,orbit_file
	CHARACTER*40 audit_file
 
	common /BASICDATA/pi,scale2_clu,atan_scale2_clu,
     &  total_mass_cluster,scale2_comph,atan_scale2_comph,total_mass_comph
	pi=2.*asin(1.)
 
	params_file='params'
	model_file='model'
	orbit_file='orbit'
	audit_file='audit'
 
	OPEN(unit=11,file=model_file,status='unknown',form='unformatted')
	READ(11)nbodies
	READ(11)it,time
	DO i=1,nbodies
	   READ(11)j,xp(i),yp(i),zp(i),vxp(i),vyp(i),vzp(i)
	   ENDDO
 
C@@
C	PRINT *,'evolve>> Version 24-07-89  (Keplerian potential) <<'
	PRINT *,'evolve>> Version 24-07-89  (Isothermal potential) <<'
	PRINT *,' '
	PRINT *,'evolve>> Model file read in <<'
 
C Read PARAMS.DAT
 	OPEN (unit=10,file=params_file,status='unknown')
	READ(10,*)sigma_cluster,core_radius_cluster,
     &            outer_radius_cluster
	READ(10,100)is_member
 100	FORMAT(l1)
	READ(10,*)x_member,y_member,z_member
	READ(10,*)lum_mass_member,re_member
	READ(10,*)nbodies,lum_mass_comp
	READ(10,*)re_comp,gamma_halo,extent
        READ(10,*)r_comp
	READ(10,101)orbit_type
 101	FORMAT(a2)
	READ(10,*)e_fraction,peri_distance
	CLOSE(unit=10)
	PRINT *,'evolve>> Parameters file read in <<'
 
C Normalization :
	sigma_cluster=sigma_cluster/unit_vel
	lum_mass_member=lum_mass_member/unit_mass
	lum_mass_comp=lum_mass_comp/unit_mass
 
C Prepares the common block BASICDATA
C for the routines comp_hallo_acc, and cluster_acc_iso
	scale2_clu=outer_radius_cluster/core_radius_cluster
	atan_scale2_clu=scale2_clu-atan(scale2_clu)
	total_mass_cluster=2.*sigma_cluster*sigma_cluster*
     &                     core_radius_cluster*atan_scale2_clu
	scale2_comph=extent
	outer_radius=extent*gamma_halo
	sigma_halo=pi*G*lum_mass_comp/(32.*re_comp)
	atan_scale2_comph=scale2_comph-atan(scale2_comph)
	total_mass_comph=2.*sigma_halo*sigma_halo*gamma_halo*
     &                 atan_scale2_comph
 
C Parameters of the simulation
	PRINT *,'evolve>> dt (in E+6 years), nsteps',
     1	' and nsteps between output ?'
	READ(5,*)dt,nsteps,nout
 
	PRINT *,' '
	PRINT *,'evolve>---> dt = ',dt
	PRINT *,'evolve>---> nsteps = ',nsteps
	PRINT *,'evolve>---> nout = ',nout
 
C Translation of the model to a position at 45 degrees in the plane xy:
	x_comp=r_comp/sqrt(2.)
	y_comp=r_comp/sqrt(2.)
	z_comp=0.
	DO i=1,nbodies
	   xp(i)=xp(i)+x_comp
	   yp(i)=yp(i)+y_comp
	   ENDDO
 
	PRINT *,'evolve>> Model translated <<'
 
C Setting up the orbit:
	IF(orbit_type.eq.' c')THEN
	   PRINT *,'evolve>> Setting up a circular orbit <<'
C@@
	   CALL cluster_acc_iso(x_comp,y_comp,z_comp,G,
     &                      core_radius_cluster,outer_radius_cluster,
     &                      accx_comp,accy_comp,accz_comp)
	   radial_acc=(x_comp/r_comp)*accx_comp +
     &                (y_comp/r_comp)*accy_comp +
     &                (z_comp/r_comp)*accz_comp
	   v_circ=sqrt(abs(radial_acc)*r_comp)
	   vx_comp=-(y_comp/r_comp)*v_circ
	   vy_comp=(x_comp/r_comp)*v_circ
	   vz_comp=0.
	   v_circ_scaled=v_circ*unit_vel
	   PRINT *,'evolve>> Circular velocity = ',v_circ_scaled
	   ELSEIF(orbit_type.eq.' p')THEN
	      PRINT *,'evolve>> Setting up a parabolic orbit <<'
C@@ works only for isothermal...
	      CALL elements2(r_comp,peri_distance,core_radius_cluster,
     &                       sigma_cluster,x_comp,y_comp,
     &                       vx_comp,vy_comp)
	      ELSEIF(orbit_type.eq.' e')THEN		
	         PRINT *,'evolve>> Setting up an elliptical orbit <<'
	         PRINT *,'evolve>> Energy fraction = ',e_fraction
C@@
                 CALL cluster_acc_iso(x_comp,y_comp,z_comp,G,
     &                    core_radius_cluster,outer_radius_cluster,
     &                    accx_comp,accy_comp,accz_comp)
	         radial_acc=(x_comp/r_comp)*accx_comp +
     &                     (y_comp/r_comp)*accy_comp +
     &                     (z_comp/r_comp)*accz_comp
	         v_circ=sqrt(e_fraction*abs(radial_acc)*r_comp)
                 vx_comp=-(y_comp/r_comp)*v_circ
	         vy_comp=(x_comp/r_comp)*v_circ
	         vz_comp=0.
	         v_circ_scaled=v_circ*unit_vel
		 PRINT *,'evolve>> Initial tangential velocity = ',
     &                          v_circ_scaled
                          ENDIF
	PRINT *,'evolve>> Initial conditions',
     &  ' established for the orbit <<'
 
C Initial velocities:
	DO i=1,nbodies
	   vxp(i)=vxp(i)+vx_comp
	   vyp(i)=vyp(i)+vy_comp
	   ENDDO
 
C Initial accelerations:
C@@
	CALL cluster_acc_iso(x_comp,y_comp,z_comp,G,
     &                   core_radius_cluster,outer_radius_cluster,
     &                   accx_comp,accy_comp,accz_comp)
 
	DO i=1,nbodies
c           CALL comp_halo_acc(xp(i),yp(i),zp(i),x_comp,y_comp,z_comp,
c     &                     G,lum_mass_comp,re_comp,extent,
c     &                     gamma_halo,accx(i),accy(i),accz(i))
           CALL comp_acc(xp(i),yp(i),zp(i),x_comp,y_comp,z_comp,
     &                     G,lum_mass_comp,
     &                     re_comp,accx(i),accy(i),accz(i))
C@@
           CALL cluster_acc_iso(xp(i),yp(i),zp(i),G,
     &                      core_radius_cluster,outer_radius_cluster,
     &                      accx_cluster,accy_cluster,accz_cluster)
 
	   accx(i)=accx(i)+accx_cluster
	   accy(i)=accy(i)+accy_cluster
	   accz(i)=accz(i)+accz_cluster
 
	   ENDDO
	
	   PRINT *,'evolve>> Initial accelerations computed <<'
 
C Storing the initial conditions in MODEL.DAT
	    it00=0
            WRITE(11)it00,time
                DO i=1,nbodies
	           WRITE(11)i,xp(i),yp(i),zp(i),vxp(i),vyp(i),vzp(i)
	        ENDDO
 
C First half step :
	CALL forward_half_step_v(nbodies,vx_comp,vy_comp,vz_comp,
     &                vxp,vyp,vzp,accx,accy,accz,accx_comp,accy_comp,
     &                accz_comp,dt)
 
	PRINT *,'evolve>> Half step complete <<'
	PRINT *,'evolve>> Initializing orbit and audit file <<'
	ncount=1
	OPEN(unit=13,file=orbit_file,status='unknown',form='unformatted')
	WRITE(13)it,x_comp,y_comp,z_comp
	OPEN(unit=14,file=audit_file,status='unknown')
C@@
	CALL audit_iso(G,x_comp,y_comp,z_comp,vx_comp,vy_comp,
     &             vz_comp,core_radius_cluster,
     &             outer_radius_cluster,e_total,am_total)
	WRITE(14,*)it,time,am_total,e_total
	PRINT *,' '
	PRINT *,'evolve> Initial values, Energy : ',
     1	e_total,' AM : ',am_total
	PRINT *,'evolve>> Beginning evolution <<'
	PRINT *,' '
 
 
C-------------- Main loop :
C--
	idelta_steps_out=max(1,nsteps/20)
 
	DO it=1,nsteps
 
	   IF(it.ne.1)THEN
C@@
             CALL cluster_acc_iso(x_comp,y_comp,z_comp,G,
     &                core_radius_cluster,outer_radius_cluster,
     &                accx_comp,accy_comp,accz_comp)
	      DO i=1,nbodies
C@@
                 CALL cluster_acc_iso(xp(i),yp(i),zp(i),G,
     &                  core_radius_cluster,outer_radius_cluster,
     &                  accx(i),accy(i),accz(i))
C@@ Suppress the acceleration due to the self-gravity
C@@ since the companion is disrupted at the initial point:
c                 CALL comp_acc_halo(xp(i),yp(i),zp(i),x_comp,
c     &                           y_comp,z_comp,G,lum_mass_comp,
c     &                           re_comp,extent,gamma_halo,
c     &                           accx_comp,accy_comp,accz_comp)
c	         accx(i)=accx(i)+accx_comp
c	         accy(i)=accy(i)+accy_comp
c	         accz(i)=accz(i)+accz_comp
	         ENDDO
	      ENDIF 	
 
C Leap frog integration :
           CALL leap_frog(nbodies,x_comp,y_comp,z_comp,
     &                    vx_comp,vy_comp,vz_comp,accx_comp,
     &                    accy_comp,accz_comp,dt,time,
     &                    xp,yp,zp,vxp,vyp,vzp,
     &                    accx,accy,accz)
 
	     WRITE(13)it,x_comp,y_comp,z_comp 	   	
 
	     IF(mod(it,idelta_steps_out).eq.0)
     1	   PRINT *,'evolve>> Step : ',it,' Time : ',
     1	time,' completed'
 
C---------- Output of the data :
           IF(ncount.eq.nout)THEN
	      CALL back_half_step_v(nbodies,vx_comp,vy_comp,vz_comp,
     &                     vxp,vyp,vzp,accx,accy,accz,accx_comp,
     &                     accy_comp,accz_comp,dt)
	
	      PRINT *,'evolve>> Step : ',it,' Time : ',time
	      PRINT *,'evolve>> Doing Audit <<'
C@@
	      CALL audit_iso(G,x_comp,y_comp,z_comp,vx_comp,vy_comp,
     &                   vz_comp,core_radius_cluster,
     &                   outer_radius_cluster,e_total,am_total)
	      PRINT *,'evolve>---> Energy : ',e_total,' AM : ',am_total
	      WRITE(14,*)it,time,am_total,e_total
 
              WRITE(11)it,time
                DO i=1,nbodies
	           WRITE(11)i,xp(i),yp(i),zp(i),vxp(i),vyp(i),vzp(i)
	        ENDDO
 
              CALL forward_half_step_v(nbodies,vx_comp,
     &                     vy_comp,vz_comp,vxp,vyp,vzp,
     &                     accx,accy,accz,accx_comp,accy_comp,
     &                     accz_comp,dt)
	      ncount=1
	      ELSE
	      ncount=ncount+1
	      ENDIF
 
	   ENDDO
 
	CLOSE(unit=11)
	CLOSE(unit=13)
	CLOSE(unit=14)
	PRINT *,'evolve>> Evolution complete <<'
	END
 
C*****************************************************************
C
C	Subroutine to compute orbital elements for
C	parabolic orbits in a modified halo field.
C
C*****************************************************************
 
	SUBROUTINE elements2(radius,peri_distance,gamma,sigma_v,
     &                       x1,y1,vx1,vy1)
 
	REAL m1,m2
 
	va=sqrt(2.)*sigma_v
	m1=0.
	p1=peri_distance
 
	m2=va*va*(radius-gamma*atan(radius/gamma))
	r1=radius/(1.+(m1/m2))
	x1=r1-p1
	y1=sqrt(r1*r1-x1*x1)
	vy1=-(m2/(m1+m2))*sqrt((2./r1)*m2/((y1*y1/(p1*p1))+1.))
	vx1=vy1*y1/p1
	
	END
 
 
C****************************************************************************
C Subroutine to compute the acceleration due to the cluster
C Isothermal model.
C****************************************************************************
 
	SUBROUTINE cluster_acc_iso(x,y,z,G,core_radius_cluster,
     &                         outer_radius_cluster,accx,accy,accz)
 
	PARAMETER (epsilon=0.01)
	REAL scale2_clu,total_mass_cluster
	REAL scale2_comph,total_mass_comph
 
	common /BASICDATA/pi,scale2_clu,atan_scale2_clu,
     &  total_mass_cluster,scale2_comph,atan_scale2_comph,total_mass_comph
 
	radius=sqrt(x*x+y*y+z*z)
	IF(radius.gt.outer_radius_cluster)THEN
	   contained_mass=total_mass_cluster
	   ELSE
	   scale=radius/core_radius_cluster
	   contained_mass=total_mass_cluster *
     &                 (scale-atan(scale))/atan_scale2_clu
	   ENDIF
	radial_acc=-G*contained_mass/(radius*radius+epsilon*epsilon)
	accx=radial_acc*x/radius
	accy=radial_acc*y/radius
	accz=radial_acc*z/radius
	RETURN
	END
C****************************************************************************
C	Subroutine to compute the acceleration due to the cluster
C @@ JULY 89 : Keplerian orbit
C****************************************************************************
 
	SUBROUTINE cluster_acc_kepler(x,y,z,G,core_radius_cluster,
     &                         outer_radius_cluster,accx,accy,accz)
	PARAMETER (epsilon=0.01)
	REAL scale2_clu,total_mass_cluster
	REAL scale2_comph,total_mass_comph
 
	common /BASICDATA/pi,scale2_clu,atan_scale2_clu,
     &  total_mass_cluster,scale2_comph,atan_scale2_comph,total_mass_comph
 
	radius=sqrt(x*x+y*y+z*z)
	radial_acc=-G*total_mass_cluster/(radius*radius+epsilon*epsilon)
	accx=radial_acc*x/radius
	accy=radial_acc*y/radius
	accz=radial_acc*z/radius
	RETURN
	END
 
C*****************************************************************************
C Subroutine to compute the acceleration
C due to the self-gravity of the companion with a dark halo
C*****************************************************************************
 
	SUBROUTINE comp_halo_acc(x,y,z,x_comp,y_comp,z_comp,
     &                        G,lum_mass_comp,
     &                        re_comp,extent,
     &                        gamma_halo,accx,accy,accz)
 
	PARAMETER (epsilon=0.01)
	REAL lum_mass_comp
	REAL scale2_clu,total_mass_cluster
	REAL scale2_comph,total_mass_comph
 
	common /BASICDATA/pi,scale2_clu,atan_scale2_clu,
     &  total_mass_cluster,scale2_comph,atan_scale2_comph,total_mass_comph
 
	xx=x-x_comp
	yy=y-y_comp
	zz=z-z_comp
 
	radius=sqrt(xx*xx+yy*yy+zz*zz)
 
	IF(radius.gt.outer_radius)THEN
	   contained_mass_halo=total_mass_comph
	   ELSE
	   scale=radius/gamma_halo
	   outer_radius=extent*gamma_halo
	   contained_mass_halo=total_mass_comph *
     &                 (scale-atan(scale))/atan_scale2_comph
	
	   ENDIF
 
	radial_acc_plummer=-G*lum_mass_comp*radius/(
     &                      radius*radius+re_comp*re_comp)**1.5
        radial_acc_halo=-G*contained_mass_halo/
     &                      (radius*radius+epsilon*epsilon)
	total_radial_acc=radial_acc_plummer+radial_acc_halo
 
	accx=total_radial_acc*xx/radius
	accy=total_radial_acc*yy/radius
	accz=total_radial_acc*zz/radius
 
	RETURN
 
	END
C*****************************************************************************
C Subroutine to compute the acceleration
C due to the self-gravity of the companion without a dark halo
C*****************************************************************************
 
	SUBROUTINE comp_acc(x,y,z,x_comp,y_comp,z_comp,
     &                        G,lum_mass_comp,
     &                        re_comp,accx,accy,accz)
 
	PARAMETER (epsilon=0.01)
	REAL lum_mass_comp
 
	xx=x-x_comp
	yy=y-y_comp
	zz=z-z_comp
 
	radius=sqrt(xx*xx+yy*yy+zz*zz)
 
	total_radial_acc=-G*lum_mass_comp*radius/(
     &                      radius*radius+re_comp*re_comp)**1.5
	
	accx=total_radial_acc*xx/radius
	accy=total_radial_acc*yy/radius
	accz=total_radial_acc*zz/radius
 
	RETURN
 
	END
 
C******************************************************************************
 
C	Subroutine to advance the velocities by half a time step
 
C******************************************************************************
 
 
	SUBROUTINE forward_half_step_v(nbodies,vx_comp,vy_comp,vz_comp,
     &                                 vxp,vyp,vzp,
     &                                 accx,accy,accz,accx_comp,accy_comp,
     &                                 accz_comp,dt)
 
	DIMENSION vxp(1),vyp(1),vzp(1)
	DIMENSION accx(1),accy(1),accz(1)
 
	vx_comp=vx_comp+accx_comp*dt/2.
	vy_comp=vy_comp+accy_comp*dt/2.
	vz_comp=vz_comp+accz_comp*dt/2.
 
	DO i=1,nbodies
	   vxp(i)=vxp(i)+accx(i)*dt/2.
	   vyp(i)=vyp(i)+accy(i)*dt/2.
	   vzp(i)=vzp(i)+accz(i)*dt/2.
	   ENDDO
 
	RETURN
 
	END	
 
 
C****************************************************************************
 
C	Subroutine to advance the positions and velocities one time step
 
C***************************************************************************
 
 
	SUBROUTINE leap_frog(nbodies,x_comp,y_comp,z_comp,
     &                       vx_comp,
     &                       vy_comp,vz_comp,accx_comp,
     &                       accy_comp,accz_comp,dt,time,
     &                       xp,yp,zp,vxp,vyp,vzp,
     &                       accx,accy,accz)
 
	DIMENSION xp(1),yp(1),zp(1)
	DIMENSION vxp(1),vyp(1),vzp(1)
	DIMENSION accx(1),accy(1),accz(1)
 
	vx_comp=vx_comp+dt*accx_comp
	vy_comp=vy_comp+dt*accy_comp
	vz_comp=vz_comp+dt*accz_comp
	
	x_comp=x_comp+dt*vx_comp
	y_comp=y_comp+dt*vy_comp
	z_comp=z_comp+dt*vz_comp
 
C	print *,x_comp,y_comp,v_comp
C	print *,vx_comp,vy_comp,vz_comp
C	print *,accx_comp,accy_comp,accz_comp
	DO i=1,nbodies	
	   IF(i.eq.10)THEN
	       xx=xp(i)-x_comp
	       yy=yp(i)-y_comp
	       zz=zp(i)-z_comp
	       vx=vxp(i)-vx_comp
	       vy=vyp(i)-vy_comp
	       vz=vzp(i)-vz_comp
	       rad=sqrt(xx*xx+yy*yy+zz*zz)
	       radv=(xx*vx/rad)+(yy*vy/rad)+(zz*vz/rad)	
	       radacc=(xx*accx(i)/rad)+(yy*accy(i)/rad)+
     &	              (zz*accz(i)/rad)
C	       print *,i,rad,radv,radacc
C	       print *,i,accx(i),accy(i),accz(i)
C	       print *,i,vx,vy,vz
C	       print *,i,xx,yy,zz
C	   print *,i,vxp(i),accx(i),dt,vx_comp
	       ENDIF
	   vxp(i)=vxp(i)+dt*accx(i)
	   vyp(i)=vyp(i)+dt*accy(i)
	   vzp(i)=vzp(i)+dt*accz(i)
	   xp(i)=xp(i)+dt*vxp(i)
	   yp(i)=yp(i)+dt*vyp(i)
	   zp(i)=zp(i)+dt*vzp(i)
	   ENDDO
 
	time=time+dt
	RETURN
	END
 
 
C******************************************************************************
 
C	Subroutine to backup the velocities by half a time step
 
C******************************************************************************
 
 
	SUBROUTINE back_half_step_v(nbodies,vx_comp,vy_comp,vz_comp,
     &                                 vxp,vyp,vzp,
     &                                 accx,accy,accz,accx_comp,accy_comp,
     &                                 accz_comp,dt)
 
	DIMENSION vxp(1),vyp(1),vzp(1)
	DIMENSION accx(1),accy(1),accz(1)
 
	vx_comp=vx_comp-accx_comp*dt/2.
	vy_comp=vy_comp-accy_comp*dt/2.
	vz_comp=vz_comp-accz_comp*dt/2.
 
	DO i=1,nbodies
	   vxp(i)=vxp(i)-accx(i)*dt/2.
	   vyp(i)=vyp(i)-accy(i)*dt/2.
	   vzp(i)=vzp(i)-accz(i)*dt/2.
	   ENDDO
 
	RETURN
	END	
 
C**************************************************************************
C	Subroutine to check the energy and angular momentum of the
C	companion orbit in the cluster field.
C Isothermal model
C**************************************************************************
 
	SUBROUTINE audit_iso(G,x_comp,y_comp,z_comp,vx_comp,vy_comp,
     &                   vz_comp,core_radius_cluster,
     &                   outer_radius_cluster,e_total,am_total)
	REAL scale2_clu,total_mass_cluster
	REAL scale2_comph,total_mass_comph
 
	common /BASICDATA/pi,scale2_clu,atan_scale2_clu,
     &  total_mass_cluster,scale2_comph,atan_scale2_comph,total_mass_comph
 
	amx=y_comp*vz_comp-z_comp*vy_comp
	amy=z_comp*vx_comp-x_comp*vz_comp
	amz=x_comp*vy_comp-y_comp*vx_comp
 
	am_total=sqrt(amx*amx+amy*amy+amz*amz)
 
	radius=sqrt(x_comp*x_comp+y_comp*y_comp+z_comp*z_comp)
	
	IF(radius.lt.outer_radius_cluster)THEN
	   scale=radius/core_radius_cluster
	   t1=(1./scale)*atan(scale)-1.
	   t2=0.5*alog((1.+scale*scale)/(1.+scale2_clu*scale2_clu))
           t3=total_mass_cluster/
     &            (core_radius_cluster*atan_scale2_clu)
           cluster_potential=t3*(t1+t2)
	   ELSE
	   cluster_potential=-G*total_mass_cluster/radius
	   ENDIF
	energy_kinetic=0.5*(vx_comp*vx_comp+vy_comp*vy_comp+
     &                      vz_comp*vz_comp)
 
	e_total=cluster_potential+energy_kinetic
 
	RETURN
	END
C**************************************************************************
C	Subroutine to check the energy and angular momentum of the
C	companion orbit in the cluster field.
C Keplerian model
C**************************************************************************
 
	SUBROUTINE audit_kepler(G,x_comp,y_comp,z_comp,vx_comp,vy_comp,
     &                   vz_comp,core_radius_cluster,
     &                   outer_radius_cluster,e_total,am_total)
	REAL scale2_clu,total_mass_cluster
	REAL scale2_comph,total_mass_comph
 
	common /BASICDATA/pi,scale2_clu,atan_scale2_clu,
     &  total_mass_cluster,scale2_comph,atan_scale2_comph,total_mass_comph
 
	amx=y_comp*vz_comp-z_comp*vy_comp
	amy=z_comp*vx_comp-x_comp*vz_comp
	amz=x_comp*vy_comp-y_comp*vx_comp
 
	am_total=sqrt(amx*amx+amy*amy+amz*amz)
 
	radius=sqrt(x_comp*x_comp+y_comp*y_comp+z_comp*z_comp)
	
	cluster_potential=-G*total_mass_cluster/radius
	energy_kinetic=0.5*(vx_comp*vx_comp+vy_comp*vy_comp+
     &                      vz_comp*vz_comp)
 
	e_total=cluster_potential+energy_kinetic
 
	RETURN
	END
