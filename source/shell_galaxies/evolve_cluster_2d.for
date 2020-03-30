C*********************************************************************
C
C Program  to evolve the initial conditions created with
C the cluster-filament SETUP program.
C
C From	Peter Quinn Sept. '87
C*********************************************************************
	PROGRAM EVOLVE_CLUSTER
	PARAMETER (max_bodies=10000)
	PARAMETER (G=1.,unit_vel=976.6,unit_mass=2.217e+11)
 
	REAL xp(max_bodies),yp(max_bodies),zp(max_bodies)
	REAL vxp(max_bodies),vyp(max_bodies),vzp(max_bodies)
	REAL accx(max_bodies),accy(max_bodies),accz(max_bodies)
 
	LOGICAL is_member
 
	REAL lum_mass_filgal,lum_mass_member
 
	CHARACTER*2 orbit_type
	CHARACTER*40 params_file,model_file,orbit_file
	CHARACTER*40 audit_file
 
	params_file='params'
	model_file='model'
	orbit_file='orbit'
	audit_file='audit'
 
	OPEN(unit=11,file=model_file,status='unknown')
	READ(11,*)nbodies
	READ(11,*)it,time
	DO i=1,nbodies
	   READ(11,*)j,xp(i),yp(i),work,vxp(i),vyp(i),work
	   zp(i)=0.
	   vzp(i)=0.
	   ENDDO
 
	PRINT *,'evolve>> Model file read in <<'
 
C Read PARAMS.DAT
 	OPEN (unit=10,file=params_file,status='unknown')
	READ(10,*)sigma_cluster,core_radius_cluster,
     &            outer_radius_cluster
	READ(10,100)is_member
 100	FORMAT(l1)
	READ(10,*)x_member,y_member,z_member
	READ(10,*)lum_mass_member,re_member
	READ(10,*)nbodies,lum_mass_filgal
	READ(10,*)re_filgal,gamma_halo,extent
        READ(10,*)r_filgal
	READ(10,101)orbit_type
 101	FORMAT(a2)
	READ(10,*)e_fraction,peri_distance
	CLOSE(unit=10)
	PRINT *,'evolve>> Parameters file read in <<'
 
C Normalization :
	sigma_cluster=sigma_cluster/unit_vel
	lum_mass_member=lum_mass_member/unit_mass
	lum_mass_filgal=lum_mass_filgal/unit_mass
 
C Parameters of the simulation
	PRINT *,'evolve>> dt (in E+6 years), nsteps',
     1	' and nsteps between output ?'
	READ(5,*)dt,nsteps,nout
 
	PRINT *,' '
	PRINT *,'evolve>---> dt = ',dt
	PRINT *,'evolve>---> nsteps = ',nsteps
	PRINT *,'evolve>---> nout = ',nout
 
C Translation of the model :
	x_filgal=r_filgal/sqrt(2.)
	y_filgal=r_filgal/sqrt(2.)
	z_filgal=0.
	DO i=1,nbodies
	   xp(i)=xp(i)+x_filgal
	   yp(i)=yp(i)+y_filgal
	   ENDDO
 
	PRINT *,'evolve>> Model translated <<'
 
C Setting up the orbit:
	IF(orbit_type.eq.' c')THEN
	   PRINT *,'evolve>> Setting up a circular orbit <<'
	   CALL cluster_acc(x_filgal,y_filgal,z_filgal,G,
     &                      core_radius_cluster,
     &                      outer_radius_cluster,sigma_cluster,
     &                      accx_filgal,
     &                      accy_filgal,accz_filgal)
	   radial_acc=(x_filgal/r_filgal)*accx_filgal +
     &                (y_filgal/r_filgal)*accy_filgal +
     &                (z_filgal/r_filgal)*accz_filgal
	   v_circ=sqrt(abs(radial_acc)*r_filgal)
	   vx_filgal=-(y_filgal/r_filgal)*v_circ
	   vy_filgal=(x_filgal/r_filgal)*v_circ
	   vz_filgal=0.
	   v_circ_scaled=v_circ*unit_vel
	   PRINT *,'evolve>> Circular velocity = ',v_circ_scaled
	   ELSEIF(orbit_type.eq.' p')THEN
	 	  PRINT *,'evolve>> Setting up a parabolic orbit <<'
	          CALL elements2(r_filgal,peri_distance,core_radius_cluster,
     &                           sigma_cluster,x_filgal,y_filgal,
     &                           vx_filgal,vy_filgal)
		  ELSEIF(orbit_type.eq.' e')THEN		
	                 PRINT *,'evolve>> Setting up an elliptical orbit <<'
	                 PRINT *,'evolve>> Energy fraction = ',e_fraction
                         CALL cluster_acc(x_filgal,y_filgal,z_filgal,G,
     &                      core_radius_cluster,
     &                      outer_radius_cluster,sigma_cluster,
     &                      accx_filgal,
     &                      accy_filgal,accz_filgal)
	                  radial_acc=(x_filgal/r_filgal)*accx_filgal +
     &                               (y_filgal/r_filgal)*accy_filgal +
     &                               (z_filgal/r_filgal)*accz_filgal
	                  v_circ=sqrt(e_fraction*abs(radial_acc)*r_filgal)
                          vx_filgal=-(y_filgal/r_filgal)*v_circ
	                  vy_filgal=(x_filgal/r_filgal)*v_circ
	                  vz_filgal=0.
	                  v_circ_scaled=v_circ*unit_vel
			  PRINT *,'evolve>> Initial tangential velocity = ',
     &                                     v_circ_scaled
                          ENDIF
	PRINT *,'evolve>> Orbit established <<'
 
C Initial velocities:
	DO i=1,nbodies
	   vxp(i)=vxp(i)+vx_filgal
	   vyp(i)=vyp(i)+vy_filgal
	   ENDDO
 
C Initial accelerations:
	CALL cluster_acc(x_filgal,y_filgal,z_filgal,G,
     &                   core_radius_cluster,
     &                   outer_radius_cluster,sigma_cluster,accx_filgal,
     &                   accy_filgal,accz_filgal)
 
	DO i=1,nbodies
           CALL filgal_acc(xp(i),yp(i),zp(i),x_filgal,y_filgal,z_filgal,
     &                     G,lum_mass_filgal,
     &                     re_filgal,extent,
     &                     gamma_halo,accx(i),accy(i),accz(i))
           CALL cluster_acc(xp(i),yp(i),zp(i),G,
     &                      core_radius_cluster,
     &                      outer_radius_cluster,sigma_cluster,
     &                      accx_cluster,accy_cluster,accz_cluster)
 
	   accx(i)=accx(i)+accx_cluster
	   accy(i)=accy(i)+accy_cluster
C	   accz(i)=accz(i)+accz_cluster
	   accz(i)=0.
 
	   ENDDO
	
	   PRINT *,'evolve>> Initial accelerations computed <<'
 
C First half step :
	CALL forward_half_step_v(nbodies,vx_filgal,vy_filgal,vz_filgal,
     &                           vxp,vyp,vzp,
     &                           accx,accy,accz,accx_filgal,accy_filgal,
     &                           accz_filgal,dt)
 
	PRINT *,'evolve>> Half step complete <<'
	PRINT *,'evolve>> Initializing orbit and audit file <<'
	ncount=1
	OPEN(unit=13,file=orbit_file,status='unknown')
	WRITE(13,*)it,x_filgal,y_filgal,z_filgal
	OPEN(unit=14,file=audit_file,status='unknown')
	CALL audit(G,x_filgal,y_filgal,z_filgal,vx_filgal,vy_filgal,
     &             vz_filgal,sigma_cluster,core_radius_cluster,
     &             outer_radius_cluster,e_total,am_total)
	WRITE(14,*)it,time,am_total,e_total
	PRINT *,' '
	PRINT *,'evolve>> Beginning evolution <<'
	PRINT *,' '
 
C Main loop :
	DO it=1,nsteps
	   PRINT *,'evolve>> Step : ',it,' Time : ',time
	   IF(it.ne.1)THEN
             CALL cluster_acc(x_filgal,y_filgal,z_filgal,G,
     &                         core_radius_cluster,
     &                         outer_radius_cluster,sigma_cluster,
     &                         accx_filgal,
     &                         accy_filgal,accz_filgal)
	      DO i=1,nbodies
                 CALL filgal_acc(xp(i),yp(i),zp(i),x_filgal,
     &                           y_filgal,z_filgal,
     &                           G,lum_mass_filgal,
     &                           re_filgal,extent,
     &                           gamma_halo,accx(i),accy(i),accz(i))
                 CALL cluster_acc(xp(i),yp(i),zp(i),G,
     &                         core_radius_cluster,
     &                         outer_radius_cluster,sigma_cluster,
     &                         accx_cluster,
     &                         accy_cluster,accz_cluster)
	         accx(i)=accx(i)+accx_cluster
	         accy(i)=accy(i)+accy_cluster
C	         accz(i)=accz(i)+accz_cluster
	         accz(i)=0.
	         ENDDO
	      ENDIF 	
 
C Leap frog integration :
           CALL leap_frog(nbodies,x_filgal,y_filgal,z_filgal,
     &                    vx_filgal,
     &                    vy_filgal,vz_filgal,accx_filgal,
     &                    accy_filgal,accz_filgal,dt,time,
     &                    xp,yp,zp,vxp,vyp,vzp,
     &                    accx,accy,accz)
 
	   WRITE(13,*)it,x_filgal,y_filgal,z_filgal 	   	
 
           IF(ncount.eq.nout)THEN
	      CALL back_half_step_v(nbodies,vx_filgal,vy_filgal,vz_filgal,
     &                              vxp,vyp,vzp,
     &                              accx,accy,accz,accx_filgal,
     &                              accy_filgal,accz_filgal,dt)
	
	      PRINT *,'evolve>> Doing Audit <<'
	      CALL audit(G,x_filgal,y_filgal,z_filgal,vx_filgal,vy_filgal,
     &                   vz_filgal,sigma_cluster,core_radius_cluster,
     &                   outer_radius_cluster,e_total,am_total)
	      PRINT *,'evolve>---> Energy : ',e_total,' AM : ',am_total
	      WRITE(14,*)it,time,am_total,e_total
 
              WRITE(11,*)it,time
              DO i=1,nbodies
	         WRITE(11,*)i,xp(i),yp(i),zp(i),vxp(i),vyp(i),vzp(i)
	         ENDDO
 
              CALL forward_half_step_v(nbodies,vx_filgal,
     &                                 vy_filgal,vz_filgal,
     &                                 vxp,vyp,vzp,
     &                                 accx,accy,accz,
     &                                 accx_filgal,accy_filgal,
     &                                 accz_filgal,dt)
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
 
C	Subroutine to compute orbital elements for
C	parabolic orbits in a modified halo field.
 
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
 
C	Subroutine to compute the acceleration due to the cluster
 
C****************************************************************************
 
	SUBROUTINE cluster_acc(x,y,z,G,
     &                         core_radius_cluster,
     &                         outer_radius_cluster,sigma_cluster,
     &                         accx,accy,accz)
 
 
	PARAMETER (epsilon=0.01)
 
	radius=sqrt(x*x+y*y+z*z)
	scale=radius/core_radius_cluster
	scale2=outer_radius_cluster/core_radius_cluster
	total_mass_cluster=2.*sigma_cluster*sigma_cluster*
     &                     core_radius_cluster*
     &                     (scale2-atan(scale2))
	contained_mass=2.*sigma_cluster*sigma_cluster*
     &                 core_radius_cluster*
     &                 (scale-atan(scale))
	IF(radius.gt.outer_radius_cluster)THEN
	   the_mass=total_mass_cluster
	   ELSE
	   the_mass=contained_mass
	   ENDIF
	radial_acc=-G*the_mass/(radius*radius+epsilon*epsilon)
	accx=radial_acc*x/radius
	accy=radial_acc*y/radius
	accz=radial_acc*z/radius
	RETURN
	END
 
C*****************************************************************************
 
C	Subroutine to compute the acceleration due to the filament galaxy
 
C*****************************************************************************
 
	SUBROUTINE filgal_acc(x,y,z,x_filgal,y_filgal,z_filgal,
     &                        G,lum_mass_filgal,
     &                        re_filgal,extent,
     &                        gamma_halo,accx,accy,accz)
 
	PARAMETER (epsilon=0.01)
	REAL lum_mass_filgal
 
	pi=2.*asin(1.)
 
	xx=x-x_filgal
	yy=y-y_filgal
	zz=z-z_filgal
 
	radius=sqrt(xx*xx+yy*yy+zz*zz)
 
	radial_acc_plummer=-G*lum_mass_filgal*radius/(
     &                      radius*radius+re_filgal*re_filgal)**1.5
	
	
	scale=radius/gamma_halo
	scale2=extent
	outer_radius=extent*gamma_halo
	sigma_halo=pi*G*lum_mass_filgal/(32.*re_filgal)
	contained_mass_halo=2.*sigma_halo*sigma_halo*gamma_halo*
     &                 (scale-atan(scale))
	total_mass=2.*sigma_halo*sigma_halo*gamma_halo*
     &                 (scale2-atan(scale2))
	IF(radius.gt.outer_radius)THEN
	   radial_acc_halo=-G*total_mass/(radius*radius)
	   ELSE
           radial_acc_halo=-G*contained_mass_halo/
     &                      (radius*radius+epsilon*epsilon)
	   ENDIF
C	radial_acc_halo=0.
	total_radial_acc=radial_acc_plummer+radial_acc_halo
 
	accx=total_radial_acc*xx/radius
	accy=total_radial_acc*yy/radius
	accz=total_radial_acc*zz/radius
 
	RETURN
 
	END
 
C******************************************************************************
 
C	Subroutine to advance the velocities by half a time step
 
C******************************************************************************
 
 
	SUBROUTINE forward_half_step_v(nbodies,vx_filgal,vy_filgal,vz_filgal,
     &                                 vxp,vyp,vzp,
     &                                 accx,accy,accz,accx_filgal,accy_filgal,
     &                                 accz_filgal,dt)
 
	DIMENSION vxp(1),vyp(1),vzp(1)
	DIMENSION accx(1),accy(1),accz(1)
 
	vx_filgal=vx_filgal+accx_filgal*dt/2.
	vy_filgal=vy_filgal+accy_filgal*dt/2.
	vz_filgal=vz_filgal+accz_filgal*dt/2.
 
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
 
 
	SUBROUTINE leap_frog(nbodies,x_filgal,y_filgal,z_filgal,
     &                       vx_filgal,
     &                       vy_filgal,vz_filgal,accx_filgal,
     &                       accy_filgal,accz_filgal,dt,time,
     &                       xp,yp,zp,vxp,vyp,vzp,
     &                       accx,accy,accz)
 
	DIMENSION xp(1),yp(1),zp(1)
	DIMENSION vxp(1),vyp(1),vzp(1)
	DIMENSION accx(1),accy(1),accz(1)
 
	vx_filgal=vx_filgal+dt*accx_filgal
	vy_filgal=vy_filgal+dt*accy_filgal
	vz_filgal=vz_filgal+dt*accz_filgal
	
	x_filgal=x_filgal+dt*vx_filgal
	y_filgal=y_filgal+dt*vy_filgal
	z_filgal=z_filgal+dt*vz_filgal
 
C	print *,x_filgal,y_filgal,v_filgal
C	print *,vx_filgal,vy_filgal,vz_filgal
C	print *,accx_filgal,accy_filgal,accz_filgal
	DO i=1,nbodies	
	   IF(i.eq.10)THEN
	       xx=xp(i)-x_filgal
	       yy=yp(i)-y_filgal
	       zz=zp(i)-z_filgal
	       vx=vxp(i)-vx_filgal
	       vy=vyp(i)-vy_filgal
	       vz=vzp(i)-vz_filgal
	       rad=sqrt(xx*xx+yy*yy+zz*zz)
	       radv=(xx*vx/rad)+(yy*vy/rad)+(zz*vz/rad)	
	       radacc=(xx*accx(i)/rad)+(yy*accy(i)/rad)+
     &	              (zz*accz(i)/rad)
C	       print *,i,rad,radv,radacc
C	       print *,i,accx(i),accy(i),accz(i)
C	       print *,i,vx,vy,vz
C	       print *,i,xx,yy,zz
C	   print *,i,vxp(i),accx(i),dt,vx_filgal
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
 
 
	SUBROUTINE back_half_step_v(nbodies,vx_filgal,vy_filgal,vz_filgal,
     &                                 vxp,vyp,vzp,
     &                                 accx,accy,accz,accx_filgal,accy_filgal,
     &                                 accz_filgal,dt)
 
	DIMENSION vxp(1),vyp(1),vzp(1)
	DIMENSION accx(1),accy(1),accz(1)
 
	vx_filgal=vx_filgal-accx_filgal*dt/2.
	vy_filgal=vy_filgal-accy_filgal*dt/2.
	vz_filgal=vz_filgal-accz_filgal*dt/2.
 
	DO i=1,nbodies
	   vxp(i)=vxp(i)-accx(i)*dt/2.
	   vyp(i)=vyp(i)-accy(i)*dt/2.
	   vzp(i)=vzp(i)-accz(i)*dt/2.
	   ENDDO
 
	RETURN
	END	
 
C**************************************************************************
 
C	Subroutine to check the energy and angular momentum of the
C	filament galaxies orbit in the cluster field.
 
C**************************************************************************
 
 
	SUBROUTINE audit(G,x_filgal,y_filgal,z_filgal,vx_filgal,vy_filgal,
     &                   vz_filgal,sigma_cluster,core_radius_cluster,
     &                   outer_radius_cluster,e_total,am_total)
 
	amx=y_filgal*vz_filgal-z_filgal*vy_filgal
	amy=z_filgal*vx_filgal-x_filgal*vz_filgal
	amz=x_filgal*vy_filgal-y_filgal*vx_filgal
 
	am_total=sqrt(amx*amx+amy*amy+amz*amz)
 
	radius=sqrt(x_filgal*x_filgal+y_filgal*y_filgal+z_filgal*z_filgal)
	scale=radius/core_radius_cluster	
	scale2=outer_radius_cluster/core_radius_cluster
	
	t1=(1./scale)*atan(scale)-1.
	t2=0.5*alog((1.+scale*scale)/(1.+scale2*scale2))
	cluster_mass=2.*sigma_cluster*sigma_cluster*core_radius_cluster*(
     &                  scale2-atan(scale2))
        t3=cluster_mass/(core_radius_cluster*(scale2-atan(scale2)))
	IF(radius.lt.outer_radius_cluster)THEN
           cluster_potential=t3*(t1+t2)
	   ELSE
	   cluster_potential=-G*cluster_mass/radius
	   ENDIF
	energy_kinetic=0.5*(vx_filgal*vx_filgal+vy_filgal*vy_filgal+
     &                      vz_filgal*vz_filgal)
 
	e_total=cluster_potential+energy_kinetic
 
	RETURN
	END
