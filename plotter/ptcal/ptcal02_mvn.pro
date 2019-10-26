; Particle trajectory calculation
; runs in MKS units
; 4th order Runge-Kutta method
; uniform magnetic field
; ununiform electric field (due to v is not uniform)


pro ptcal02_mvn,r0,ene0,azv0,elev0,oplot=oplot


;----- constants ------------;
me = 9.1093897d-31 ; [kg]    ;
mi = 1.6726231d-27 ; [kg]    ;
e = 1.60217733d-19 ; [C]     ;
;----------------------------;

;----- set parameters -------------------
m = 16.d*mi                          ;- mass
q = e                           ;- charge
Vsw0 = [-200.d3,0.d3, 0.d3]       ;- [m/s] solar wind velocity
B0 = [ 0.d-9, 11.d-9, 0.d-9 ]   ;- [T]
Bms = [ 0.d-9, 20.d-9, 0.d-9 ]
Bim = [ 0.d-9, 20.d-9, 0.d-9 ]
E0 = -crossp(Vsw0,B0)
RL = m*Vsw0[1]/q/sqrt(total(B0^2))
;E0 = [ 0.d-3, 0.d-3, 1.d-3 ]    ;- [V/m]

Nmax = 1000                     ;- max step number

if ~keyword_set(r0) then r0 = [1.58 * 3389900d,0.d0,0.d0]           ;- initial location

if ~keyword_set(ene0) then ene0 = 1000.d0                  ;- initial energy [eV]
if ~keyword_set(azv0) then azv0 = 0.d0/!radeg             ;- initial azimuthal angle
if ~keyword_set(elev0) then elev0 = 0.d0/!radeg             ;- initial elevation angle
v0abs = sqrt(2.d0*ene0*abs(q)/m)
v0 = v0abs * [ cos(azv0)*cos(elev0), sin(azv0)*cos(elev0), sin(elev0) ]

time_step = 1000.d0              ;- Dt = T_c/time_step, Dt<0 for back-tracing

bin = 100000.                     ;- [km] plot box size
Xmax = bin & Xmin = -bin
Ymax = bin & Ymin = -bin
Zmax = bin+bin & Zmin = -bin+bin

; parameters of Bow Shock
Rm = 3389.9d * 1d3
X0_bs = 0.55 * Rm
e = 1.05
L = 2.1 *Rm


;Evar = E0 + [ 0.d-3, 0.d-3, - 1.d-3/dindgen(bin)]
;----------------------------------------


;----- define variables -----------------
v = dblarr(3)
r = dblarr(3)
v_rec = dblarr(Nmax,3)
r_rec = dblarr(Nmax,3)
k0v = dblarr(3) & k1v = dblarr(3) & k2v = dblarr(3) & k3v = dblarr(3)
k0r = dblarr(3) & k1r = dblarr(3) & k2r = dblarr(3) & k3r = dblarr(3)
Dv = dblarr(3)
Dr = dblarr(3)
;----------------------------------------


;----- calculate trajectory -------------
v = v0
r = r0
t = 0.d0
Vswvec_all=0d
idx_arr=0d
!p.multi=[0,1,1]
for i=0,Nmax-1 do begin

      
      Vswvec = dblarr(3) & Evec = dblarr(3) & Bvec = dblarr(3)
      Vswvec = Vsw0
      Evec = E0

      Vswvec_all  = [Vswvec_all,Vswvec[1]]
      if r[0] lt 1.57 * Rm and r[2] lt sqrt( (e^2d -1d)*(r[0]-X0_bs+L*e/(1-e^2d))^2-L^2/(e^2-1d) ) $
                      and r[2] gt -sqrt( (e^2d -1d)*(r[0]-X0_bs+L*e/(1-e^2d))^2-L^2/(e^2-1d) ) $
                      then Bvec = Bms $
                      else  Bvec = B0
     
 
      f_c = abs(q)*sqrt(total(Bvec^2))/m/(2.d0*!dpi) ;- gyro-frequency
      T_c = 1.d0/f_c                                 ;- gyration time
      Dt = T_c/time_step                             ;- delta t [s]

;---- 4th order Runge-Kutta -----
      k0v = Dt*q/m*( Evec + crossp(v,Bvec) )
      k0r = Dt*v
      k1v = Dt*q/m*( Evec + crossp(v+0.5d0*k0v,Bvec) )
      k1r = Dt*( v + 0.5d0*k0v )
      k2v = Dt*q/m*( Evec + crossp(v+0.5d0*k1v,Bvec) )
      k2r = Dt*( v + 0.5d0*k1v )
      k3v = Dt*q/m*( Evec + crossp(v+k2v,Bvec) )
      k3r = Dt*( v + k2v )
      Dv = ( k0v + 2.d0*k1v + 2.d0*k2v + k3v )/6.d0
      Dr = ( k0r + 2.d0*k1r + 2.d0*k2r + k3r )/6.d0
      v = v + Dv & r = r + Dr & t = t + Dt
;--------------------------------

      v_rec[i,*] = v
      r_rec[i,*] = r
      print,'i',i
      print,'t',t
      print,'r',r
      print,'v',v
     idx1000 = where(r_rec[*,1] ge 1000e3)
     idx2000 = where(r_rec[*,1] ge 2000e3)
     idx3000 = where(r_rec[*,1] ge 2000e3)
endfor
;----------------------------------------
Vswvec_all = Vswvec_all[1:*]

;----- check values ---------------------
print,'--- input values ---'
print,'m [kg]',m
print,'q [C]',q
print,'B [nT]',B0[0]*1.d9,B0[1]*1.d9,B0[2]*1.d9,sqrt(total(B0^2))*1.d9
print,'E [mV/m]',E0[0]*1.d3,E0[1]*1.d3,E0[2]*1.d3,sqrt(total(E0^2))*1.d3
print,'ene0 [eV]',ene0
print,'v0 [km/s]',v0*1.d-3,v0abs*1.d-3
print,'T_c [s]',T_c
print,'Dt [s]',Dt
print,'Nmax',Nmax
;----------------------------------------


;----- plot trajectory ------------------
;plot_3dbox,r_rec[*,0]*1.d-3,r_rec[*,1]*1.d-3,r_rec[*,2]*1.d-3, $
; /xy_plane,/yz_plane,/xz_plane,thick=2.0, $
; xtitle='x [km]',ytitle='y [km]',ztitle='z [km]',charsize=2., $
; xstyle=1,ystyle=1,zstyle=1, $
; xystyle=0,yzstyle=0,xzstyle=0, $
; xrange=[Xmax,Xmin],yrange=[Ymax,Ymin],zrange=[Zmin,Zmax], $
; title='Particle trajectory!C!CB=('+string(B0[0]*1.d9,format='(f4.1)')+','$
; +string(B0[1]*1.d9,format='(f4.1)')+','+string(B0[2]*1.d9,format='(f4.1)')$
; +') [nT], E=('+string(E0[0]*1.d3,format='(f4.1)')+','$
; +string(E0[1]*1.d3,format='(f4.1)')+','+string(E0[2]*1.d3,format='(f4.1)')$
; +') [mV/m]',az=60
;;----------------------------------------
;
;
;
;;----------------------------------------
;;----------------------------------------
;;----------------------------------------
;;----------------------------------------
;
;
;plot,v_rec[*,0],v_rec[*,2],/iso,xrange=[-1000000,1000000],yrange=[-1000000,1000000]
;plots,circle_v2(Vsw0[1],0,Vsw0[1]),color=80
;plots,circle_v2(Vswvec_all[idx1000[0]],0,Vswvec_all[idx1000[0]]),color=120
;plots,circle_v2(Vswvec_all[idx2000[0]],0,Vswvec_all[idx2000[0]]),color=230
;oplot,[v_rec[idx1000[0],0]],[v_rec[idx1000[0],2]],color=120,psym=5
;oplot,[v_rec[idx2000[0],0]],[v_rec[idx2000[0],2]],color=230,psym=5




; parameters of Bow Shock
Rm = 3389.9d * 1d3
X0_bs = 0.55 * Rm
e = 1.05
L = 2.1 *Rm
Rss = 1.58
t =findgen(360)*!DTOR
r = L/(1.+e*cos(t)) 

;Xbs = -1.58 * Rm + dindgen(10000)*0.001 *Rm
;ybs_p = sqrt( (e^2d -1d)*(xbs-X0_bs+L*e/(1-e^2d))^2-L^2/(e^2-1d) )
;ybs_m = -sqrt( (e^2d -1d)*(xbs-X0_bs+L*e/(1-e^2d))^2-L^2/(e^2-1d) )
Xbs = r*cos(t) + X0_bs
Ybs = r*sin(t)
t_term = acos( -X0_bs/(L+e*X0_bs) )
r_bs = L/(1.+e*cos(t_term))*sin(t_term)
crcl_bs = circle_v2(0,0,r_bs)

if ~keyword_set(oplot) then plot,r_rec[*,0]/Rm,r_rec[*,2]/Rm,/iso,xrange=[4,-5],yrange=[-5,5],/xstyle,/ystyle,charsize=2 $
else oplot,r_rec[*,0]/Rm,r_rec[*,2]/Rm
plots,circle_v2(0,0,1),thick=2
oplot,Xbs/Rm,Ybs/Rm,color=230
;oplot,Xbs/Rm,Ybs_m/Rm,color=230


; paremeters of imb
X0_imb = 0.86
e = 0.92
L = 0.9
Rss = 1.33
t = findgen(360)*!DTOR
r = L/(1.+e*cos(t))

Ximb = r*cos(t) + X0_imb
Yimb = r*sin(t)
t_term = acos( -X0_imb/(L+e*X0_imb) )
r_imb = L/(1.+e*cos(t_term))*sin(t_term)
crcl_imb = circle_v2(0,0,r_imb)
oplot,Ximb,Yimb,color=80


!p.multi=[0,1,1]

end
