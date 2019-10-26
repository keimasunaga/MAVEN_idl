;+
; :Description:
;  This routine plots MAVEN's orbit in the MSO or MSE plane.
;
; ${parameters}
;
; ${keywords}
;  orbit: If set, whole MAVEN's orbit in the same orbit number is plotted.
;  time_arr: If set, MAVEN's orbit in this time array is plotted
;  seconds: If set, MAVEN's orbit data point is second
;  frame: set the name of the coordinate frame in the string. Default is 'MSO'.
;
; ${Related routines}
;  save_vex_amda_moment.pro
;
; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
;
; $Last modified Jun 18, 2015
;-


;; sub routine
function get_avg_clock_angle,rot_agl_in,rot_agl_out
  
      if rot_agl_in*rot_agl_out ge 0 then rot_agl = (rot_agl_in + rot_agl_out)/2.
      if rot_agl_in*rot_agl_out lt 0 then rot_agl = ( rot_agl_in + (2*!pi-rot_agl_out) )/2.
  
      return,rot_agl 
end

;; main routine
pro plot_mvn_orbit_upstrm,time_arr,time_upstrm=time_upstrm,orbit=orbit,xy=xy,yz=yz,xz=xz,cyl=cyl,all=all,oplot=oplot,$
                          seconds=seconds,frame=frame,show_time=show_time,Bvec=Bvec,Vvec=Vvec,Evec=Evec,len_ratio=len_ratio,Busvec=Busvec,Vusvec=Vusvec,Eusvec=Eusvec,$
                          charsize=charsize,only_map=only_map,angle=angle,title=title,show_sza=show_sza,show_alt=show_alt,$
                          output=output,$
                          xrange=xrange,yrange=yrange

   env = init_env()
   SAVE_LOC = env.SAVE_LOC
  
   ; disk
   R_m = 3389.9D
   h = findgen(180)*2*!DTOR
   disk=[[cos(h)],[sin(h)]]

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

   ; parameters of Bow Shock
   X0_bs = 0.55
   e = 1.05
   L = 2.1
   Rss = 1.58
   t = findgen(360)*!DTOR
   r = L/(1.+e*cos(t))

   Xbs = r*cos(t) + X0_bs
   Ybs = r*sin(t)
   t_term = acos( -X0_bs/(L+e*X0_bs) )
   r_bs = L/(1.+e*cos(t_term))*sin(t_term)
   crcl_bs = circle_v2(0,0,r_bs)
   
   if not keyword_set(len_ratio) then len_ratio = 1.0
  
  if keyword_set(only_map) then begin
   
   
  
   if keyword_set(xy) then begin
       if not keyword_set(xrange) then xrange = [3,-3]
       if not keyword_set(yrange) then yrange = [3,-3]
       plot,[0],/iso,xrange=xrange,yrange=yrange,/xsty,/ysty,xtit='X',ytit='Y',tit=title,charsize=charsize
       oplot,disk[*,0],disk[*,1]
       oplot,Ximb,Yimb,color=80
       oplot,Xbs,Ybs,color=230      
   endif
  
   if keyword_set(yz) then begin
       if not keyword_set(xrange) then xrange = [-3,3]
       if not keyword_set(yrange) then yrange = [-3,3]
       plot,disk[*,0],disk[*,1],/iso,xrange=xrange,yrange=yrange,/xsty,/ysty,xtit='Y',ytit='Z',tit=title,charsize=charsize
       oplot,crcl_imb[0,*],crcl_imb[1,*],color=80
       oplot,crcl_bs[0,*],crcl_bs[1,*],color=230
   endif
   
   if keyword_set(xz) then begin
       if not keyword_set(xrange) then xrange = [3,-3]
       if not keyword_set(yrange) then yrange = [-3,3]
       plot,disk[*,0],disk[*,1],/iso,xrange=xrange,yrange=yrange,/xsty,/ysty,xtit='X',ytit='Z',tit=title,charsize=charsize
       oplot,disk[*,0],disk[*,1]
       oplot,Ximb,Yimb,color=80
       oplot,Xbs,Ybs,color=230
   endif
  
   if keyword_set(cyl) then begin
       if not keyword_set(xrange) then xrange = [3,-3]
       yrange = [0,5]
       plot,disk[*,0],disk[*,1],/iso,xrange=xrange,yrange=yrange,/xsty,/ysty,xtit='X',ytit='sqrt(Y^2+Z^2)',tit=title,charsize=charsize
       oplot,Ximb,Yimb,color=80
       oplot,Xbs,Ybs,color=230
       stop
   endif  
  
   return
  endif
   
  
   if not keyword_set(frame) then frame = 'MSO'
   if ~keyword_set(xy) and ~keyword_set(yz) and ~keyword_set(xz) and ~keyword_set(cyl) then all = 1 else all=0
   
   if keyword_set(time_arr) then begin
     trange = minmax(time_arr)
     if n_elements(time_arr) eq 2 then begin
       if keyword_set(seconds) then time_arr2 = time_arr[0] + dindgen(time_arr[1]-time_arr[0]+1)  ;; data points per second  
       if keyword_set(center) then time_arr2 = (time_arr[0]+time_arr[1])/2d
     endif else begin
       time_arr2 = time_arr
     endelse
     
   endif
   
   if keyword_set(time_upstrm) then begin
     trange = minmax(time_upstrm)
     if n_elements(time_upstrm) eq 2 then begin
       if keyword_set(seconds) then time_arr_us = time_upstrm[0] + dindgen(time_upstrm[1]-time_upstrm[0]+1)  ;; data points per second
       if keyword_set(center) then time_arr_us = (time_upstrm[0]+time_upstrm[1])/2d
     endif else begin
       time_arr_us = time_upstrm
     endelse

   endif
   
   
   if keyword_set(orbit) then begin
     trange = mvn_orbit_num(orbnum=[orbit-0.5,orbit+0.5])
     time_arr2 = trange[0]+dindgen( fix( (trange[1]-trange[0]+1)/60d ) )*60d  ; data points per minute
     if keyword_set(seconds) then time_arr2 = trange[0]+dindgen(trange[1]-trange[0]+1) ;; data points per second
     time_points = trange[0]+dindgen( fix( (trange[1]-trange[0]+1)/60d/60d ) )*60d*60d  ; 60 min data points
   endif   

;   if keyword_set(yz) then begin
;     if ~keyword_set(oplot) then begin
;       plot,disk[*,0],disk[*,1],/iso,xrange=[-3,3],yrange=[-3,3],xtit='Y',ytit='Z',tit=frame+' frame',charsize=charsize
;       oplot,crcl_imb[0,*],crcl_imb[1,*],color=80
;       oplot,crcl_bs[0,*],crcl_bs[1,*],color=230
;  
   
   ;; load maven orbit
   print,'Read SPICE Below!!'
   s = spice_file_source()
   s.no_server = 1
   mk = mvn_spice_kernels(/load,trange=trange,source=s)
   pos = spice_body_pos('MAVEN','MARS',frame='MAVEN_MSO',utc=time_arr2)
   if keyword_set(time_upstrm) then pos_us = spice_body_pos('MAVEN','MARS',frame='MAVEN_MSO',utc=time_arr_us)
   if keyword_set(show_time) then begin
    pos_tp = spice_body_pos('MAVEN','MARS',frame='MAVEN_MSO',utc=time_points)  
    tp_arr = time_string(time_points)
    ;tp_arr = time_string(time_points + ( round(( time_points-time_double(strmid(time_string(trange[0]),0,13)) )/3600d)-( time_points-time_double(strmid(time_string(trange[0]),0,13)) )/3600d ) *60d*60d)
   endif

   ;; mso frame
   x=pos[0,*]/R_m
   y=pos[1,*]/R_m
   z=pos[2,*]/R_m
   sza = acos(x/sqrt(x^2+y^2+z^2))*!radeg
   alt = (sqrt(x^2+y^2+z^2)-1.)*R_m
   if not keyword_set(cyl) then if keyword_set(oplot) then ExBvec = crossp(Evec,Bvec)
   if keyword_set(time_upstrm) then if not keyword_set(cyl) then if keyword_set(oplot) then ExBusvec = crossp(Eusvec,Busvec)
   
   if keyword_set(show_time) then begin
     x_tp=pos_tp[0,*]/R_m
     y_tp=pos_tp[1,*]/R_m
     z_tp=pos_tp[2,*]/R_m
   endif
   
   if keyword_set(time_upstrm) then begin
     x_us=pos_us[0,*]/R_m
     y_us=pos_us[1,*]/R_m
     z_us=pos_us[2,*]/R_m
   endif
   
   ;; mse frame
   if frame eq 'MSE' or frame eq 'mse' then begin
    
     if ~keyword_set(angle) then begin
       print,'Use average rot angle in the solar wind region'
       filename = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit,format='(i05)')+'.sav'
       restore,filename
       angle = get_avg_clock_angle(dat_sw.Angles_in.rotation_angle, dat_sw.Angles_out.rotation_angle)
     endif
    
     mse = mso2mse(reform(pos[0,*])/R_m, reform(pos[1,*])/R_m, reform(pos[2,*])/R_m, angle)
     x = mse[*,0]
     y = mse[*,1]
     z = mse[*,2]  
     
     if keyword_set(oplot) then begin
       Bvec = mso2mse(Bvec[0],Bvec[1],Bvec[2],angle)
       Vvec = mso2mse(Vvec[0],Vvec[1],Vvec[2],angle)
       Evec = mso2mse(Evec[0],Evec[1],Evec[2],angle)
       ExBvec = crossp(Evec,Bvec)      
     endif
     
     if keyword_set(time_upstrm) then begin
       mse_us = mso2mse(reform(pos_us[0,*])/R_m, reform(pos_us[1,*])/R_m, reform(pos_us[2,*])/R_m, angle)
       x_us = mse_us[*,0]
       y_us = mse_us[*,1]
       z_us = mse_us[*,2]
       Busvec = mso2mse(Busvec[0],Busvec[1],Busvec[2],angle)  ;;Don't use angle_us
       Vusvec = mso2mse(Vusvec[0],Vusvec[1],Vusvec[2],angle)  ;;Don't use angle_us
       Eusvec = mso2mse(Eusvec[0],Eusvec[1],Eusvec[2],angle)  ;;Don't use angle_us
       ExBusvec = crossp(Eusvec,Busvec)
     endif
     
     
     if keyword_set(show_time) then begin
       mse_tp = mso2mse(reform(pos_tp[0,*])/R_m, reform(pos_tp[1,*])/R_m, reform(pos_tp[2,*])/R_m, angle)
       x_tp = mse_tp[*,0]
       y_tp = mse_tp[*,1]
       z_tp = mse_tp[*,2]
     endif
     
   endif
   
   
   ;; plot orbit
   loadct2,39
   if all eq 1 then begin
    xy=1 & yz=1 & xz=1 & cyl=1
    !p.multi=[0,2,2]
   endif
   
   ;- xy plane
   if keyword_set(xy) then begin
    if ~keyword_set(oplot) then begin
     if not keyword_set(title) then title = frame+' frame'
     if not keyword_set(xrange) then xrange_xy = [3,-3] else xrange_xy = xrange
     if not keyword_set(yrange) then yrange_xy = [3,-3] else yrange_xy = yrange
     plot,[0],/iso,xrange=xrange_xy,yrange=yrange_xy,/xsty,/ysty,xtit='X',ytit='Y',tit=title,charsize=charsize
     oplot,disk[*,0],disk[*,1]
     oplot,Ximb,Yimb,color=80
     oplot,Xbs,Ybs,color=230
     oplot, x,y
     if keyword_set(show_time) then begin
       oplot, x_tp, y_tp, psym=1
       for q=0,n_elements(x_tp)-1 do xyouts,x_tp[q],y_tp[q],strmid(tp_arr[q],11,5),/data
     endif
    endif else begin
     oplot,x,y,thick=4,color=1
     if keyword_set(Evec) then arrow,mean(x),mean(y),mean(x)+Evec[0]*len_ratio,mean(y)+Evec[1]*len_ratio,color=4,/data,hsize=.5,thick=2
     if keyword_set(Bvec) then arrow,mean(x),mean(y),mean(x)+Bvec[0]*len_ratio,mean(y)+Bvec[1]*len_ratio,color=2,/data,hsize=.5,thick=2
     if keyword_set(Vvec) then arrow,mean(x),mean(y),mean(x)+Vvec[0]*len_ratio,mean(y)+Vvec[1]*len_ratio,color=6,/data,hsize=.5,thick=2
     if keyword_set(Evec) then arrow,mean(x),mean(y),mean(x)+ExBvec[0]*len_ratio,mean(y)+ExBvec[1]*len_ratio,color=210,/data,hsize=.5,thick=2
     if keyword_set(Eusvec) then arrow,mean(x_us),mean(y_us),mean(x_us)+Eusvec[0]*len_ratio,mean(y_us)+Eusvec[1]*len_ratio,color=4,/data,hsize=.5,thick=2
     if keyword_set(Busvec) then arrow,mean(x_us),mean(y_us),mean(x_us)+Busvec[0]*len_ratio,mean(y_us)+Busvec[1]*len_ratio,color=2,/data,hsize=.5,thick=2
     if keyword_set(Vusvec) then arrow,mean(x_us),mean(y_us),mean(x_us)+Vusvec[0]*len_ratio,mean(y_us)+Vusvec[1]*len_ratio,color=6,/data,hsize=.5,thick=2
     if keyword_set(Eusvec) then arrow,mean(x_us),mean(y_us),mean(x_us)+ExBusvec[0]*len_ratio,mean(y_us)+ExBusvec[1]*len_ratio,color=210,/data,hsize=.5,thick=2
    endelse
   endif
   
   ;- yz plane
   if keyword_set(yz) then begin
    if ~keyword_set(oplot) then begin
     if not keyword_set(title) then title = frame+' frame'
     if not keyword_set(xrange) then xrange_yz = [-3,3] else xrange_yz = xrange
     if not keyword_set(yrange) then yrange_yz = [-3,3] else yrange_yz = yrange
     plot,disk[*,0],disk[*,1],/iso,xrange=xrange_yz,yrange=yrange_yz,/xsty,/ysty,xtit='Y',ytit='Z',tit=title,charsize=charsize
     oplot,crcl_imb[0,*],crcl_imb[1,*],color=80 
     oplot,crcl_bs[0,*],crcl_bs[1,*],color=230
     oplot, y,z
     if keyword_set(show_time) then begin
       oplot, y_tp, z_tp, psym=1
       for q=0,n_elements(x_tp)-1 do xyouts,y_tp[q],z_tp[q],strmid(tp_arr[q],11,5),/data
     endif
    endif else begin
     oplot,y,z,thick=4,color=1
     if keyword_set(Evec) then arrow,mean(y),mean(z),mean(y)+Evec[1]*len_ratio,mean(z)+Evec[2]*len_ratio,color=4,/data,hsize=.5,thick=2
     if keyword_set(Bvec) then arrow,mean(y),mean(z),mean(y)+Bvec[1]*len_ratio,mean(z)+Bvec[2]*len_ratio,color=2,/data,hsize=.5,thick=2
     if keyword_set(Vvec) then arrow,mean(y),mean(z),mean(y)+Vvec[1]*len_ratio,mean(z)+Vvec[2]*len_ratio,color=6,/data,hsize=.5,thick=2
     if keyword_set(Evec) then arrow,mean(y),mean(z),mean(y)+ExBvec[1]*len_ratio,mean(z)+ExBvec[2]*len_ratio,color=210,/data,hsize=.5,thick=2
     if keyword_set(Eusvec) then arrow,mean(y_us),mean(z_us),mean(y_us)+Eusvec[1]*len_ratio,mean(z_us)+Eusvec[2]*len_ratio,color=4,/data,hsize=.5,thick=2
     if keyword_set(Busvec) then arrow,mean(y_us),mean(z_us),mean(y_us)+Busvec[1]*len_ratio,mean(z_us)+Busvec[2]*len_ratio,color=2,/data,hsize=.5,thick=2
     if keyword_set(Vusvec) then arrow,mean(y_us),mean(z_us),mean(y_us)+Vusvec[1]*len_ratio,mean(z_us)+Vusvec[2]*len_ratio,color=6,/data,hsize=.5,thick=2
     if keyword_set(Eusvec) then arrow,mean(y_us),mean(z_us),mean(y_us)+ExBusvec[1]*len_ratio,mean(z_us)+ExBusvec[2]*len_ratio,color=210,/data,hsize=.5,thick=2
    endelse
     
   endif
   
   ;- xz plane
   if keyword_set(xz) then begin
    if ~keyword_set(oplot) then begin
     if not keyword_set(title) then title = frame+' frame'
     if not keyword_set(xrange) then xrange_xz = [3,-3] else xrange_xz = xrange
     if not keyword_set(yrange) then yrange_xz = [-3,3] else yrange_xz = yrange
     plot,disk[*,0],disk[*,1],/iso,xrange=xrange_xz,yrange=yrange_xz,/xsty,/ysty,xtit='X',ytit='Z',tit=title,charsize=charsize
     oplot,disk[*,0],disk[*,1]
     oplot,Ximb,Yimb,color=80
     oplot,Xbs,Ybs,color=230
     oplot, x,z
     if keyword_set(show_time) then begin
       oplot, x_tp, z_tp, psym=1
       for q=0,n_elements(x_tp)-1 do xyouts,x_tp[q],z_tp[q],strmid(tp_arr[q],11,5),/data
     endif
    endif else begin
     oplot, x,z,thick=4,color=1
     if keyword_set(Evec) then arrow,mean(x),mean(z),mean(x)+Evec[0]*len_ratio,mean(z)+Evec[2]*len_ratio,color=4,/data,hsize=.5,thick=2
     if keyword_set(Bvec) then arrow,mean(x),mean(z),mean(x)+Bvec[0]*len_ratio,mean(z)+Bvec[2]*len_ratio,color=2,/data,hsize=.5,thick=2
     if keyword_set(Vvec) then arrow,mean(x),mean(z),mean(x)+Vvec[0]*len_ratio,mean(z)+Vvec[2]*len_ratio,color=6,/data,hsize=.5,thick=2
     if keyword_set(Evec) then arrow,mean(x),mean(z),mean(x)+ExBvec[0]*len_ratio,mean(z)+ExBvec[2]*len_ratio,color=210,/data,hsize=.5,thick=2
     if keyword_set(Eusvec) then arrow,mean(x_us),mean(z_us),mean(x_us)+Eusvec[0]*len_ratio,mean(z_us)+Eusvec[2]*len_ratio,color=4,/data,hsize=.5,thick=2
     if keyword_set(Busvec) then arrow,mean(x_us),mean(z_us),mean(x_us)+Busvec[0]*len_ratio,mean(z_us)+Busvec[2]*len_ratio,color=2,/data,hsize=.5,thick=2
     if keyword_set(Vusvec) then arrow,mean(x_us),mean(z_us),mean(x_us)+Vusvec[0]*len_ratio,mean(z_us)+Vusvec[2]*len_ratio,color=6,/data,hsize=.5,thick=2
     if keyword_set(Eusvec) then arrow,mean(x_us),mean(z_us),mean(x_us)+ExBusvec[0]*len_ratio,mean(z_us)+ExBusvec[2]*len_ratio,color=210,/data,hsize=.5,thick=2
    endelse
   endif   
   
   ;- clyndal plane
   if keyword_set(cyl) then begin
    if ~keyword_set(oplot) then begin
     if not keyword_set(title) then title ='orbit '+string(orbit,format='(i05)')+': '+strmid(time_string(trange[0]),0,10)
     if not keyword_set(xrange) then xrange_cyl = [3,-3] else xrange_cyl = xrange
     yrange_cyl = [0,5]
     plot,disk[*,0],disk[*,1],/iso,xrange=xrange_cyl,yrange=yrange_cyl,/xsty,/ysty,xtit='X',ytit='sqrt(Y^2+Z^2)',tit=title,charsize=charsize
     oplot,Ximb,Yimb,color=80
     oplot,Xbs,Ybs,color=230
     oplot, x,sqrt(y^2+z^2)
     if keyword_set(show_time) then begin
       oplot, x_tp, sqrt(y_tp^2+z_tp^2), psym=1
       for q=0,n_elements(x_tp)-1 do xyouts,x_tp[q],sqrt(y_tp[q]^2+z_tp[q]^2),strmid(tp_arr[q],11,5),/data
     endif
    endif else begin
     oplot,x,sqrt(y^2+z^2),thick=4,color=1
     if keyword_set(show_sza) then xyouts,3.8,4.5,'SZA: '+strtrim(string(round(mean(sza))),2)+' deg',/data
     if keyword_set(show_alt) then xyouts,3.8,4.0,'ALT: '+strtrim(string(round(mean(alt))),2)+' km',/data
    endelse
   endif   
   
   if keyword_set(all) eq 1 then !p.multi=[0,1,1]
     
   output = {time:time_arr2, pos_x:transpose(x), pos_y:transpose(y), pos_z:transpose(z)}    
      
   
   
end