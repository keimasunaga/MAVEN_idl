pro test_sta_dens,emplot=emplot

  trange = time_double(['2015-03-05/00:00:00','2015-03-05/04:00:00'])
  erange = [0,100]
  mass_range = [12,20]
  mass_assmpt = 16.
  get_apid='d0'


  event = 1
  timespan,trange
  if not keyword_set(sta_apid) then l2_load_apid = ['c6','d0','d1']
  
  mvn_sta_l2_load,sta_apid=l2_load_apid
  mvn_sta_l2_tplot
  if get_apid eq 'd0' then get_data,'mvn_sta_d0_M',data=d $
  else get_data,'mvn_sta_d1_E',data=d

  sz = size(d,/type)
  if sz ne 8 then begin
    print,'No data, returning.'
    data_ok = 0
    return
  endif

  if get_apid eq 'd0' then idx = nn('mvn_sta_d0_M',trange) else idx = nn('mvn_sta_d1_E',trange)
  time = d.x[idx[0]:idx[1]]
 
  dens_arr = 0.
  vel_arr = fltarr(3)
  flux_arr = fltarr(3)

  for it = 0,n_elements(time)-1 do begin
 
    if get_apid eq 'd0' then tspan = [time[it]-64d,time[it]+64d] $
                                else tspan = [time[it]-8d,time[it]+8d]  

    result = execute('dat = mvn_sta_get(get_apid,tt=tspan)') 
;    sta_getdat_bgrm,dat,dat_new,ratio=0.5
;    if keyword_set(emplot) then begin
;      wi,1
;      !p.multi=[0,1,2]
;      ;mvn_sta_emplot,data=dat_new,tspan,window=1,/rotate,/fill,/label,yrange=[0,7],zrange=[1e-12,1e-6],/add,units='df',charsize=2
;      mvn_sta_emplot,data=dat,window=1,/fill,/mass,/points,/label,zrange=[10^2.5,1e7],units='eflux',charsize=2
;      mvn_sta_emplot,data=dat_new,window=1,/fill,/mass,/points,/label,zrange=[10^2.5,1e7],units='eflux',charsize=2
;      stop
;    endif
 
    dens = n_4d(dat,energy=erange,mass=mass_range,m_int=mass_assmpt)
    vel = v_4d(dat,energy=erange,mass=mass_range,m_int=mass_assmpt)
    flux = j_4d(dat,energy=erange,mass=mass_range,m_int=mass_assmpt)     
    dens_arr = [dens_arr,dens]
    vel_arr = [[vel_arr],[vel]]
    flux_arr = [[flux_arr],[flux]]    
     
  endfor
    
  dens_arr = dens_arr[1:*]
  vel_arr = transpose(vel_arr[*,1:*])
  flux_arr = transpose(flux_arr[*,1:*])
    
  store_data,'dens',data={x:time,y:dens_arr}
  store_data,'vel',data={x:time,y:vel_arr}
  store_data,'flux',data={x:time,y:flux_arr}
  options,'vel',colors=[80,120,230],labels=['Vx','Vy','Vz'],labflag=1
  options,'flux',colors=[80,120,230],labels=['Fx','Fy','Fz'],labflag=1
  tplot,['mvn_sta_'+get_apid+'_E','mvn_sta_'+get_apid+'_H_E','mvn_sta_'+get_apid+'_M','dens','vel','flux']
  
  stop
end