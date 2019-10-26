pro plot_deflect_ion,tenkev=tenkev,onekev=onekev,hundredev=hundredev,d1=d1,d0=d0

    orbit_arr = findgen(27)+336;[465]

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    xyrange = [-4,4]
    res = 0.25
    nbin = xyrange[1]/res * 2. + 1
    points = findgen(nbin)*res-xyrange[1]
    
    
    im_yz = fltarr(nbin,nbin) & im_yz2 = fltarr(nbin,nbin)
    n_yz = fltarr(nbin,nbin)
    for ii=0,n_elements(orbit_arr)-1 do begin    
       if not keyword_set(d0) then filename = SAVE_LOC + '/maven/sav/flux/deflect_ion/d1/'+string(orbit_arr[ii],format='(i05)')+'/flux_vdf_*.sav' $ 
                              else filename = SAVE_LOC + '/maven/sav/flux/deflect_ion/d0/'+string(orbit_arr[ii],format='(i05)')+'/flux_vdf_*.sav'
       fs = file_search(filename)
       file_sw_info = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit_arr[ii],format='(i05)')+'.sav' 
       ft_sw = file_test(file_sw_info)
       if ft_sw eq 0 then goto, noadd
       restore,file_sw_info
       time_bs = dat_sw.time

       for jj=0,n_elements(fs)-1 do begin
          restore,fs[jj]
          
          time = dat_sav.time
          mtime = (time[0]+time[1])/2d
          
          if mtime gt time_bs[0] and mtime lt time_bs[1] then goto, noadd
          Odens_10kev = dat_sav.Odens_10kev
          Ovel_mso_10kev = dat_sav.Ovel_mso_10kev
          Oflux_10kev = Odens_10kev * Ovel_mso_10kev * 1e5
          pos = dat_sav.pos_mso
          vmso = dat_sav.vmso
          bmso = dat_sav.bmso
          if finite(vmso[0]) eq 0 or finite(bmso[0]) eq 0 then goto, noadd
          
          rot = get_rot_angle(vmso,bmso)
          pos_mse = mso2mse(pos[0],pos[1],pos[2],rot)
          iy = round((pos_mse[1]+xyrange[1])/res) 
          iz = round((pos_mse[2]+xyrange[1])/res)
          im_yz[iy,iz] = im_yz[iy,iz] + Oflux_10kev[0]
          n_yz[iy,iz] = n_yz[iy,iz] + 1.
          noadd:
       endfor
       print,ovel_mso_10kev
       
    endfor   
    
    idx = where(n_yz ne 0)
    im_yz2[idx] = im_yz[idx]/n_yz[idx]
    
    loadct2,39
    plotxyz,points,points,im_yz2,mult='2,1',charsize=2,/zlog,range=[.1,1e4]
    plotxyz,points,points,n_yz,/add,charsize=2,/zlog,range=[1,100]
   stop

end