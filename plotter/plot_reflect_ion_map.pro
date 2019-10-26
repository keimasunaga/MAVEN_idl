pro plot_reflect_ion_map

    orbit_arr = findgen(300) + 336

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    
    nr = 16.
    nt = 18.
    
    
    res = 0.25
    range = 4.
    nbin = 2.*range/res     
    each_axis = (findgen(nbin+1) - nbin/2.)*res
    f_xz_t = fltarr(nbin+1,nbin+1)
    f_xz_t2 = fltarr(nbin+1,nbin+1)
    f_xz_x = fltarr(nbin+1,nbin+1)
    f_xz_x2 = fltarr(nbin+1,nbin+1)
    f_xz_z = fltarr(nbin+1,nbin+1)
    f_xz_z2 = fltarr(nbin+1,nbin+1) 

    
    v_xz_x = fltarr(nbin+1,nbin+1)
    v_xz_x2 = fltarr(nbin+1,nbin+1)
    v_xz_z = fltarr(nbin+1,nbin+1)
    v_xz_z2 = fltarr(nbin+1,nbin+1)  
    n_xz = fltarr(nbin+1,nbin+1)
   
    f_yz_t = fltarr(nbin+1,nbin+1)
    f_yz_t2 = fltarr(nbin+1,nbin+1)
    f_yz_y = fltarr(nbin+1,nbin+1)
    f_yz_y2 = fltarr(nbin+1,nbin+1)
    f_yz_z = fltarr(nbin+1,nbin+1)
    f_yz_z2 = fltarr(nbin+1,nbin+1)

    v_yz_y = fltarr(nbin+1,nbin+1)
    v_yz_y2 = fltarr(nbin+1,nbin+1)
    v_yz_z = fltarr(nbin+1,nbin+1)
    v_yz_z2 = fltarr(nbin+1,nbin+1)
    n_yz = fltarr(nbin+1,nbin+1)
   
    rres = 0.25
    thres = 5 * !DTOR
    phres = 5 * !DTOR
    
    nr = 4./rres
    nth = !pi/2./thres
    nph = 2.*!pi/thres
    
    f = fltarr(nr+1,nth+1,nph+1)
   
   stop
   
    for in=0,n_elements(orbit_arr)-1 do begin  
     
      filename = SAVE_LOC + '/maven/sav/flux/deflect_ion/d0/'+string(orbit_arr[in],format='(i05)')+'/flux_vdf_*.sav'   
      fs = file_search(filename)
      if fs[0] eq '' then goto, no_data
       
      file_sw = SAVE_LOC + '/maven/sav/solar_wind_info/15min/sw_info_'+string(orbit_arr[in],format='(i05)')+'.sav'
      ft_sw = file_test(file_sw)
      if ft_sw eq 0 then goto, no_data
      restore,file_sw
      time_bs = dat_sw.time
      
      for ii=0,n_elements(fs)-1 do begin
          
          restore,fs[ii]
          time = dat_sav.time
          mtime = total(time)/2d
          if mtime gt time_bs[0] and mtime lt time_bs[1] then goto, skip
                  
          Vmso = dat_sav.Vmso
          Bmso = dat_sav.Bmso
          fnt_v = finite(Vmso)
          fnt_b = finite(Bmso)
          if fnt_v[0] eq 0 or fnt_v[1] eq 0 or fnt_v[2] eq 0 then goto, skip
          if fnt_b[0] eq 0 or fnt_b[1] eq 0 or fnt_b[2] eq 0 then goto, skip
          rot = get_rot_angle(Vmso,Bmso)
          
          Odens_10keV = dat_sav.Odens_10keV
          Ovel_mso_10keV = dat_sav.Ovel_mso_10keV   
          Ovel_mse_10keV = mso2mse(Ovel_mso_10keV[0],Ovel_mso_10keV[1],Ovel_mso_10keV[2],rot)   
          Oflux_mso_10keV = Odens_10keV * Ovel_mso_10keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_10keV = mso2mse(Oflux_mso_10keV[0],Oflux_mso_10keV[1],Oflux_mso_10keV[2],rot)       
          
          Odens_1keV = dat_sav.Odens_1keV
          Ovel_mso_1keV = dat_sav.Ovel_mso_1keV
          Ovel_mse_1keV = mso2mse(Ovel_mso_1keV[0],Ovel_mso_1keV[1],Ovel_mso_1keV[2],rot)
          Oflux_mso_1keV = Odens_1keV * Ovel_mso_1keV * 1e5 ;[#/cm2/sec]
          Oflux_mse_1keV = mso2mse(Oflux_mso_1keV[0],Oflux_mso_1keV[1],Oflux_mso_1keV[2],rot)
          
          pos_mso = dat_sav.pos_mso
          pos_mse = mso2mse(pos_mso[0],pos_mso[1],pos_mso[2],rot)
          pos_r = total(pos_mse^2)^.5
          
          i = round(pos_mse[0]/res) + nbin/2.
          j = round(pos_mse[1]/res) + nbin/2.
          k = round(pos_mse[2]/res) + nbin/2.
          
          f_xz_t[i,k] = f_xz_t[i,k] + total(Oflux_mse_10keV^2)^.5
          f_xz_x[i,k] = f_xz_x[i,k] + Oflux_mse_10keV[0]
          f_xz_z[i,k] = f_xz_z[i,k] + Oflux_mse_10keV[2]
          v_xz_x[i,k] = v_xz_x[i,k] + Ovel_mse_10keV[0]
          v_xz_z[i,k] = v_xz_z[i,k] + Ovel_mse_10keV[2]
          n_xz[i,k] = n_xz[i,k] + 1.
          
          f_yz_t[j,k] = f_yz_t[j,k] + total(Oflux_mse_10keV^2)^.5        
          f_yz_y[j,k] = f_yz_y[j,k] + Oflux_mse_10keV[1]
          f_yz_z[j,k] = f_yz_z[j,k] + Oflux_mse_10keV[2]
          v_yz_y[j,k] = v_yz_y[j,k] + Ovel_mse_10keV[1]
          v_yz_z[j,k] = v_yz_z[j,k] + Ovel_mse_10keV[2]
          n_yz[j,k] = n_yz[j,k] + 1.
          
         
         skip:
      endfor   
      no_data:
    endfor
    
    !p.multi=[0,2,1]
    idx = where(n_xz ne 0)
    f_xz_t2[idx] = f_xz_t[idx]/n_xz[idx]
    f_xz_x2[idx] = f_xz_x[idx]/n_xz[idx]
    f_xz_z2[idx] = f_xz_z[idx]/n_xz[idx]
    v_xz_x2[idx] = v_xz_x[idx]/n_xz[idx]
    v_xz_z2[idx] = v_xz_z[idx]/n_xz[idx]

    
    idx = where(n_yz ne 0)
    f_yz_t2[idx] = f_yz_t[idx]/n_yz[idx]
    f_yz_y2[idx] = f_yz_y[idx]/n_yz[idx]
    f_yz_z2[idx] = f_yz_z[idx]/n_yz[idx]
    v_yz_y2[idx] = v_yz_y[idx]/n_yz[idx]
    v_yz_z2[idx] = v_yz_z[idx]/n_yz[idx]
    
    
    plotxyz,each_axis,each_axis,f_xz_t2,mult='2,2',zrange=[0,1000],xrange=[4,0],yrange=[-4,4],tit='Flux',xtit='X',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/xz
    plotxyz,each_axis,each_axis,n_xz,/add,zrange=[0,100],xrange=[4,0],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/xz

    stop
    
    plotxyz,each_axis,each_axis,f_xz_x2,mult='2,2',zrange=[0,1000],xrange=[4,0],yrange=[-4,4],tit='Flux (X dir)',xtit='X',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/xz
    velovect,f_xz_x2,f_xz_z2,each_axis,each_axis,/over
    plotxyz,each_axis,each_axis,n_xz,/add,zrange=[0,100],xrange=[4,0],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/xz
    
    plotxyz,each_axis,each_axis,f_xz_z2,/add,zrange=[-1000,1000],xrange=[4,0],yrange=[-4,4],tit='Flux (Z dir)',xtit='X',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/xz
    velovect,f_xz_x,f_xz_z,each_axis,each_axis,/over
    plotxyz,each_axis,each_axis,n_xz,/add,zrange=[0,100],xrange=[4,0],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/xz
    
    stop
    
    
    plotxyz,each_axis,each_axis,f_yz_t2,mult='2,2',zrange=[0,1000],xrange=[-4,4],yrange=[-4,4],tit='Flux',xtit='Y',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/yz
    plotxyz,each_axis,each_axis,n_yz,/add,zrange=[0,100],xrange=[-4,4],yrange=[-4,4],xtit='Y',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/yz

    stop

    
    
    plotxyz,each_axis,each_axis,f_yz_y2,mult='2,2',zrange=[-1000,1000],xrange=[-4,4],yrange=[-4,4],tit='Flux (Y dir)',xtit='X',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/yz
    velovect,f_yz_y2,f_yz_z2,each_axis,each_axis,/over
    plotxyz,each_axis,each_axis,n_yz,/add,zrange=[0,100],xrange=[-4,4],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/yz

    plotxyz,each_axis,each_axis,f_yz_z2,/add,zrange=[-1000,1000],xrange=[-4,4],yrange=[-4,4],tit='Flux (Z dir)',xtit='X',ytit='Z',ztit='flux [cm-2s-1]'
    plot_bs_imb_mars,/edb,/oplot,/yz
    velovect,f_yz_y2,f_yz_z2,each_axis,each_axis,/over
    plotxyz,each_axis,each_axis,n_yz,/add,zrange=[0,100],xrange=[-4,4],yrange=[-4,4],xtit='X',ytit='Z',ztit='measurements [#]'
    plot_bs_imb_mars,/edb,/oplot,/yz
    
    stop


    plot_bs_imb_mars,/edb,/xz
    velovect,v_xz_x,v_xz_z,each_axis,each_axis,/over
    !p.multi=[0,2,1]

    plot_bs_imb_mars,/edb,/xz
    velovect,v_xz_x2,v_xz_z2,each_axis,each_axis,/over,length=2.
    
    plot_bs_imb_mars,/edb,/yz
    velovect,v_yz_y2,v_yz_z2,each_axis,each_axis,/over,length=2.
    
stop
end