pro tplot_saved_mvn_flux

    env = init_env()
    SAVE_LOC = env.SAVE_LOC
    
    filepath1 = SAVE_LOC + '/maven/sav/flux/deflect_ion/d0/00346/*.sav'
    filename1 = file_search(filepath1)
    filepath2 = SAVE_LOC + '/maven/sav/flux/deflect_ion/d0/00347/*.sav'
    filename2 = file_search(filepath2)
    filepath3 = SAVE_LOC + '/maven/sav/flux/deflect_ion/d0/00348/*.sav'
    filename3 = file_search(filepath3)
    filepath4 = SAVE_LOC + '/maven/sav/flux/deflect_ion/d0/00349/*.sav'
    filename4 = file_search(filepath4)
    filepath5 = SAVE_LOC + '/maven/sav/flux/deflect_ion/d0/00350/*.sav'
    filename5 = file_search(filepath5)
    
    filename = [filename1,filename2,filename3,filename4,filename5]
    
    time_arr = 0d
    Odens_arr = 0d
    Ovel_arr = [0d,0d,0d]
    Oflux_arr = [0d,0d,0d]
    
    for i=0,n_elements(filename)-1 do begin
      restore,filename[i]
      mtime = avg(dat_sav.time)
      time_arr = [time_arr,mtime]
      Odens = dat_sav.Odens_5keV_in
      Ovel = dat_sav.Ovel_mso_5keV_in
      Odens_arr = [Odens_arr,Odens]
      Ovel_arr = [[Ovel_arr],[Ovel]]
      Oflux_arr = [[Oflux_arr],[Odens*Ovel*1e5]]
      
      
    endfor

    time_arr = time_arr[1:*]
    Odens_arr = Odens_arr[1:*]
    Ovel_arr = transpose(Ovel_arr[*,1:*])
    Oflux_arr = transpose(Oflux_arr[*,1:*])

    store_data,'Odens',data={x:time_arr,y:Odens_arr}
    store_data,'Ovel',data={x:time_arr,y:Ovel_arr}
    store_data,'Oflux',data={x:time_arr,y:-Oflux_arr[*,0]}
    ;options,['Ovel','Oflux'],colors=[80,120,230]
    ylim,'Oflux',10,1e7,1
    tplot,['Odens','Ovel','Oflux']
end