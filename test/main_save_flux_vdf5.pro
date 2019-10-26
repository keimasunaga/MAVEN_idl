pro main_save_flux_vdf5
   
   orbit_arr = 814 + indgen(200)
   rmv_orbit = [409,470,471,545,637,638]
   orbit_spice_load = [628,665]
   for qq=0,n_elements(orbit_arr)-1 do begin
    idx_spice = where(orbit_arr[qq] eq orbit_spice_load)
    if idx_spice ne -1 then spice_load = 1
    idx = where(orbit_arr[qq] eq rmv_orbit)
    if idx[0] ne -1 then goto,skip_save
    save_flux_vdf5,orbit_arr[qq],get_apid='d0',/save,spice_load=spice_load
    skip_save:
   endfor



end