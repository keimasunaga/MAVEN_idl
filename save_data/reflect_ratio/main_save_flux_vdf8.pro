pro main_save_flux_vdf8
   
   orbit_arr = 1946 + indgen(20)
   rmv_orbit = [372,373,409,470,471,545,637,638,$
                1695+indgen(13)]
   orbit_spice_load = [372,408,628,665,$
                       1480,1530,1580,1630,1680,1730,1770,1780,1830,1880,1930]
   for qq=0,n_elements(orbit_arr)-1 do begin
    idx_spice = where(orbit_arr[qq] eq orbit_spice_load)
    if idx_spice ne -1 then spice_load = 1
    idx = where(orbit_arr[qq] eq rmv_orbit)
    if idx[0] ne -1 then goto,skip_save
    save_flux_vdf8,orbit_arr[qq],get_apid='d1',/highres,/save,spice_load=spice_load;,/plot_vdf
    skip_save:
   endfor

end