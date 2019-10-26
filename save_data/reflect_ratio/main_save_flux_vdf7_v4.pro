;+
; :Description:
;    This routine is a main routine to save inward/outward flux as well as local field info in each orbit
;
; ${parameters}
;
; ${keywords}
;
; ${Return values}
;
; ${Related routines}
;
; $Author: Kei Masunaga (@EPS, Univ. Tokyo)
;
; $Last modified July 15, 2016
;-



pro main_save_flux_vdf7_v4
   
;   orbit_arr = 2787 + indgen(600)
   orbit_arr = 2925 + indgen(100)
   rmv_orbit = [324,409,470,471,545,637,638,$
                911+indgen(404),1455+indgen(15),1695+indgen(13), 2840]
   ;orbit_spice_load = [372,408,628,665,$
   ;                    1480,1530,1580,1630,1680,1730,1770,1780,1830,1880,1930]
   for qq=0,n_elements(orbit_arr)-1 do begin
    ;idx_spice = where(orbit_arr[qq] eq orbit_spice_load)
    ;if idx_spice ne -1 then spice_load = 1
    idx = where(orbit_arr[qq] eq rmv_orbit)
    if idx[0] ne -1 then goto,skip_save
    save_flux_vdf7_v4,orbit_arr[qq],get_apid='d0',/save
    skip_save:
   endfor

end