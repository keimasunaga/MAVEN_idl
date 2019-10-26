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
; $Last modified Jan 18, 2017
;-



pro main_save_flux_vdf7_v6
   
  env = init_env()   ;;temporal
  SAVE_LOC = env.SAVE_LOC   ;;temporal
      
;   orbit_arr = 2787 + indgen(600)
   orbit_arr = 317 + indgen(2700)
   rmv_orbit = [324,409,455,470,471,545,628,634,637,638,$
                898+indgen(7),911+indgen(404),1392+indgen(9),1408,1455+indgen(15),1695+indgen(13), 1975, 2840]
   ;orbit_spice_load = [372,408,628,665,$
   ;                    1480,1530,1580,1630,1680,1730,1770,1780,1830,1880,1930]
   num = 0.
   for qq=0,n_elements(orbit_arr)-1 do begin
    
     
    
    ;idx_spice = where(orbit_arr[qq] eq orbit_spice_load)
    ;if idx_spice ne -1 then spice_load = 1
    idx = where(orbit_arr[qq] eq rmv_orbit)
    if idx[0] ne -1 then goto,skip_save
    fn_swi = SAVE_LOC + '/maven/sav/swia_flg/orbit/swia_flg_'+string(round(orbit_arr[qq]),format='(i05)')+'.sav'
    restore,fn_swi
    flg_ca = swiflg.flg_ca
    flg_cs = swiflg.flg_cs
    ;if n_elements(flg_ca) eq 2 then begin   ;;temporal
    ;if flg_ca[0] eq 1 and flg_ca[1] eq 0 then begin   ;;temporal
    num = num + 1

    save_flux_vdf7_v6,orbit_arr[qq],get_apid='d0',/save
    skip_save:
    ;endif
    ;endif
   endfor

end