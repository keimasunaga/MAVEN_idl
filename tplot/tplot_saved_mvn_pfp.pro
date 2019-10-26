  ;+
  ; :Description:
  ;    Purpose of this routine is to display (and save) a SAVED time series of pfp (plasma and field package) of single orbit
  ;
  ; ${parameters}
  ;  orbit: orbit number
  ;
  ; ${keywords}
  ;
  ; ${Related routines}
  ;  tplot_mvn_pfp_orbit
  ;
  ; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
  ;
  ; $Last modified May 13, 2014
  ;-


; sub routine

pro rename_temp,tvar_arr,num=num

  if not keyword_set(num) then num = 0
  for i=0,n_elements(tvar_arr)-1 do store_data,tvar_arr[i],newname=tvar_arr[i]+'_'+strcompress(string(num),/remove)

end

pro cmb_tvar,tvar_arr,num1,num2



   for j=0,n_elements(tvar_arr)-1 do begin
     
;     ;get_data,tvar_arr[j]+'_'+strcompress(string(num1),/remove),data1
;     ;get_data,tvar_arr[j]+'_'+strcompress(string(num2),/remove),data2
;     store_data,tvar_arr[j],data=[ tvar_arr[j]+'_'+strcompress(string(num1),/remove), tvar_arr[j]+'_'+strcompress(string(num2),/remove) ]
   
     get_data,tvar_arr[j]+'_'+strcompress(string(num1),/remove),data=data1
     get_data,tvar_arr[j]+'_'+strcompress(string(num2),/remove),data=data2
     if size(data1,/type) eq 3 or size(data2,/type) eq 3 then goto, nodata
     
     if tag_exist(data1,'v') eq 0 then begin
      store_data,tvar_arr[j],data={x:[data1.x,data2.x], y:[data1.y,data2.y]}
     endif else begin
      if (size(data1.v))[0] eq 2 then store_data,tvar_arr[j],data={x:[data1.x,data2.x], y:[data1.y,data2.y],v:[data1.v,data2.v]} $
                                 else store_data,tvar_arr[j],data={x:[data1.x,data2.x], y:[data1.y,data2.y],v:data1.v}
     endelse
     nodata:
   endfor
   
end

pro get_lims,tva_arr,num

 ; for k=0,n_elements(tvar_arr)-1 get_data,tvar_arr[k],data=data
 ; ylim,tvar_arr[k],

end


; main routine
pro tplot_saved_mvn_pfp,date=date,orbit=orbit,noplot=noplot,swi_flg_out=swi_flg_out

   del_data,'*'
   env = init_env()
   SAVE_LOC = env.SAVE_LOC
   
   
   
   if keyword_set(date) then begin
    
     filename = SAVE_LOC +'/maven/tplot_save/daily/tplot_pfp_'+date+'.tplot'
     tplot_restore,filename=filename 
     timespan,date,1,/day
     
     
     filename = SAVE_LOC + '/maven/sav/swia_flg/daily/swia_flg_'+date+'.sav'
     restore,filename
     idx_ca = swiflg.flg_ca
     idx_cs = swiflg.flg_cs

     ;;check if swica data has time period more than 4 hours
     if n_elements(idx_ca) eq 1 then begin
       if idx_ca eq 1 then begin
         swi_flg = 'swica'
         get_data,'mvn_swica_velocity_mso',data=v_temp
         ndat = n_elements(v_temp.x)
         tspan_swica = v_temp.x[ndat-1]-v_temp.x[0]
         if tspan_swica lt 4d*3600d then swi_flg = 'swics'
         if ndat lt 500 then swi_flg = 'swics'
       endif else begin
         swi_flg = 'swics'
       endelse
     endif else begin
       if idx_ca[0] eq 1 and idx_ca[1] eq 1 then begin
         swi_flg = 'swica'
         get_data,'mvn_swica_velocity_mso',data=v_temp
         ndat = n_elements(v_temp.x)
         tspan_swica = v_temp.x[ndat-1]-v_temp.x[0]
         if tspan_swica lt 4d*3600d then swi_flg = 'swics'
         if ndat lt 500 then swi_flg = 'swics'
       endif else begin
         swi_flg = 'swics'
       endelse

     endelse
     
     
     ;;merge vel_xyz and vel_total
     tplot_vec_tot,'mvn_swica_velocity_mso'
     store_data,'mvn_swica_velocity_mso_all',data=['mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot']
     tplot_vec_tot,'mvn_swics_velocity_mso'
     store_data,'mvn_swics_velocity_mso_all',data=['mvn_swics_velocity_mso','mvn_swics_velocity_mso_tot']
     
     ;;merge mag_xyz and mag_total
     tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
     store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
     
     store_data,'alt2',data= ['wind','sheath','pileup','wake']
     
     tplot_options,'var_label',['orbnum','lat','lon','Zmso','Ymso','Xmso']
     
     
     if ~keyword_set(noplot) then begin
       
       tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
              'mvn_swe_et_spec_svy',$
              'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
              'mvn_'+swi_flg+'_swica_en_eflux','mvn_'+swi_flg+'_density','mvn_'+swi_flg+'_velocity_mso_all','mvn_'+swi_flg+'_pdy',$
              'mvn_B_1sec_MAVEN_MSO_all',$
              'alt2','sza']
       ;tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
     endif
   endif




   if keyword_set(orbit) then begin
   
     
   if n_elements(orbit) eq 1 then begin
     tspan = mvn_orbit_num(orbnum=[orbit-0.5,orbit+0.5])
     tspan_strng = time_string(tspan)
     date_s = strmid(tspan_strng[0],0,4)+strmid(tspan_strng[0],5,2)+strmid(tspan_strng[0],8,2)
     date_e = strmid(tspan_strng[1],0,4)+strmid(tspan_strng[1],5,2)+strmid(tspan_strng[1],8,2)
     ;if date_s eq date_e then begin
        filename = SAVE_LOC +'/maven/tplot_save/orbit/tplot_pfp_'+string(orbit,format='(i05)')+'.tplot'
        tplot_restore,filename=filename
        timespan,tspan
        
        
        filename = SAVE_LOC + '/maven/sav/swia_flg/orbit/swia_flg_'+string(orbit,format='(i05)')+'.sav'
        restore,filename
        idx_ca = swiflg.flg_ca
        idx_cs = swiflg.flg_cs
        
        ;;check if swica data has time period more than 4 hours
        if n_elements(idx_ca) eq 1 then begin
          if idx_ca eq 1 then begin
            swi_flg = 'swica'
            get_data,'mvn_swica_velocity_mso',data=v_temp
            ndat = n_elements(v_temp.x)
            tspan_swica = v_temp.x[ndat-1]-v_temp.x[0]
            if tspan_swica lt 4d*3600d then swi_flg = 'swics'
            if ndat lt 500 then swi_flg = 'swics'     
          endif else begin
             swi_flg = 'swics'
          endelse
        endif else begin
          if idx_ca[0] eq 1 and idx_ca[1] eq 1 then begin
            swi_flg = 'swica'
            get_data,'mvn_swica_velocity_mso',data=v_temp
            ndat = n_elements(v_temp.x)
            tspan_swica = v_temp.x[ndat-1]-v_temp.x[0]
            if tspan_swica lt 4d*3600d then swi_flg = 'swics'
            if ndat lt 500 then swi_flg = 'swics'
          endif else begin
            swi_flg = 'swics'
          endelse
          
        endelse
          
        
        ;; combine v_xyz and vt
        tplot_vec_tot,'mvn_'+swi_flg+'_velocity_mso'
        store_data,'mvn_'+swi_flg+'_velocity_mso_all',data=['mvn_'+swi_flg+'_velocity_mso','mvn_'+swi_flg+'_velocity_mso_tot']
        
        ;; combine b_xyz and bt    
        tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
        store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
        
        store_data,'alt2',data= ['wind','sheath','pileup','wake']
     
        tplot_options,'var_label',['orbnum','lat','lon','Zmso','Ymso','Xmso']        
        
;        if orbit ge 316 and orbit lt 1050 then begin
;          sep_name_1 = 'MVN'
;          sep_name_2 = 'ION_EFLUX'
;        endif else begin
          sep_name_1 = 'mvn'
          sep_name_2 = 'ion_eflux'
;        endelse
       
        if ~keyword_set(noplot) then begin
          if n_elements(idx_ca) eq 1 then begin 
            if swi_flg eq 'swica' then $
            tplot,[sep_name_1+'_SEP1F_'+sep_name_2,sep_name_1+'_SEP1R_'+sep_name_2,sep_name_1+'_SEP2F_'+sep_name_2,sep_name_1+'_SEP2R_'+sep_name_2,$
                   'mvn_swe_et_spec_svy',$
                   'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
                   'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy',$
                   'mvn_B_1sec_MAVEN_MSO_all',$
                   'alt2','sza'] $
            else $
            tplot,[sep_name_1+'_SEP1F_'+sep_name_2,sep_name_1+'_SEP1R_'+sep_name_2,sep_name_1+'_SEP2F_'+sep_name_2,sep_name_1+'_SEP2R_'+sep_name_2,$
                   'mvn_swe_et_spec_svy',$
                   'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
                   'mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy',$
                   'mvn_B_1sec_MAVEN_MSO_all',$
                   'alt2','sza'] 
                   ;tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy','mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M','mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy','mvn_B_1sec_MAVEN_MSO_all','alt2','sza']
          endif else begin
            if swi_flg eq 'swica' then $
              tplot,[sep_name_1+'_SEP1F_'+sep_name_2,sep_name_1+'_SEP1R_'+sep_name_2,sep_name_1+'_SEP2F_'+sep_name_2,sep_name_1+'_SEP2R_'+sep_name_2,$
              'mvn_swe_et_spec_svy',$
              'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
              'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy',$
              'mvn_B_1sec_MAVEN_MSO_all',$
              'alt2','sza'] $
            else $
              tplot,[sep_name_1+'_SEP1F_'+sep_name_2,sep_name_1+'_SEP1R_'+sep_name_2,sep_name_1+'_SEP2F_'+sep_name_2,sep_name_1+'_SEP2R_'+sep_name_2,$
              'mvn_swe_et_spec_svy',$
              'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
              'mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy',$
              'mvn_B_1sec_MAVEN_MSO_all',$
              'alt2','sza']
          endelse
       endif
        
        
        
;     endif else begin
;      
;       filename0 = SAVE_LOC +'/maven/tplot_save/daily/tplot_pfp_'+string(orbit,format='(i05)')+'.tplot'
;       filename1 = SAVE_LOC +'/maven/tplot_save/daily/tplot_pfp_'+string(orbit,format='(i05)')+'.tplot'
;       stop
;       tplot_restore,filename=filename0
;       tspan = mvn_orbit_num(orbnum=[orbit-0.5,orbit+0.5])
;       timespan,tspan
;       
;       tplot_vec_tot,'mvn_swim_velocity_mso'
;       tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
;
;       rename_temp,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
;                    'mvn_swe_et_spec_svy',$
;                    'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
;                    'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
;                    'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
;                    'wind','sheath','pileup','wake','lat','lon','sza','Zmso','Ymso','Xmso'],num=1
;
;       tplot_restore,filename=filename1
;
;      rename_temp,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
;                    'mvn_swe_et_spec_svy',$
;                    'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
;                    'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
;                    'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
;                    'wind','sheath','pileup','wake','lat','lon','sza','Zmso','Ymso','Xmso'],num=2
;
;       cmb_tvar,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
;                 'mvn_swe_et_spec_svy',$
;                 'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
;                 'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
;         'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
;         'wind','sheath','pileup','wake','lat','lon','sza','Zmso','Ymso','Xmso'],1,2
;
;       store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
;
;
;       options,'sheath*',colors=4
;       options,'pileup*',colors=5
;       options,'wake*',colors=2
;       store_data,'alt2',data=['wind_1','sheath_1','pileup_1','wake_1','wind_2','sheath_2','pileup_2','wake_2']
;       tplot_options,'var_label',['orbnum','lat','lon','Zmso','Ymso','Xmso']
;
;
;       zlim,'mvn_sep*_Energy',10.,100000,1
;       ylim,'mvn_sep*_Energy',10.,100000,1
;       ylim,'mvn_sta_c0_E',1.,100000,1
;       ylim,'mvn_sta_c0_H_E',1.,100000,1
;       ylim,'mvn_sta_c6_M',1.,100,1
;       ylim,'mvn_swi*_en_eflux',1,30000
;       if ~keyword_set(noplot) then tplot,['mvn_sep1_B-O_Eflux_Energy', 'mvn_sep2_B-O_Eflux_Energy',$
;                 'mvn_swe_et_spec_svy',$
;                 'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
;                 'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy',$
;                 'mvn_B_1sec_MAVEN_MSO_all',$
;                 'alt2','sza']
;            
;     endelse
     
     
   endif

   


   if n_elements(orbit) eq 2 then begin
     tspan = mvn_orbit_num(orbnum=[orbit[0]-0.5,orbit[1]+0.5])
     timespan,tspan
     tspan_strng = time_string(tspan)
     date_s = strmid(tspan_strng[0],0,4)+strmid(tspan_strng[0],5,2)+strmid(tspan_strng[0],8,2)
     date_e = strmid(tspan_strng[1],0,4)+strmid(tspan_strng[1],5,2)+strmid(tspan_strng[1],8,2)
     filename0 = SAVE_LOC +'/maven/tplot_save/orbit/tplot_pfp_'+string(orbit[0],format='(i05)')+'.tplot'
     filename1 = SAVE_LOC +'/maven/tplot_save/orbit/tplot_pfp_'+string(orbit[1],format='(i05)')+'.tplot'
        
;     if avg(orbit) ge 400 and avg(orbit) lt 1050 then begin
;       sep_name_1 = 'MVN'
;       sep_name_2 = 'ION_EFLUX'
;     endif else begin
       sep_name_1 = 'mvn'
       sep_name_2 = 'ion_eflux'
;     endelse
     
     tplot_restore,filename=filename0
    
     tplot_vec_tot,'mvn_swica_velocity_mso'
     tplot_vec_tot,'mvn_swics_velocity_mso'
     tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
   
     rename_temp,[sep_name_1+'_SEP1F_'+sep_name_2,sep_name_1+'_SEP1R_'+sep_name_2,sep_name_1+'_SEP2F_'+sep_name_2,sep_name_1+'_SEP2R_'+sep_name_2,$
                  'mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
                  'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
                  'mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso','mvn_swics_velocity_mso_tot','mvn_swics_pdy','mvn_swics_temperature','mvn_swics_pressure',$
                  'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
                  'wind','sheath','pileup','wake','lat','lon','sza','Zmso','Ymso','Xmso'],num=1




                file_swiflg_1 = SAVE_LOC + '/maven/sav/swia_flg/orbit/swia_flg_'+string(orbit[0],format='(i05)')+'.sav'
                restore,file_swiflg_1
                idx_ca_1 = swiflg.flg_ca
                idx_cs_1 = swiflg.flg_cs


                ;;check if swica data has time period more than 4 hours
                if n_elements(idx_ca_1) eq 1 then begin
                  if idx_ca_1 eq 1 then begin
                    swi_flg_1 = 'swica'
                    get_data,'mvn_swica_velocity_mso_1',data=v_temp
                    ndat = n_elements(v_temp.x)
                    tspan_swica = v_temp.x[ndat-1]-v_temp.x[0]
                    if tspan_swica lt 4d*3600d then swi_flg_1 = 'swics'
                    if ndat lt 500 then swi_flg_1 = 'swics'
                  endif else begin
                    swi_flg_1 = 'swics'
                  endelse
                endif else begin
                  if idx_ca_1[0] eq 1 and idx_ca_1[1] eq 1 then begin
                    swi_flg_1 = 'swica'
                    get_data,'mvn_swica_velocity_mso_1',data=v_temp
                    ndat = n_elements(v_temp.x)
                    tspan_swica = v_temp.x[ndat-1]-v_temp.x[0]
                    if tspan_swica lt 4d*3600d then swi_flg_1 = 'swics'
                    if ndat lt 500 then swi_flg_1 = 'swics'
                  endif else begin
                    swi_flg_1 = 'swics'
                  endelse

                endelse
     
               
     
     tplot_restore,filename=filename1
            
     
     tplot_vec_tot,'mvn_swica_velocity_mso'
     tplot_vec_tot,'mvn_swics_velocity_mso'
     tplot_vec_tot,'mvn_B_1sec_MAVEN_MSO'
    
  
     rename_temp,[sep_name_1+'_SEP1F_'+sep_name_2,sep_name_1+'_SEP1R_'+sep_name_2,sep_name_1+'_SEP2F_'+sep_name_2,sep_name_1+'_SEP2R_'+sep_name_2,$
                  'mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
                  'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
                  'mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso','mvn_swics_velocity_mso_tot','mvn_swics_pdy','mvn_swics_temperature','mvn_swics_pressure',$
                  'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
                  'wind','sheath','pileup','wake','lat','lon','sza','Zmso','Ymso','Xmso'],num=2
  
                file_swiflg_2 = SAVE_LOC + '/maven/sav/swia_flg/orbit/swia_flg_'+string(orbit[1],format='(i05)')+'.sav'
                restore,file_swiflg_2
                idx_ca_2 = swiflg.flg_ca
                idx_cs_2 = swiflg.flg_cs


                ;;check if swica data has time period more than 4 hours
                if n_elements(idx_ca_2) eq 1 then begin
                  if idx_ca_2 eq 1 then begin
                    swi_flg_2 = 'swica'
                    get_data,'mvn_swica_velocity_mso_2',data=v_temp
                    ndat = n_elements(v_temp.x)
                    tspan_swica = v_temp.x[ndat-1]-v_temp.x[0]
                    if tspan_swica lt 4d*3600d then swi_flg_2 = 'swics'
                    if ndat lt 500 then swi_flg_2 = 'swics'
                  endif else begin
                    swi_flg_2 = 'swics'
                  endelse
                endif else begin
                  if idx_ca_2[0] eq 1 and idx_ca_2[1] eq 1 then begin
                    swi_flg_2 = 'swica'
                    get_data,'mvn_swica_velocity_mso_2',data=v_temp
                    ndat = n_elements(v_temp.x)
                    tspan_swica = v_temp.x[ndat-1]-v_temp.x[0]
                    if tspan_swica lt 4d*3600d then swi_flg_2 = 'swics'
                    if ndat lt 500 then swi_flg_2 = 'swics'
                  endif else begin
                    swi_flg_2 = 'swics'
                  endelse

                endelse

                swi_flg = [swi_flg_1,swi_flg_2] 
                if swi_flg[0] eq 'swica' and swi_flg[1] eq 'swica' then swi_flg = 'swica' else swi_flg = 'swics'
             
     cmb_tvar,[sep_name_1+'_SEP1F_'+sep_name_2,sep_name_1+'_SEP1R_'+sep_name_2,sep_name_1+'_SEP2F_'+sep_name_2,sep_name_1+'_SEP2R_'+sep_name_2,$
               'mvn_swe_et_spec_svy','mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
               'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot','mvn_swica_pdy','mvn_swica_temperature','mvn_swica_pressure',$
               'mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso','mvn_swics_velocity_mso_tot','mvn_swics_pdy','mvn_swics_temperature','mvn_swics_pressure',$
               'mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot',$
               'wind','sheath','pileup','wake','lat','lon','sza','Zmso','Ymso','Xmso'],1,2    
     
     store_data,'mvn_B_1sec_MAVEN_MSO_all',data=['mvn_B_1sec_MAVEN_MSO','mvn_B_1sec_MAVEN_MSO_tot']
     store_data,'mvn_swica_velocity_mso_all',data=['mvn_swica_velocity_mso','mvn_swica_velocity_mso_tot']
     store_data,'mvn_swics_velocity_mso_all',data=['mvn_swics_velocity_mso','mvn_swics_velocity_mso_tot']
     
     options,'sheath_*',colors=4
     options,'pileup_*',colors=5
     options,'wake_*',colors=2
     store_data,'alt2',data=['wind_1','sheath_1','pileup_1','wake_1','wind_2','sheath_2','pileup_2','wake_2']
     tplot_options,'var_label',['orbnum','lat','lon']
     options,[sep_name_1+'_SEP*'+sep_name_2,'mvn_swi*_en_eflux','mvn_swe_et_spec_svy','mvn_sta_c*','mvn_sta_d*'],'spec',1
     options,['mvn_swi*_velocity_mso','mvn_B_1sec_MAVEN_MSO'],'colors',[80,120,230]
     
     ylim,sep_name_1+'_SEP*'+sep_name_2,10.,7000,1
     zlim,sep_name_1+'_SEP*'+sep_name_2,1.,100000,1
     ylim,'mvn_swe_et_spec_svy',10,10000,1
     zlim,'mvn_swe_et_spec_svy',1,1e,1
     ylim,'mvn_sta_c0_E',0.1,35000,1
     zlim,'mvn_sta_c0_E',1e3,1e9,1
     ylim,'mvn_sta_c0_H_E',0.1,35000,1
     zlim,'mvn_sta_c0_H_E',1e3,1e9,1
     ylim,'mvn_sta_c6_M',1.,100,1
     zlim,'mvn_sta_c6_M',1e3,1e9,1
     ylim,'mvn_swi*_en_eflux',1,30000,1
     zlim,'mvn_swi*_en_eflux',1e4,1e8,1
     
     
     if ~keyword_set(noplot) then begin
;     tplot,['MVN_SEP1F_ION_EFLUX','MVN_SEP1R_ION_EFLUX','MVN_SEP2F_ION_EFLUX','MVN_SEP2R_ION_EFLUX',$
;            'mvn_swe_et_spec_svy',$
;            'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
;            'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso','mvn_swica_pdy',$
;            'mvn_B_1sec_MAVEN_MSO','alt2','sza']
     
     
          if swi_flg eq 'swica' then $
            tplot,[sep_name_1+'_SEP1F_'+sep_name_2,sep_name_1+'_SEP1R_'+sep_name_2,sep_name_1+'_SEP2F_'+sep_name_2,sep_name_1+'_SEP2R_'+sep_name_2,$
            'mvn_swe_et_spec_svy',$
            'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
            'mvn_swica_en_eflux','mvn_swica_density','mvn_swica_velocity_mso_all','mvn_swica_pdy',$
            'mvn_B_1sec_MAVEN_MSO_all',$
            'alt2','sza'] $
          else $
            tplot,[sep_name_1+'_SEP1F_'+sep_name_2,sep_name_1+'_SEP1R_'+sep_name_2,sep_name_1+'_SEP2F_'+sep_name_2,sep_name_1+'_SEP2R_'+sep_name_2,$
            'mvn_swe_et_spec_svy',$
            'mvn_sta_c0_E','mvn_sta_c0_H_E','mvn_sta_c6_M',$
            'mvn_swics_en_eflux','mvn_swics_density','mvn_swics_velocity_mso_all','mvn_swics_pdy',$
            'mvn_B_1sec_MAVEN_MSO_all',$
            'alt2','sza']
        
     
     
     endif
     
   endif
   endif
   
   swi_flg_out = swi_flg
end