;+
; :Description:
;    This routine is a main routine of save_mvn_sw_info.
;    saves time of the bow shock crossings and
;    correspoinding solar wind parameters averaged for 15 minutes from the shock boundaries
;
; ${parameters}
;
; ${keywords}
; orbit: orbit number
; apend_tvar: set when plotting two continuous orbits
; swi_flg_manual: set when manually difine swia flgs. Swia flgs are values to judge if both swica and swics data or either of them exists
;
; ${Return values}
;
; ${Related routines}
; main_save_mvn_sw_info
;
; $Author: Kei Masunaga (@EPS, Univ. Tokyo)
;
; $Last modified May 16, 2016
;-



pro main_save_mvn_sw_info,orbit,norb=norb,swi_flg_manual=swi_flg_manual,multorb=multorb,backward=backward

   if not keyword_set(norb) then ndays=1
   
   for ii=0,norb-1 do begin
     if ii ne 0 then orbit = orbit + 1
     if not keyword_set(multorb) then begin
       tplot_saved_mvn_pfp,orbit=orbit,swi_flg_out=swi_flg_out,/noplot
       ylim,'mvn_B_1sec_MAVEN_MSO_all',0,30
       ylim,'mvn_B_1sec_MAVEN_MSO_tot',0,30
       ;tplot
       tplot,['mvn_swe_et_spec_svy','mvn_'+swi_flg_out+'_en_eflux','mvn_'+swi_flg_out+'_velocity_mso','mvn_'+swi_flg_out+'_density','mvn_B_1sec_MAVEN_MSO_tot','alt2']
       save_mvn_sw_info,orbit=orbit,swi_flg_manual=swi_flg_out
     endif else begin
       if not keyword_set(backward) then tplot_saved_mvn_pfp,orbit=[orbit,orbit+1],swi_flg_out=swi_flg_out,/noplot $
       else tplot_saved_mvn_pfp,orbit=[orbit-1,orbit],swi_flg_out=swi_flg_out,/noplot
       ylim,'mvn_B_1sec_MAVEN_MSO_all',0,30
       ylim,'mvn_B_1sec_MAVEN_MSO_tot',0,30
       ;tplot
       tplot,['mvn_swe_et_spec_svy','mvn_'+swi_flg_out+'_en_eflux','mvn_'+swi_flg_out+'_velocity_mso','mvn_'+swi_flg_out+'_density','mvn_B_1sec_MAVEN_MSO_tot','alt2']
       save_mvn_sw_info,orbit=orbit,swi_flg_manual=swi_flg_out
     endelse    
   endfor

end