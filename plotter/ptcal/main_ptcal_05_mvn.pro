pro main_ptcal_05_mvn

;    Rm = 3389.9d0*1.d3
;    e = 1.05d0
;    L = 2.1d0
;    X0 = 0.55d0
;    xbs = 0.1d*(dindgen(30)-15.d)
;    zbs_p = sqrt( (e^2d -1d)*(xbs-X0+L*e/(1d -e^2d))^2d -L^2/(e^2-1d) )
;    zbs_m = -sqrt( (e^2d -1d)*(xbs-X0+L*e/(1d -e^2d))^2d -L^2/(e^2-1d) )
;    xbs0 = 1.57
;    zbs0 = sqrt( (e^2d -1d)*(xbs0-X0+L*e/(1d -e^2d))^2d -L^2/(e^2-1d) )
;    zbs_p_dx = (e^2-1.d0)*(xbs-X0+L*e/(1d -e^2))/sqrt( (e^2-1d)*(xbs-X0+L*e/(1d -e^2))^2-L^2/(e^2-1d) )
;    zbs_m_dx = -(e^2-1.d0)*(xbs-X0+L*e/(1d -e^2))/sqrt( (e^2-1d)*(xbs-X0+L*e/(1d -e^2))^2-L^2/(e^2-1d) )
;    
;    
;    a = -1.d/zbs_m_dx
;    b = -1.d/zbs_p_dx
;    theta_m = atan(a)*!radeg
;    theta_p = atan(b)*!radeg
;    
    r0 = [[findgen(10)*(-1000.d3)],[fltarr(10)],[replicate(3000.d3,10)]]   ;initial location
;    ene0 = 10000d  ;[eV] 
;    azv0 = 0d
;    elev0 = [theta_m,0d,theta_p]/ !radeg
    
   
    for qq=0,9 do begin
    if qq eq 0 then ptcal_05_mvn,r0[qq,*],/only_vdf,color=qq*30.
    if qq gt 0 then ptcal_05_mvn,r0[qq,*],/only_vdf,color=qq*30.,/oplot

    endfor

end