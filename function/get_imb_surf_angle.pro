;+
; :Description:
;   This function returns tangential angles on the IMB points.
;   These points are the intersection points of the IMB and the line along the E field from the SC position.
;   The return angles are seen in the Vexb - Ve plane  
;
; ${parameters}
;   pos_mso: position of the spacecraft in the MSO frame
;   Vmso: proton velocity vector in the MSO frame
;   Bmso: magnetic field vector in the MSO frame
;
; ${keywords}
;  foot_point: foot point of the electric field on the BS/IMB in the MSO frame
; 
; ${return value}
;  angle from the Vexb plane to the Ve plane
;
; ${Related routines}
;
; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
;
; $Last modified Jul 11, 2015
;-

function get_imb_surf_angle,pos_mso,Vmso,Bmso,bs=bs,imb=imb,foot_point=foot_point
    
     
    ;; BS equation is taken from Edberg et al. [2008] 
    if keyword_set(bs) then begin
      e = 1.05
      L = 2.1
      X0 = 0.55
    endif
    
    ;; IMB equation is taken from Edberg et al. [2008]
    if keyword_set(imb) then begin
      e = 0.92
      L = 0.90
      X0 = 0.86
    endif
    

    Emso = -crossp(Vmso,Bmso)
    ExBmso = crossp(Emso,Bmso)
    
    bvec = Bmso/total(Bmso^2)^.5
    vvec = ExBmso/total(ExBmso^2)^.5
    evec = Emso/total(Emso^2)^.5
   
    mtx = fltarr(3,3)
    mtx[0,*] = bvec
    mtx[1,*] = vvec
    mtx[2,*] = evec    
   
    pos_bve = mtx # (pos_mso - [X0, 0.,0.])
   
    A0 = ((1-e^2)*bvec[0]^2+bvec[1]^2+bvec[2]^2)
    B0 = ((1-e^2)*vvec[0]^2+vvec[1]^2+vvec[2]^2)
    C0 = ((1-e^2)*evec[0]^2+evec[1]^2+evec[2]^2)
    D0 = 2 * ((1-e^2)*bvec[0]*vvec[0]+bvec[1]*vvec[1]+bvec[2]*vvec[2])
    E0 = 2 * ((1-e^2)*vvec[0]*evec[0]+vvec[1]*evec[1]+vvec[2]*evec[2])
    F0 = 2 * ((1-e^2)*evec[0]*bvec[0]+evec[1]*bvec[1]+evec[2]*bvec[2])
    G0 = 2*L*e*bvec[0] 
    H0 = 2*L*e*vvec[0]
    I0 = 2*L*e*evec[0]
    J0 = -L^2
    
    if keyword_set(bs) then begin
      A0 = -A0
      B0 = -B0
      C0 = -C0
      D0 = -D0
      E0 = -E0
      F0 = -F0
      G0 = -G0
      H0 = -H0
      I0 = -I0
      J0 = -J0
    endif
    
    S0 = C0
    T0 = E0*pos_bve[1]+F0*pos_bve[0] + I0
    U0 = A0*pos_bve[0]^2 + B0*pos_bve[1]^2 + D0*pos_bve[0]*pos_bve[1] + G0*pos_bve[0] + H0*pos_bve[1] + J0
    
    
    Zqbve = [ (-T0 + sqrt(T0^2-4*S0*U0))/2/S0, (-T0 - sqrt(T0^2-4*S0*U0))/2/S0 ]
    
    foot_point = [ [transpose(mtx ## [[pos_bve[0],pos_bve[0],Zqbve[0]]] + [X0, 0.,0.])], [transpose(mtx ## [[pos_bve[0],pos_bve[0],Zqbve[1]]] + [X0, 0.,0.])] ]
    slope_surf = (-D0*pos_bve[0]-2*B0*pos_bve[1]-E0*Zqbve-H0)/(2*C0*Zqbve+E0*pos_bve[1]+F0*pos_bve[0]+I0)
    surf_angle = atan(slope_surf)
    
    return,surf_angle
   
end