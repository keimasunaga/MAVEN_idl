;+
; :Description:
;    This routine converts vector(s) in the MSO frame into the BVE frame
;    B, V, and E denote magnetic field vector, velocity vector, and convection electric field vector, respectively 
;
; ${parameters}
;  vec: vector(s) in MSO frame
;  V: a velocity vector in the MSO frame
;  B: a magnetic field vector in the MSO frame
;  
; ${keywords}
;
; ${Related routines}
;
; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
;
; $Last modified May 20, 2015
;-


function mso2bve,vec,V,B

E = -crossp(V,B)
Etot = sqrt(E[0]^2+E[1]^2+E[2]^2)
Vtot = sqrt(V[0]^2+V[1]^2+V[2]^2)
Btot = sqrt(B[0]^2+B[1]^2+B[2]^2)

ExB = crossp(E,B)
ExBtot = sqrt(ExB[0]^2 + ExB[1]^2 + ExB[2]^2)

vec_E = E/Etot
vec_B = B/Btot
vec_ExB = ExB/ExBtot

mtx = fltarr(3,3)
mtx[0,0] = vec_B[0] & mtx[1,0] = vec_B[1] & mtx[2,0] = vec_B[2]
mtx[0,1] = vec_ExB[0] & mtx[1,1] = vec_ExB[1] & mtx[2,1] = vec_ExB[2]
mtx[0,2] = vec_E[0] & mtx[1,2] = vec_E[1] & mtx[2,2] = vec_E[2]

Vbve = transpose( mtx ## vec )

return,Vbve


end

