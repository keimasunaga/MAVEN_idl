function mvn_judge_near_bs,x,y,z,km=km

  ; parameters of Bow Shock (Hyperbola)
  X0_bs = 0.55
  e_bs = 1.05
  L_bs = 2.1
  Ryz = sqrt(y^2+z^2)
 
  if (e_bs^2-1.)*(x-X0_bs+L_bs*e_bs/(1-e_bs^2))^2+Ryz^2 lt L_bs^2/(e_bs^2-1.) and $
      1.1*(e_bs^2-1.)*(x-X0_bs+L_bs*e_bs/(1-e_bs^2))^2+Ryz^2 ge L_bs^2/(e_bs^2-1.) then ok=1 else ok=0

  
  return,ok

end

function mvn_judge_region,x,y,z,km=km
  
  if keyword_set(km) then begin
    R_m = 3389.9D
    x = x/R_m & y = y/R_m & z = z/R_m
  endif
  
  ; parameters of Bow Shock (Hyperbola)
  X0_bs = 0.55
  e_bs = 1.05
  L_bs = 2.1
  
  ; paremeters of imb (ellipse)
  X0_imb = 0.86
  e_imb = 0.92
  L_imb = 0.9

  Ryz = sqrt(y^2+z^2)
  
  if (e_bs^2-1.)*((x-X0_bs)+L_bs*e_bs/(1-e_bs^2))^2-Ryz^2 lt L_bs^2/(e_bs^2-1.) then region = 'sw'
  if (e_bs^2-1.)*((x-X0_bs)+L_bs*e_bs/(1-e_bs^2))^2-Ryz^2 ge L_bs^2/(e_bs^2-1.) and (1-e_imb^2)*(x-X0_imb+L_imb*e_imb/(1-e_imb^2))^2+Ryz^2 gt L_imb^2/(1.-e_imb^2) then region = 'ms'
  if  (1-e_imb^2)*(x-x0_imb+L_imb*e_imb/(1-e_imb^2))^2+Ryz^2 le L_imb^2/(1.-e_imb^2) then region = 'im'
  
  return,region

end