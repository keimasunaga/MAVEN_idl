function mso2mse,x_mso,y_mso,z_mso,angle
  ;+
  ; :Description:
  ;   This function converts any measurements in the MSO frame into the MSE frame.
  ;   Rotaion is conducted in the YZ plane.
  ;
  ; ${parameters}
  ;  x_mso, y_mso, z_mso: x, y, and z values in the MSO frame.
  ;    The type of these input data should be 1 dimension. 
  ;    For example, if x_mso is array[1,200], reduce the dimension as reform(x_mso)    
  ;  
  ;  angle: rotation angle (Radian)
  ;  
  ; ${keywords}
  ;
  ; ${Related routines}
  ;
  ; :Author: Kei Masunaga (@STEL, Nagoya Univ.)
  ;
  ; $Last modified May 12, 2015
  ;-  
  x_mse = x_mso
  y_mse = cos(angle)*y_mso -sin(angle)*z_mso
  z_mse = sin(angle)*y_mso +cos(angle)*z_mso
  mse = [[x_mse],[y_mse],[z_mse]]
  
  return,mse
end