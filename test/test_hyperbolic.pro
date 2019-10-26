pro test_hyperbolic


   theta_arr = findgen(103)*!DTOR
   L = 2.1
   e = 1.05
   X0 = 0.55
   
   r = L/(1.+e*cos(theta_arr))
   
 
   
   A = L*cos(theta_arr)/(1.+e*cos(theta_arr))
   B = L*sin(theta_arr)/(1.+e*cos(theta_arr))
   
   theta_dash_arr = acos((X0 + A)/sqrt((X0+A)^2+B^2))

   r_dash = (X0 + r*cos(theta_arr))/cos(theta_dash_arr)


   theta_dash_bin = findgen(19)*5*!DTOR
   r_dash_bin = interpol(r_dash,theta_dash_arr,theta_dash_bin)

   plot,theta_arr*!RADEG,r
   oplot,theta_dash_arr*!RADEG,r_dash,psym=2
   oplot,theta_dash_bin*!RADEG,r_dash_bin,color=230,psym=1
   stop
   plot,theta_arr*!RADEG,/iso
   oplot,theta_dash_arr*!RADEG,psym=2
   stop
   stop
end