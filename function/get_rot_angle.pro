function get_rot_angle,V,B

    E = crossp(B,V)
    if E[1] lt 0 then clock = acos(E[2]/sqrt(E[1]^2+E[2]^2)) else clock = -acos(E[2]/sqrt(E[1]^2+E[2]^2))
    rot = -clock
    
    return,rot
    
end