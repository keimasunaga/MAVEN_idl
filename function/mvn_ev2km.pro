function mvn_eV2km,energy,mass

m = mass*1.67e-27
E = energy*1.6e-19
km = (2.*E/m)^0.5/1000.

return,km
end