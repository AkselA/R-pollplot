# This file will not be included in the build of the package,
# but objects created here will, via add_data(), be available
# from the 'data/' folder

alpha <- 0.8
ppcol <- c(Ap=hsv(0.971, 0.704, 0.990, alpha),
        Hoyre=hsv(0.578, 0.517, 0.941, alpha), 
          Frp=hsv(0.581, 0.900, 0.672, alpha), 
           SV=hsv(0.990, 0.990, 0.980, alpha), 
           Sp=hsv(0.244, 0.953, 0.518, alpha), 
          KrF=hsv(0.121, 1.000, 0.982, alpha), 
      Venstre=hsv(0.498, 1.000, 0.453, alpha), 
          MdG=hsv(0.220, 0.993, 0.788, alpha),   
         Rodt=hsv(0.010, 1.000, 0.691, alpha), 
        Andre=hsv(0.000, 0.000, 0.501, alpha))

rm(alpha)
