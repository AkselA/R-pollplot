#' @export

ppcol <- function(...) {
	ppcol0 <- c(Ap=hsv(0.025, 0.840, 0.900, 0.94),
	         Hoyre=hsv(0.578, 0.517, 0.900, 0.94), 
	           Frp=hsv(0.600, 0.900, 0.520, 0.94), 
	            SV=hsv(0.940, 0.650, 1.000, 0.94), 
	            Sp=hsv(0.244, 0.980, 0.558, 0.94), 
	           KrF=hsv(0.121, 1.000, 0.982, 0.94), 
	       Venstre=hsv(0.498, 1.000, 0.490, 0.94), 
	           MdG=hsv(0.220, 1.000, 0.788, 0.94),   
	          Rodt=hsv(0.000, 1.000, 0.700, 0.94), 
	         Andre=hsv(0.000, 0.000, 0.500, 0.94))
	if (...length() != 0) {
		ppcol1 <- adjustcolor(ppcol0, ...)
		names(ppcol1) <- names(ppcol0)
		ppcol1
	} else {
		ppcol0
	}
}
