# This file will not be included in the build of the package,
# but objects created here will, via add_data(), be available
# from the 'data/' folder

alpha <- 0.94
ppcol <- c(Ap=hsv(0.025, 0.840, 0.900, alpha),
        Hoyre=hsv(0.578, 0.517, 0.900, alpha), 
          Frp=hsv(0.600, 0.900, 0.520, alpha), 
           SV=hsv(0.940, 0.650, 1.000, alpha), 
           Sp=hsv(0.244, 0.980, 0.558, alpha), 
          KrF=hsv(0.121, 1.000, 0.982, alpha), 
      Venstre=hsv(0.498, 1.000, 0.490, alpha), 
          MdG=hsv(0.220, 1.000, 0.788, alpha),   
         Rodt=hsv(0.000, 1.000, 0.700, alpha), 
        Andre=hsv(0.000, 0.000, 0.500, alpha))
        

# general election results
loc <- Sys.getlocale("LC_CTYPE")
Sys.setlocale("LC_CTYPE", "C")

csvurl <- paste0("http://www.pollofpolls.no/lastned.csv?",
                 "tabell=valgresultater_historisk&type=riks")
rc <- read.csv2(csvurl, skip=2, stringsAsFactors=FALSE)

Sys.setlocale("LC_CTYPE", loc)

rc <- rc[grep("^[12]", rc[,1]),]

colnames(rc) <- c("year", "Ap", "Hoyre", "Frp", "SV", "Sp", "KrF", 
                  "Venstre", "MdG", "Rodt", "Andre")

gen.votes <- sub("\\s*\\([^\\)]+\\)", "", as.matrix(rc))
gen.votes <- sub(",", ".", gen.votes)
storage.mode(gen.votes) <- "numeric"

gen.mand <- apply(rc, 2, 
  function(x) {
      y <- regmatches(x, regexec("\\([0-9]*\\)", x))
      y[lengths(y) == 0] <- 0
      do.call(c, y)
  }
)

gen.mand <- gsub("[()]", "", as.matrix(gen.mand))
storage.mode(gen.mand) <- "numeric"

gen.mand[,1] <- as.integer(rc[,1])

rm(loc)
rm(alpha)
rm(csvurl)
rm(rc)
