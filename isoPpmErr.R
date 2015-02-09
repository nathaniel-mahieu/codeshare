# Array implementation of previous looping code.

isoPpmErr = function(mzs, mzdiff = 1.003355, charges = c(1,2,3,4,5,6)) {
  # Two m3 peak matrices, the first has peaks$mz increment along the z axis, the second has peaks$mz increment along the y axis
  #These conform to the m3 matrices created below and are used to calculate ppm
  # There has to be a better way to do this
  peaks.z.m3 = outer(
    outer(
      mzs, 
      rep(1, length(mzs)), "*"), 
    rep(1, length(charges)), "*"
  )
  
  peaks.y.m3 = outer(
    outer(
      rep(1, length(mzs)), 
      mzs, "*"), 
    rep(1, length(charges)), "*"
  )
  
  #Pairwise differences and carbon number calculation
  mzd.m2 = outer(mzs, mzs, "-")
  mzd.m2[lower.tri(mzd.m2, T)] = NA # Each pair is repeated, remove repeats in lower triangle including diag
  
  cn.m3 = outer(mzd.m2, charges, function(mzd, chg) {# Predicted isotope carbon number
    round(mzd * chg / mzdiff)
  })
  pmz.m3 = outer(mzd.m2, charges, function(mzd, chg) { # Predicted isotope mz based on base mass, charge, and number of carbons
    cn = round(mzd * chg / mzdiff)
    cn*mzdiff/chg
  }) + peaks.y.m3
  
  ppm.m3 = (pmz.m3 - peaks.z.m3) / pmz.m3 * 1E6
  
  # Which pairs exist within the mass error tolerance
  #isos = which(abs(ppm.m3) < ppm_for_isotopes, arr.ind=T)
  #dimnames(isos) = list(NULL, c("base", "iso", "charge"))
  
  list(
    cn.m3, 
    ppm.m3
  )
}
