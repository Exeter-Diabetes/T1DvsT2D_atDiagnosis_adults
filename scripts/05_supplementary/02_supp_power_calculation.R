#install.packages("pmsampsize")
library(pmsampsize)
#Clinical features only model
pmsampsize(type="b",
           cstatistic=0.85,
           parameters=9,
           prevalence=0.25)
#Clinical features only model +grs
pmsampsize(type="b",
           cstatistic=0.85,
           parameters=10,
           prevalence=0.25)
#Clinical features only model + antibodies
pmsampsize(type="b",
           cstatistic=0.85,
           parameters=10,
           prevalence=0.25)
#Clinical features only model + antibodies + grs
pmsampsize(type="b",
           cstatistic=0.8,
           parameters=15,
           prevalence=0.25)

#######################################################################################
#Full potential model
#Clinical features only model + antibodies + grs
pmsampsize(type="b",
           cstatistic=0.8,
           parameters=19,
           prevalence=0.25)
