#https://www5.kingcounty.gov/sdc/FGDCDocs/RPSALE_EXTR_fgdc.htm

propertyClass <- data.frame(c(0:9), c("Unknown", "C/I land only",
"C/I Improved property, no condo or mobile home", "C/I - Condominium",
"C/I Air rights only", "C/I - Improved property excluding air rights",
"C/I - Land or building, no split","Residential - Land only",
"Residential - Improved property","Residential or C/I Mobile Home"))

names(propertyClass) <- c("ID", "HumanReadablePropertyClass")

write.csv(propertyClass, "data/PROPERTYCLASS.csv")


propertyType <- data.frame(c(0:6), c("Unknown","Land only", "Land with new building",
                                     "Land with previously used building","Land with mobile home",
                                     "Timber only","Building only"))

names(propertyType) <- c("ID", "HumanReadablePropertyType")
write.csv(propertyType, "data/PROPERTYTYPE.csv")