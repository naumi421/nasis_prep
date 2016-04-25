### Data manipulation script for NASIS pedon data prep for contiguous United States
library(stringdist)
library(rgdal)

## Subsetting and Joining tables
setwd("D:/GIS_Archive/NRCS_pedons/NASIS_Pedons_20160301")
pedtab = read.delim("dbo_pedon_tab.txt", header=TRUE)
sitetab = read.delim("dbo_site_tab.txt", header=TRUE)
seriestab = read.delim("dbo_soilseries_tab.txt", header=TRUE)
pscs = read.delim("pscs_lup_tab.txt", header=TRUE)
gglup = read.delim("greatgrp_lup_tab.txt", header=TRUE)
subgrplup = read.delim("subgrp_lup_tab.txt", header=TRUE)

## join tables, can also use 'merge' command, but it was problematic
nasispedons = cbind(sitetab[match(pedtab$upedonid, sitetab$usiteid),], pedtab)

## Subset to get lats higher than 25 N to allow for better use of decimal lengths for precision
## Code will still work for other latitudes, but might exclude some sites unnecessarily
## by using the nchar of 7 sig digits to weed out coordinates with low precision
nasispedons = subset(nasispedons, latstddecimaldegrees > 25 | latdegrees > 25)


## Select most detailed Geo coordinates
# Precision of decimal lat/long fields
nasispedons$latdeclen = nchar(abs(nasispedons$latstddecimaldegrees))
nasispedons$longdeclen = nchar(abs(nasispedons$longstddecimaldegrees))
#nasispedons$decyxlendiff = nasispedons$latdeclen - nasispedons$longdeclen

## Start filling new x & y fields with best data ####NEED to fix
nasispedons$ywgs84 = ifelse(nasispedons$latdeclen > 7, nasispedons$latstddecimaldegrees, 9999)
nasispedons$xwgs84 = ifelse(nasispedons$longdeclen > 7, nasispedons$longstddecimaldegrees, 9999)
nasispedons$latcalc = as.numeric(nasispedons$latdegrees) + as.numeric(nasispedons$latminutes)/60 + as.numeric(nasispedons$latseconds)/3600
nasispedons$longcalc = as.numeric(nasispedons$longdegrees) + as.numeric(nasispedons$longminutes)/60 + as.numeric(nasispedons$longseconds)/3600
nasispedons$longcalc = ifelse(nasispedons$longdir == 2, -nasispedons$longcalc, nasispedons$longcalc)
# Again, narrowing down to USA lower 48, other longitudes closer to prime meridian may be unnecessarily excluded by
# the precision standard use: at least 7 signif. digits in this case
nasispedons = subset(nasispedons, longstddecimaldegrees < -60 | longcalc < -60)
nasispedons['pid'] = rownames(nasispedons)
#Reproject pts with nad83 and nad27 datums
#nad83
pedsnad83 = subset(nasispedons, horizdatnm == 2, select = c(pid, longcalc, latcalc, latseconds, longseconds))
pedsnad83$newlat = ""
pedsnad83$newlong = ""
pedsnad83$ynad83 = ifelse(pedsnad83$latseconds != "NA", pedsnad83$latcalc, 9999)
pedsnad83$ynad83[is.na(pedsnad83$ynad83)]<-9999
pedsnad83 = subset(pedsnad83, pedsnad83$ynad83 != 9999)
pedsnad83$xnad83 = ifelse(pedsnad83$longseconds != "NA", pedsnad83$longcalc, 9999)
pedsnad83$xnad83[is.na(pedsnad83$xnad83)]<-9999
pedsnad83 = subset(pedsnad83, pedsnad83$xnad83 != 9999)
coordinates(pedsnad83) = c("xnad83", "ynad83")
proj4string(pedsnad83) = CRS("+proj=longlat +ellps=GRS80")
nadtowgs = spTransform(pedsnad83, CRS("+proj=longlat +datum=WGS84"))
nadtowgs$newlat = as.numeric(nadtowgs$ynad83)
nadtowgs$newlong = as.numeric(nadtowgs$xnad83)
nadtowgs = subset(nadtowgs, select = c(pid, newlat, newlong))
nasispedons = merge(x=nasispedons, y=nadtowgs, by = "pid", all.x = TRUE)
nasispedons$xwgs84 = ifelse(nasispedons$xwgs84 == 9999, nasispedons$newlong, nasispedons$xwgs84)
nasispedons$ywgs84 = ifelse(nasispedons$ywgs84 == 9999, nasispedons$newlat, nasispedons$ywgs84)
nasispedons$newlong = ""
nasispedons$newlat = ""
## nad27 tranformation
pedsnad27 = subset(nasispedons, horizdatnm == 1, select = c(pid, longcalc, latcalc, latseconds, longseconds))
pedsnad27$newlat = ""
pedsnad27$newlong = ""
pedsnad27$ynad27 = ifelse(pedsnad27$latseconds != "NA", pedsnad27$latcalc, 9999)
pedsnad27$ynad27[is.na(pedsnad27$ynad27)]<-9999
pedsnad27 = subset(pedsnad27, pedsnad27$ynad27 != 9999)
pedsnad27$xnad27 = ifelse(pedsnad27$longseconds != "NA", pedsnad27$longcalc, 9999)
pedsnad27$xnad27[is.na(pedsnad83$xnad27)]<-9999
pedsnad27 = subset(pedsnad27, pedsnad27$xnad27 != 9999)
# Need new data frame for projections with ability to link back...
coordinates(pedsnad27) = c("xnad27", "ynad27")
proj4string(pedsnad27) = CRS("+proj=longlat +ellps=GRS80")
nadtowgs = ""
nadtowgs = spTransform(pedsnad27, CRS("+proj=longlat +datum=WGS84"))
nadtowgs$newlat = as.numeric(nadtowgs$ynad27)
nadtowgs$newlong = as.numeric(nadtowgs$xnad27)
nadtowgs = subset(nadtowgs, select = c(pid, newlat, newlong))
nasispedons = merge(x=nasispedons, y=nadtowgs, by = "pid", all.x = TRUE)
nasispedons$xwgs84 = ifelse(nasispedons$xwgs84 == 9999, nasispedons$newlong, nasispedons$xwgs84)
nasispedons$ywgs84 = ifelse(nasispedons$ywgs84 == 9999, nasispedons$newlat, nasispedons$ywgs84)
nasispedons = subset(nasispedons, ywgs84 != "NA" & ywgs84 != 9999)# remove all points 
nasispedons = subset(nasispedons, xwgs84 != "NA" & xwgs84 != 9999)# without coordinates

### Now create subtables at different levels of soil taxonomy###

## Particle Size in Control Section
pscsvec = c(as.character(pscs$pscs))
pscsvec = toupper(pscsvec)
ggvec = c(as.character(gglup$grtgrp_nm))
ggvec = toupper(ggvec)
nasispedons$taxclname = toupper(nasispedons$taxclname)
nasispedons$taxclname = as.factor(nasispedons$taxclname)
# Capitalize all soil names in nasis pedons
nasispedons$seriescap = toupper(nasispedons$taxonname)
nasispedons$pscs = ""
for(i in pscsvec){
  i = as.character(i)
  nasispedons$pscs = ifelse(grepl(i, nasispedons$taxclname), i, nasispedons$pscs)
  print(i)
}
#Now clean up PSCS: include Psamments and Rock outcrops
nasispedons$pscs = ifelse(nasispedons$pscs == "" & grepl("ROCK OUTCROP", nasispedons$seriescap), "ROCK OUTCROP", nasispedons$pscs)
nasispedons$pscs = ifelse(nasispedons$pscs == "" & grepl("PSAMMENT", nasispedons$taxclname), "PSAMMENTS", nasispedons$pscs)

##Great Group match on pedon table taxclname
nasispedons$gg = ""
for(i in ggvec){
  i = as.character(i)
  nasispedons$gg = ifelse(grepl(i, nasispedons$taxclname), i, nasispedons$gg)
  print(i)
}
#Now add "Rock Outcrop" instances to great groups field
nasispedons$gg = ifelse(nasispedons$gg == "" & grepl("ROCK OUTCROP", nasispedons$seriescap), "ROCK OUTCROP", nasispedons$gg)

#### Now match to nearest soil series to link to other table ####

# Get list of unique soil series
allseries  =  c(as.character(seriestab$soilseriesname))
allseries = toupper((allseries))

## Best match code: works, but too many higher taxa mismatches...
'''seriesmatch = function(ptdf, slist){
  slist[amatch(ptdf, slist, maxDist = Inf)]
}
nasispedons$seriesmatch = seriesmatch(nasispedons$seriescap, allseries) '''
nasispedons$seriesmatch = ""
no = 1
# partial match code for series: also creates large number of errors... 
for(i in allseries){
  i = as.character(i)
  # Start with full matches
  nasispedons$seriesmatch = ifelse(i==nasispedons$seriescap, nasispedons$seriescap, nasispedons$seriesmatch)
  # Add partial matches (e.g. "Dekalb like" would be recoded to "Dekalb")
  nasispedons$seriesmatch = ifelse(nasispedons$seriesmatch == "" & grepl(i, nasispedons$seriescap), i, nasispedons$seriesmatch)
  no = no + 1
  print(no)
}


#Need a different series table taxclname for join
seriestab$taxclnamep = seriestab$taxclname

## Join nasispedons and series table
#Conservative match
nasispedonsjn = cbind(seriestab[match(nasispedons$seriescap, seriestab$soilseriesname),], nasispedons)
# Riskier match
nasispedonsjnc = cbind(seriestab[match(nasispedons$seriesmatch, seriestab$soilseriesname),], nasispedons)

## now fill in the pscs where the series table can add in
#Riskier matches: pscs
for(i in pscsvec){
  i = as.character(i)
  nasispedonsjnc$pscs = ifelse(nasispedonsjnc$pscs =="" & grepl(i, nasispedonsjnc$taxclnamep), i, nasispedonsjnc$pscs)
  print(i)
}
# With the conservative matches: pscs
for(i in pscsvec){
  i = as.character(i)
  nasispedonsjn$pscs = ifelse(nasispedonsjn$pscs =="" & grepl(i, nasispedonsjn$taxclnamep), i, nasispedonsjn$pscs)
  print(i)
}


## Now pull out great groups 
#Riskier match: gg
for(i in ggvec){
  i = as.character(i)
  nasispedonsjnc$gg = ifelse(nasispedonsjnc$gg =="" & grepl(i, nasispedonsjnc$taxclnamep), i, nasispedonsjnc$gg)
  print(i)
}

## With the conservative matches: GG
for(i in ggvec){
  i = as.character(i)
  nasispedonsjn$gg = ifelse(nasispedonsjn$gg =="" & grepl(i, nasispedonsjn$taxclnamep), i, nasispedonsjn$gg)
  print(i)
}



## Save new table to tab delimited - important when commas are in records...
nasispedonsjn_pscs = subset(nasispedonsjn, nasispedonsjn$pscs != "", select = c(pid, upedonid, xwgs84, ywgs84, pscs))
nasispedonsjn_gg = subset(nasispedonsjn, nasispedonsjn$gg != "", select = c(pid, upedonid, xwgs84, ywgs84, gg))
write.table(nasispedonsjn_pscs, file = "nasispts_pscs_ttab.txt", sep = "\t")
write.table(nasispedonsjn_gg, file = "nasispts_gg_ttab.txt", sep = "\t")
