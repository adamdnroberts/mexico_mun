library(dplyr)
library(ggplot2)
library(sf)
library(spdep)
library(spatialreg)

mex_sf <- read_sf("~/mexico_RD/mun1995shp/Municipios_1995.shp")
mex_sf$mun_id <- gsub(" ", "", paste(mex_sf$CVE_ENT,mex_sf$CVE_MUN))

load("~/mexico_RD/full_dataset_mexelec.Rdata")
df <- subset(big_df, year >= 1995 & year <= 1997)
df$PRD_pct <- df$PRD / (df$p1 + df$p2) # PRD percentage compared to top two
df$PRD_treat <- ifelse(df$PRD_pct > 0.5, 1, 0)
df <- subset(df, select = c(Municipio, PAN_pct, PRD_pct, next_PAN_pct, estado, year))
df$mun_id <- gsub(" ", "", df$Municipio)

mex_sf2 <- merge(mex_sf, df, by = "mun_id")
mex_sf3 <- na.omit(mex_sf2)

#Coerce the sf object into a new sp object
mex_sp <- as(mex_sf3, "Spatial")

#create neighbors list? matrix?
w <- poly2nb(mex_sp, row.names=mex_sp$mun_id)

wm <- nb2mat(w, style='W', zero.policy = T)
sum(wm[1,])

#spatial weights list for Moran's I
ww <-  nb2listw(w, style='B', zero.policy = T)

moran(mex_sp$PAN_pct, ww, n=length(ww$neighbours), S0=Szero(ww))

set.seed(42)
moranmc_results <- moran.mc(mex_sp$PAN_pct, ww, nsim=1000)
moranmc_results

rwm <- mat2listw(wm, style='W', zero.policy = T)
# Checking if rows add up to 1 (they should)
mat <- listw2mat(rwm)
#This code is simply adding each row to see if we get one when we add their values up, we are only displaying the first 15 rows in the matrix
apply(mat, 1, sum)[1:15]

moran.plot(mex_sp$PAN_pct, rwm, labels = FALSE, xlab = "PAN %", ylab = "Spatially lagged PAN %")
title(main = "PAN % Spatial Lag, 1995-1997 Municipal Elections")

moran(mex_sp$PRD_pct, ww, n=length(ww$neighbours), S0=Szero(ww))

set.seed(42)
moranmc_results <- moran.mc(mex_sp$PRD_pct, ww, nsim=1000)
moranmc_results

moran.plot(mex_sp$PRD_pct, rwm, labels = FALSE, xlab = "PRD %", ylab = "Spatially lagged PRD %")
title(main = "PRD % Spatial Lag, 1995-1997 Municipal Elections")

m.pan <- lm(next_PAN_pct ~ PAN_pct + estado + year, data = mex_sf3)
summary(m.pan)

lm.RStests(m.pan, rwm, test = c("LMerr","LMlag","RLMerr","RLMlag","SARMA"))

test <- rwm
test$neighbours <- test$neighbours[!is.na(mex_sf3$next_PAN_pct)]
test$weights <- test$weights[!is.na(mex_sf3$next_PAN_pct)]

spatreg <- spatialreg::lagsarlm(next_PAN_pct ~ PAN_pct + estado, data = mex_sf3, rwm, zero.policy = T)
summary(spatreg)

W <- as(rwm, "CsparseMatrix")
trMC <- trW(W, type="MC")

im<-impacts(spatreg, tr = trMC, R=100)

sums<-summary(im,  zstats=T)

data.frame(sums$res)
hey <- data.frame(sums$pzmat)
