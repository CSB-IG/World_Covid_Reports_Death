
### Reading data ###############
Y <-read.csv(file="DailyDeaths_05_20_2020.csv", header=TRUE)

### Initial check up ###########
# summary(Y)

#### Required libraries ###################

library(psd)
library(TSEntropies)
library(fractal)

#######################################################
######## Section II - Daily Death COVID-19 Dynamics ###
#######################################################
#### Calculating the autocorrelation function #########

### Generating time series ####

MexTS=Y[162,115:186]
# MexTS
MexFull=Y[162,68:186]
# MexFull
MXT=as.vector(MexTS)
MXF=as.vector(MexFull)

USTS=Y[1,105:186]
#USTS
USFull=Y[1,68:186]
#USFull
UST=as.vector(USTS)
USF=as.vector(USFull)


CNTS=Y[11,68:182]
#CNTS
CNFull=Y[11,68:186]
#CNFull
CNT=as.vector(CNTS)
CNF=as.vector(CNFull)

BRTS=Y[70,122:186]
#BRTS
BRFull=Y[70,68:186]
#BRFull
BRT=as.vector(BRTS)
BRF=as.vector(BRFull)

INTS = Y[163,133:186]
#INTS
INFull=Y[163,68:186]
#INFull
INT=as.vector(INTS)
INF=as.vector(INFull)


UKTS=Y[2,114:186]
#UKTS
UKFull=Y[2,68:186]
#UKFull
UKT=as.vector(UKTS)
UKF=as.vector(UKFull)

BETS=Y[6,121:186]
#BETS
BEFull=Y[6,68:186]
#BEFull
BET=as.vector(BETS)
BEF=as.vector(BEFull)

##### Calculating the partial autocorrelation function ###############################################
######################################################################################################
# In time series analysis, the partial autocorrelation function (PACF) 
# gives the partial correlation of a stationary time series with its own lagged values, 
# regressed the values of the time series at all shorter lags. 
# It contrasts with the autocorrelation function, which does not control for other lags.
# It is a partial correlation, hence it measures the degree of association between two 
# random variables, with the effect of a set of controlling random variables removed. 
# If we are interested in finding to what extent there is a numerical relationship between 
# two variables of interest, using their correlation coefficient will give misleading results 
# if there is another, confounding, variable that is numerically related to both variables of interest.
########################################################################################################

PACFMX = acf(MXT, lag.max = 71,
    type = "partial",
    plot = FALSE)


#PACFMX
plot(PACFMX, main="Partial autocorrelation daily deaths Mexico")

#### Full series (zero-biased) -> **F ######

PACFMXF = acf(MXF, lag.max = 118,
             type = "partial",
             plot = FALSE)


#PACFMXF
plot(PACFMXF, main="Partial autocorrelation daily deaths Mexico zero-biased")


PACFUS = acf(UST, lag.max = 81,
             type = "partial",
             plot = FALSE)

#PACFUS
plot(PACFUS, main="Partial autocorrelation daily deaths US")

PACFUSF = acf(USF, lag.max = 118,
             type = "partial",
             plot = FALSE)

#PACFUSF
plot(PACFUSF,main="Partial autocorrelation daily deaths US zero-biased")


PACFCN = acf(CNT, lag.max = 114,
             type = "partial",
             plot = FALSE)

#PACFCN
plot(PACFCN, main="Partial autocorrelation daily deaths China")


PACFCNF = acf(CNF, lag.max = 118,
             type = "partial",
             plot = FALSE)

#PACFCNF
plot(PACFCNF, main="Partial autocorrelation daily deaths China zero-biased")


PACFBR = acf(BRT, lag.max = 64,
             type = "partial",
             plot = FALSE)

#PACFBR
plot(PACFBR, main="Partial autocorrelation daily deaths Brazil")

PACFBRF = acf(BRF, lag.max = 118,
             type = "partial",
             plot = FALSE)

#PACFBRF
plot(PACFBRF, main="Partial autocorrelation daily deaths Brazil zero-biased")


PACFIN = acf(INT, lag.max = 53,
             type = "partial",
             plot = FALSE)

#PACFIN
plot(PACFIN, main="Partial autocorrelation daily deaths India")

PACFINF = acf(INF, lag.max = 118,
             type = "partial",
             plot = FALSE)

#PACFINF
plot(PACFINF, main="Partial autocorrelation daily deaths India zero-biased")

PACFUK = acf(UKT, lag.max = 72,
             type = "partial",
             plot = FALSE)

#PACFUK
plot(PACFUK, main="Partial autocorrelation daily deaths UK")

PACFUKF = acf(UKF, lag.max = 118,
             type = "partial",
             plot = FALSE)

#PACFUKF
plot(PACFUKF, main="Partial autocorrelation daily deaths UK zero-biased")



PACFBE = acf(BET, lag.max = 65,
             type = "partial",
             plot = FALSE)

#PACFBE
plot(PACFBE, main="Partial autocorrelation daily deaths Belgium")

PACFBEF = acf(BEF, lag.max = 118,
             type = "partial",
             plot = FALSE)

#PACFBEF
plot(PACFBEF, main="Partial autocorrelation daily deaths Belgium zero-biased")

############################################################################################
# Calculation of the power spectral density to look up for periodicities in the time series #
#############################################################################################

MXTT = as.numeric(MXT)
USTT = as.numeric(UST)
CNTT= as.numeric(CNT)
BRTT= as.numeric(BRT)
INTT=as.numeric(INT)
UKTT=as.numeric(UKT)
BETT=as.numeric(BET)

MXTF = as.numeric(MXF)
USTF = as.numeric(USF)
CNTF= as.numeric(CNF)
BRTF= as.numeric(BRF)
INTF=as.numeric(INF)
UKTF=as.numeric(UKF)
BETF=as.numeric(BEF)

##################################################################################
### Scatterplots for daily death Epidemics #######################################
##################################################################################


plot(MXTT, main="Daily COVID-19 deaths Mexico")
plot(MXTF, main="Daily COVID-19 deaths Mexico zero-biased")

plot(USTT, main="Daily COVID-19 deaths US")
plot(USTF, main="Daily COVID-19 deaths US zero-biased")

plot(CNTT, main="Daily COVID-19 deaths China")
plot(CNTF, main="Daily COVID-19 deaths China zero-biased")

plot(BRTT, main="Daily COVID-19 deaths Brazil")
plot(BRTF, main="Daily COVID-19 deaths Brazil zero-biased")

plot(INTT, main="Daily COVID-19 deaths India")
plot(INTF, main="Daily COVID-19 deaths India zero-biased")

plot(UKTT, main="Daily COVID-19 deaths UK")
plot(UKTF, main="Daily COVID-19 deaths UK zero-biased")

plot(BETT, main="Daily COVID-19 deaths Belgium")
plot(BETF, main="Daily COVID-19 deaths Belgium zero-biased")


##### Periodogram and autoregressive power spectral densities (linear scale) ####

spectrum(MXTT, log="no",main="Power Spectral Density daily deaths Mexico")
spectrum(MXTT, method="ar", log="no", main="Power Spectral Density daily deaths Mexico autoregressive")
spectrum(MXTF, log="no",main="Power Spectral Density daily deaths Mexico zero-biased")
spectrum(MXTF, method="ar", log="no",main="Power Spectral Density daily deaths Mexico autoregressive zero-biased")

spectrum(USTT, log="no",main="Power Spectral Density daily deaths US")
spectrum(USTT, method="ar", log="no",main="Power Spectral Density daily deaths US autoregressive")
spectrum(USTF, log="no",main="Power Spectral Density daily deaths US zero-biased")
spectrum(USTF, method="ar", log="no",main="Power Spectral Density daily deaths US autoregressive zero-biased")

spectrum(CNTT, log="no",main="Power Spectral Density daily deaths China")
spectrum(CNTT, method="ar", log="no",main="Power Spectral Density daily deaths China autoregressive")
spectrum(CNTF, log="no",main="Power Spectral Density daily deaths China zero-biased")
spectrum(CNTF, method="ar", log="no",main="Power Spectral Density daily deaths China autoregressive zero-biased")

spectrum(BRTT, log="no",main="Power Spectral Density daily deaths Brazil")
spectrum(BRTT, method="ar", log="no",main="Power Spectral Density daily deaths Brazil autoregressive")
spectrum(BRTF, log="no" ,main="Power Spectral Density daily deaths Brazil zero-biased")
spectrum(BRTF, method="ar", log="no",main="Power Spectral Density daily deaths Brazil autoregressive zero-biased")

spectrum(INTT, log="no",main="Power Spectral Density daily deaths India")
spectrum(INTT, method="ar", log="no",main="Power Spectral Density daily deaths India autoregressive")
spectrum(INTF, log="no",main="Power Spectral Density daily deaths India zero-biased")
spectrum(INTF, method="ar", log="no",main="Power Spectral Density daily deaths India autoregressive zero-biased")

spectrum(UKTT, log="no",main="Power Spectral Density daily deaths UK")
spectrum(UKTT, method="ar", log="no",main="Power Spectral Density daily deaths UK autoregressive")
spectrum(UKTF, log="no",main="Power Spectral Density daily deaths UK zero-biased")
spectrum(UKTF, method="ar", log="no",main="Power Spectral Density daily deaths UK autoregressive zero-biased")

spectrum(BETT, log="no",main="Power Spectral Density daily deaths Belgium")
spectrum(BETT, method="ar", log="no",main="Power Spectral Density daily deaths Belgium autoregressive")
spectrum(BETF, log="no",main="Power Spectral Density daily deaths Belgium zero-biased")
spectrum(BETF, method="ar", log="no",main="Power Spectral Density daily deaths Belgium autoregressive zero-biased")

##### Tappered power spectral density ##################################################################################
# Adaptive sine multitaper power spectral density estimation  ##########################################################
# This is the primary function to be used in this package: it returns power spectral density estimates of a univariate #
# timeseries, with an optimal number of tapers at each frequency based on iterative reweighted spectral derivatives.####
########################################################################################################################

pspectrum(MXTT,plot=TRUE,main="Power Spectral Density daily deaths tappered Mexico")
pspectrum(USTT,plot=TRUE,main="Power Spectral Density daily deaths tappered US")
pspectrum(CNTT,plot=TRUE,main="Power Spectral Density daily deaths tappered China")
pspectrum(BRTT,plot=TRUE,main="Power Spectral Density daily deaths tappered Brazil")
pspectrum(INTT,plot=TRUE,main="Power Spectral Density daily deaths tappered India")
pspectrum(UKTT,plot=TRUE,main="Power Spectral Density daily deaths tappered UK")
pspectrum(BETT,plot=TRUE,main="Power Spectral Density daily deaths tappered Belgium")

pspectrum(MXTF,plot=TRUE,main="Power Spectral Density daily deaths tappered Mexico zero-biased")
pspectrum(USTF,plot=TRUE,main="Power Spectral Density daily deaths tappered US zero-biased")
pspectrum(CNTF,plot=TRUE,main="Power Spectral Density daily deaths tappered China zero-biased")
pspectrum(BRTF,plot=TRUE,main="Power Spectral Density daily deaths tappered Brazil zero-biased")
pspectrum(INTF,plot=TRUE,main="Power Spectral Density daily deaths tappered India zero-biased")
pspectrum(UKTF,plot=TRUE,main="Power Spectral Density daily deaths tappered UK zero-biased")
pspectrum(BETF,plot=TRUE,main="Power Spectral Density daily deaths tappered Belgium zero-biased")

###### Approximate Entropy ###################################################################
# Approximate entropy (ApEn) is a technique used to quantify the amount of regularity and  ###
# the unpredictability of fluctuations over time-series data.                              ###
# Regularity was originally measured by exact regularity statistics, which has mainly      ###  
# centered on various entropy measures. However, accurate entropy calculation requires vast ##
# amounts of data, and the results will be greatly influenced by system noise,             ###
# therefore it is not practical to apply these methods to experimental data.               ###
# ApEn was developed by Steve M. Pincus to handle these limitations by modifying an        ###
# exact regularity statistic, Kolmogorov–Sinai entropy.                                    ###
##############################################################################################

MX_entropy=FastApEn_R(MXTT, dim = 2, lag = 1, r = 0.15 * sd(MXTT))
MX_entropy
MX_entropy_F=FastApEn_R(MXTF, dim = 2, lag = 1, r = 0.15 * sd(MXTT))
MX_entropy_F

US_entropy=FastApEn_R(USTT, dim = 2, lag = 1, r = 0.15 * sd(USTT))
US_entropy
US_entropy_F=FastApEn_R(USTF, dim = 2, lag = 1, r = 0.15 * sd(USTT))
US_entropy_F

CN_entropy=FastApEn_R(CNTT, dim = 2, lag = 1, r = 0.15 * sd(CNTT))
CN_entropy
CN_entropy_F=FastApEn_R(CNTF, dim = 2, lag = 1, r = 0.15 * sd(CNTT))
CN_entropy_F

BR_entropy=FastApEn_R(BRTT, dim = 2, lag = 1, r = 0.15 * sd(BRTT))
BR_entropy
BR_entropy_F=FastApEn_R(BRTF, dim = 2, lag = 1, r = 0.15 * sd(BRTT))
BR_entropy_F

IN_entropy=FastApEn_R(INTT, dim = 2, lag = 1, r = 0.15 * sd(INTT))
IN_entropy
IN_entropy_F=FastApEn_R(INTF, dim = 2, lag = 1, r = 0.15 * sd(INTT))
IN_entropy_F

UK_entropy=FastApEn_R(UKTT, dim = 2, lag = 1, r = 0.15 * sd(UKTT))
UK_entropy
UK_entropy_F=FastApEn_R(UKTF, dim = 2, lag = 1, r = 0.15 * sd(UKTT))
UK_entropy_F

BE_entropy=FastApEn_R(BETT, dim = 2, lag = 1, r = 0.15 * sd(BETT))
BE_entropy
BE_entropy_F=FastApEn_R(BETF, dim = 2, lag = 1, r = 0.15 * sd(BETT))
BE_entropy_F

###### SampleEntropy (no self-similarities accounted) #############

MX_fast_entropy=FastSampEn(MXTT, dim = 2, lag = 1, r = 0.15 * sd(MXTT))
MX_fast_entropy
MX_fast_entropy_F=FastSampEn(MXTF, dim = 2, lag = 1, r = 0.15 * sd(MXTT))
MX_fast_entropy_F

US_fast_entropy=FastSampEn(USTT, dim = 2, lag = 1, r = 0.15 * sd(USTT))
US_fast_entropy
US_fast_entropy_F=FastSampEn(USTF, dim = 2, lag = 1, r = 0.15 * sd(USTT))
US_fast_entropy_F

CN_fast_entropy=FastSampEn(CNTT, dim = 2, lag = 1, r = 0.15 * sd(CNTT))
CN_fast_entropy
CN_fast_entropy_F=FastSampEn(CNTF, dim = 2, lag = 1, r = 0.15 * sd(CNTT))
CN_fast_entropy_F

BR_fast_entropy=FastSampEn(BRTT, dim = 2, lag = 1, r = 0.15 * sd(BRTT))
BR_fast_entropy
BR_fast_entropy_F=FastSampEn(BRTF, dim = 2, lag = 1, r = 0.15 * sd(BRTT))
BR_fast_entropy_F

IN_fast_entropy=FastSampEn(INTT, dim = 2, lag = 1, r = 0.15 * sd(INTT))
IN_fast_entropy
IN_fast_entropy_F=FastSampEn(INTF, dim = 2, lag = 1, r = 0.15 * sd(INTT))
IN_fast_entropy_F

UK_fast_entropy=FastSampEn(UKTT, dim = 2, lag = 1, r = 0.15 * sd(UKTT))
UK_fast_entropy
UK_fast_entropy_F=FastSampEn(UKTF, dim = 2, lag = 1, r = 0.15 * sd(UKTT))
UK_fast_entropy_F

BE_fast_entropy=FastSampEn(BETT, dim = 2, lag = 1, r = 0.15 * sd(BETT))
BE_fast_entropy
BE_fast_entropy_F=FastSampEn(BETF, dim = 2, lag = 1, r = 0.15 * sd(BETT))
BE_fast_entropy_F


##################################################################################
#### Detrended Fluctuations Analysis Daily Deaths Time Series ####################

DFA(MXTT, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(MXTT)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(MXTF, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(MXTF)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(USTT, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(USTT)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(USTF, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(USTF)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(CNTT, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(CNTT)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(CNTF, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(CNTF)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(BRTT, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(BRTT)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(BRTF, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(BRTF)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(INTT, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(INTT)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(INTF, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(INTF)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(UKTT, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(UKTT)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(UKTF, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(UKTF)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(BETT, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(BETT)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

DFA(BETF, detrend="poly1", sum.order=0, overlap=0,
    scale.max=trunc(length(BETF)/2), scale.min=NULL,
    scale.ratio=2, verbose=FALSE)

