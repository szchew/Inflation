set fredkey 2cf4ed60332254125e3f4abcd8e59920, permanently
**** Clear everything and reset frames 
clear
frames reset

***Import necessary series from FRED except NFCI_all
***Generate monthly time series dates using tsset
***Fill in missing months data using tsfill
import fred FEDFUNDS PCEPILFE DPCCRV1Q225SBEA DPCCRG3A086NBEA GDPPOT GDPC1 BBKMGDP
gen monthly_date = mofd(daten)
tsset monthly_date, monthly
tsfill
drop daten 
drop datestr
gen id = _n
order id monthly_date


***Create additional frame to handle NFCI series as NFCI only contains weekly data
***Aggregate NFCI by each month and take the average 
***Following which, we merge with the default frame 
frame create NFCI_all
frame change NFCI_all
import fred NFCI, aggregate(monthly)
gen monthly_date = mofd(daten)
tsset monthly_date, monthly
frame change default
frlink 1:1 monthly_date, frame(NFCI_all)
frget NFCI, from(NFCI_all)
drop NFCI_all

***Carry quarterly and annual data points to all months of the same quarter
***Generate Output Gap as well
replace DPCCRV1Q225SBEA = DPCCRV1Q225SBEA[_n-1] if missing(DPCCRV1Q225SBEA) & id <= 1125
replace DPCCRG3A086NBEA = DPCCRG3A086NBEA[_n-1] if missing(DPCCRG3A086NBEA) & id <= 1116
replace GDPPOT = GDPPOT[_n-1] if missing(GDPPOT) 
replace GDPC1 = GDPC1[_n-1] if missing(GDPC1) & id <= 1125
gen output_gap = ((GDPC1 - GDPPOT)/GDPPOT)*100

***Generate PCE inflation - Annual Growth Rate, Quarterly Growth rate and Monthly Growth Rate
gen annual_pce_inflate = .
forvalues i=361(12)1117{
  replace annual_pce_inflate = ((PCEPILFE[`i'+12] - PCEPILFE[`i'])/PCEPILFE[`i'])*100 if id>=`i' & id<`i'+12
}

gen quarterly_pce_inflate =.
forvalues i=361(4)1126{
  replace quarterly_pce_inflate = ((PCEPILFE[`i'+4] - PCEPILFE[`i'])/PCEPILFE[`i'])*100 if id>=`i' & id<`i'+4
}

gen monthly_pce_inflate =.
forvalues i=361(1)1126{
  replace monthly_pce_inflate = ((PCEPILFE[`i'+1] - PCEPILFE[`i'])/PCEPILFE[`i'])*100 if id>=`i' & id<`i'+1
}

***Trend GDP based on quarterly GDP carried forward to monthly GDPC1
frame create hp_gdp
frame change hp_gdp
import fred GDPC1
gen monthly_date = mofd(daten)
gen quarterly_date = qofd(daten)
tsset quarterly_date, quarterly
tsfilter hp ct=GDPC1, trend(trend_hp_GDPC1)
drop ct
tsset monthly_date, monthly
frame change default
frlink 1:1 monthly_date, frame(hp_gdp)
frget trend_hp_GDPC1, from(hp_gdp)
replace trend_hp_GDPC1 = trend_hp_GDPC1[_n-1] if missing(trend_hp_GDPC1) & id <= 1125
gen hp_output_gap = ((GDPC1 - trend_hp_GDPC1)/trend_hp_GDPC1)*100
drop hp_gdp

*** Label the variables
label variable monthly_date "Year, Month"
label variable output_gap "Output Gap - Difference between Potential and Actual GDP"
label variable annual_pce_inflate "Annual Growth Rate(PCE Inflation)"
label variable quarterly_pce_inflate "Quarterly Growth Rate(PCE Inflation)"
label variable monthly_pce_inflate "Monthly Growth Rate(PCE Inflation)"
label variable trend_hp_GDPC1 "Hodrick-Prescott Filter Trend GDP"
label variable hp_output_gap "Hodrick-Prescott Filter Output Gap"
label variable id "Row Number"


*** Detect Surging Inflation(Binary)
drop surging_inflation
gen surging_inflation = 0 if id >=361 & id <= 1116
egen sd_monthly_pce_inflate = sd(monthly_pce_inflate)
replace surging_inflation = 1 if annual_pce_inflate>2 & monthly_pce_inflate> 2*sd_monthly_pce_inflate & id >=361 & id <= 1116
drop sd_monthly_pce_inflate