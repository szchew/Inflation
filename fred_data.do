set fredkey 2cf4ed60332254125e3f4abcd8e59920, permanently
//ssc install bgshade
//ssc install tsspell


**** Clear everything and reset frames 
clear
frames reset

***Import necessary series from FRED except NFCI_all
***Generate monthly time series dates using tsset
***Fill in missing months data using tsfill
import fred CPIAUCSL UNRATE FEDFUNDS PCEPILFE DPCCRV1Q225SBEA DPCCRG3A086NBEA GDPPOT GDPC1 BBKMGDP
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
label variable annual_pce_inflate "Annual PCE Inflation Rate"
label variable quarterly_pce_inflate "Quarterly PCE Inflation Rate"
label variable monthly_pce_inflate "Monthly PCE Inflation Rate"
label variable trend_hp_GDPC1 "Hodrick-Prescott Filter Trend GDP"
label variable hp_output_gap "Hodrick-Prescott Filter Output Gap"
label variable id "Row Number"


*** Detect Surging Inflation(Binary)

//Generate First Differnece(t - (t-1)) & First Forward Difference ((t+1) - t) for Annual PCE Inflation and carry forward the two differences forward to their respective years
//We first generate 2 columns named annual_pce_inflate_1st_diff and annual_pce_inflate_1st_f_diff which contains the first difference and first forward difference respectively
//Next, we would label the 2 columns
//Lastly, we would generate the required values usinf PCE inflation and carry them forward
gen annual_pce_inflate_1st_f_diff=.
gen annual_pce_inflate_1st_diff=.

label variable annual_pce_inflate_1st_diff "First Difference of Annual PCE Inflation"
label variable annual_pce_inflate_1st_f_diff "First Forward Difference of Annual PCE Inflation"

replace annual_pce_inflate_1st_f_diff = f.annual_pce_inflate - annual_pce_inflate if f.annual_pce_inflate - annual_pce_inflate != 0
forvalues i=1/11{
	replace annual_pce_inflate_1st_f_diff = annual_pce_inflate_1st_f_diff[id+1] if missing(annual_pce_inflate_1st_f_diff) & id<=1116
}
replace annual_pce_inflate_1st_diff = d.annual_pce_inflate if id >=361 & id <= 1116 
replace annual_pce_inflate_1st_diff=. if annual_pce_inflate_1st_diff==0
replace annual_pce_inflate_1st_diff = annual_pce_inflate_1st_diff[id-1] if missing(annual_pce_inflate_1st_diff) & id<=1116 & id>=361


//Next, we would detect all Annual PCE inflation that are above 5 percent(Identified using 3 conditions)
//1)First Forward Difference of PCE Inflation <=0 & First Difference of PCE Inflation >= 0 
//2)Time period is valid 
// Following which, we would store the Peaks as a binary variable called aboveXpercentS 
gen aboveXpercentS = 0 if id >=361 & id <= 1116
label variable aboveXpercentS "Above X percent & First Forward Diff >= 0 & First Diff <= 0"
replace aboveXpercentS = 1 if annual_pce_inflate>5 & id >=361 & id <= 1116 & annual_pce_inflate_1st_diff>=0 & annual_pce_inflate_1st_f_diff<=0

//Ensure that for each group of constant high inflation, we can identify the max
tsspell aboveXpercentS if aboveXpercentS==1 
egen max = max(annual_pce_inflate) if annual_pce_inflate !=., by(_spell) 
gen surgeInflation=.
label variable surgeInflation "Inflation Surge"
replace surgeInflation=1 if aboveXpercentS==1 & annual_pce_inflate==max
drop _spell _seq _end max

//We would now find the trough for every peak in surgeInflation by finding the minimum Annual PCE Inflation that is  >= 2%
// & First Forward Difference in PCE Inflation >= 0(Ensure consistent increase in inflation to better detect troughs) -- Store these value in trough_min
// (We then create trough_min2 to carry forward the minimum identified above to each group that they are in, followed by dropping the unnecessary trough_min2)
replace surgeInflation=0 if surgeInflation!=1 & id >=361 & id <= 1116
tsspell surgeInflation if surgeInflation==0 
bysort _spell (monthly_date): egen trough_min = min(annual_pce_inflate) if id >=361 & id <= 1116 & annual_pce_inflate >=2 & annual_pce_inflate_1st_f_diff>=0
bysort _spell: egen trough_min2 = max(trough_min)
replace trough_min = trough_min2
drop trough_min2

//We then match each peak with their respective trough 
// P.S. I create max_spell in order to identify the last consecutive period where trough might exist so that I can eliminate this particular peiod when matching the peaks and the troughs

egen max_spell = max(_spell)
replace surgeInflation = 1 if annual_pce_inflate==trough_min & _spell >=1 & _spell < max_spell
drop _spell _seq _end trough_min max_spell
tsspell surgeInflation if surgeInflation== 1
   //Using tf and the for loop below, I 'closed' the gap between each peak and their respective trough (and drop redundant columns)
sort monthly_date
gen tf=0
forvalues i=362/1116{
	replace tf=1 if surgeInflation[`i'-1]==1 & surgeInflation[`i']==0 & mod(_spell[`i'-1],2)==1
	replace tf=0 if surgeInflation[`i'-1]==1 & surgeInflation[`i']==1 & mod(_spell[`i'-1],2)==0
	replace surgeInflation=1 in `i' if tf==1
}
drop _spell _seq _end tf

//Plot the Inflation Surge Graph!!!
bgshade monthly_date, shaders(surgeInflation) legend ///
	twoway(line annual_pce_inflate monthly_date if tin(1950m1, 2025m12) , yaxis(1) || line FEDFUNDS monthly_date if FEDFUNDS !=., yaxis(2) ///
	title("USA") legend(pos(1) ring(0) cols(1) symxsize(5) size(2) region(col(none)) stack symplacement(center)) ///
	ytitle("Annual PCE Inflation Rate"))


*** Detect Stability
gen stableInflation = 0
replace stableInflation = 1 if annual_pce_inflate>=0 & annual_pce_inflate<=2

*** Detect Deflation
drop deflation
gen deflation = 0 if id >=361 & id <= 1116
label variable deflation "Deflation"
replace deflation = 1 if annual_pce_inflate <= 2 & D.annual_pce_inflate <=0
