//ssc install wbopendata
//db wbopendata
//ssc install tsegen
//ssc install bgshade

clear
frames reset

wbopendata, language(en - English) country() topics() indicator(FP.CPI.TOTL.ZG - Inflation, consumer prices (annual %))
drop if countrycode != "USA"
drop countryname region regionname adminregion adminregionname incomelevel incomelevelname lendingtype lendingtypename indicatorname indicatorcode
reshape long yr, i(countrycode) j(year2) string
rename yr inflation_rate
gen year = yearly(year2,"Y")
drop year2
tsset year
gen inflation_rate_change = inflation_rate-L.inflation_rate

frame create fedfunds
frame change fedfunds
import fred FEDFUNDS, aggregate(annual)
gen year = yofd(daten)
tsset year
frame change default
frlink 1:1 year, frame(fedfunds)
frget FEDFUNDS, from(fedfunds)
frame change default
frame drop fedfunds

gen isa = 0
replace isa = 1 if inflation_rate_change >= 2.1
tsegen last_10_mean = rowmean(L(0/9).inflation_rate) if year>=1970
tsegen last_10_sd = rowsd(L(0/9).inflation_rate) if year>=1970
gen isr = 0
replace isr = 1 if inflation_rate_change > last_10_mean+1.65*last_10_sd

label variable inflation_rate "Annual Inflation Rate"
label variable year "Year"
label variable inflation_rate_change "Change in inflation rate"
label variable isa "Ottonello's Absolute Inflation Surge"
label variable last_10_mean "Rolling mean of past 10 year's change in inflation rate"
label variable last_10_sd "Rolling sd of past 10 year's change in inflation rate"
label variable isr "Ottonello's Relative Inflation Surge"
label variable FEDFUNDS "FED Interest Rate"

bgshade year, shaders(isa) legend ///
	twoway(line inflation_rate FEDFUNDS year, title("Annual Inflation Rate for USA") lcolor(navy) legend(pos(1) ring(0) cols(1) symxsize(5) size(2) rowgap() region(col(none)) stack symplacement(center)))




