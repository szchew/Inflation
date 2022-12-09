//ssc install wbopendata
//db wbopendata
//ssc install tsegen

clear
wbopendata, language(en - English) country() topics() indicator(FP.CPI.TOTL.ZG - Inflation, consumer prices (annual %))
drop if countrycode != "USA"
drop countryname region regionname adminregion adminregionname incomelevel incomelevelname lendingtype lendingtypename indicatorname indicatorcode
reshape long yr, i(countrycode) j(year2) string
rename yr inflation_rate
gen year = yearly(year2,"Y")
drop year2
tsset year
gen inflation_rate_change = inflation_rate-L.inflation_rate

gen inflation_surge_abs = 1 if inflation_rate_change >= 2.1
tsegen last_10_mean = rowmean(L(0/9).inflation_rate) if year>=1970
tsegen last_10_sd = rowsd(L(0/9).inflation_rate) if year>=1970
gen inflation_surge_rel = 1 if inflation_rate_change > last_10_mean+1.65*last_10_sd

label variable inflation_rate "Annual inflation rate(From CPI Inflation - World Bank)"
label variable year "Year"
label variable inflation_rate_change "Change in inflation rate"
label variable inflation_surge_abs "Inflation Surge(Binary) - Ottonello's absolute criterion"
label variable last_10_mean "Rolling mean of past 10 year's change in inflation rate"
label variable last_10_sd "Rolling sd of past 10 year's change in inflation rate"
label variable inflation_surge_rel "Inflation surge(Binary) - Ottonello's relative criterion"