library(MonteCarlo)
library(EnvStats)
library(dplyr)
library(ggplot2)
library(tidymv) #superseded by tidygam
#defining the inventory function; overall emissions inventory sum
inventory <- function(n) {
  #sample from the pdfs
  housesTO_1 <- runif(n,381071.3000,429718.7000) #source 1: house paint shedding
  exterior_surface_area1 <- rtri(n,2530,4382,3578) #confirmed #source 1: house paint shedding
  proportion_painted <- rtri(n,0.25,1,0.5) #NEW!
  mass_paint_area_1 <- runif(n,0.0088,0.0112) #source 1: house paint shedding
  kg_gallon_paint1 <- runif(n,2.72155,5.44311) #source 1: house paint shedding
  percent_solids1 <- runif(n,17.6000,22.4000)/100 #source 1: house paint shedding
  house_shedding_rate1 <- runif(n,0.01,0.04) #confirmed #source 1: house paint shedding

  #source 2 MMW or littering: considering uncertainty from "litterable area" as well as littering timescale
  littering_rate1 <- runif(n,0.391438402177165,0.983602320576728)+runif(n,3.94655314204831,13.6422558847085)+runif(n,3.22745344538179,37.5565601021775)+runif(n,5.63778203324361,16.9081814240018)+runif(n,3.94694692611396,17.2594602897542)+runif(n,3.41760562393187,10.1794553811305)+runif(n,2.4882593515349,7.6158459166319)+runif(n,1.82799813120172,4.31025310236055)+runif(n,1.27903356356134,3.12534044988941)+runif(n,9.72955657205548,41.5018912339657)+runif(n,2.56475741790535,7.36889228061836)+runif(n,14.4560868079744,71.5038296980515)+runif(n,2.75594543011381,10.7029464154899)+runif(n,4.3160876458958,12.8098240743688)+runif(n,4.50047268166171,14.8135453900702)+runif(n,1.04995958405722,2.56233592893723)+runif(n,5.50366376082537,15.4600287445741)+runif(n,6.79237301432807,18.0815849309858)+runif(n,1.48871439755562,10.9460940344972)+runif(n,5.11817721462802,14.21037087283)+runif(n,2.95485915530633,7.31429191917501)+runif(n,10.1736610725576,28.9354438082811)+runif(n,6.05005206871066,22.3075551039628)+runif(n,3.09635391110603,11.305978704994)+runif(n,5.13583104490466,16.3690892194057)+runif(n,5.60025685718506,14.4346277007538)+runif(n,3.37054803767564,16.4835643523029)+runif(n,0.85352412720543,2.35244766824465)+runif(n,1.1830735668227,14.9633706334029)+runif(n,5.89950018795366,17.3480972585549)+runif(n,2.76696134208546,6.79733674532405)+runif(n,31.0483421709326,87.8437334878306)+runif(n,10.1168861607408,30.888188820349)+runif(n,8.6053402940873,29.0095092854447)+runif(n,13.3535149593952,44.2488764234017)+runif(n,7.32305055260027,21.2986935266053)+runif(n,10.9384809008475,30.6285293981027)+runif(n,1.72790454874013,6.3608639076703)+runif(n,0.29593682892258,0.749948011992112)+runif(n,0.0397287436913667,0.357165077664186)+runif(n,25.993051767541,63.2364753456513)+runif(n,3.17814633554849,11.1788252269504)+runif(n,0.0412789434175218,0.104033283107479)+runif(n,1.53135688506845,4.71925790528699)+runif(n,0.00467649805135976,0.0167001743268518)+runif(n,7.33517300414959,20.8169876014024)+runif(n,10.0921877935971,28.3991163791631)+runif(n,0.573225246687335,3.2487162699874)+runif(n,2.98006540016769,8.25268494091298)+runif(n,0.790726238906733,2.44027421744929)+runif(n,6.2059449699211,16.4550159483781)+runif(n,1.22284195418513,4.9451771623867)+runif(n,19.5056207366144,151.490046805987)+runif(n,1.0382421327266,5.64552833375915)+runif(n,4.66554571610607,42.5926036685556)+runif(n,2.4811656546548,6.80256162512416)+runif(n,2.24320482602513,22.8772118251065)+runif(n,2.37512502320795,10.6800248321611)+runif(n,3.7228435410862,17.49640326531)+runif(n,2.7963808390879,10.9791287121239)+runif(n,4.5022839250348,14.8818659086841)+runif(n,6.96428395241635,22.9423103508813)+runif(n,2.22767763036722,16.9865340556003)+runif(n,3.23102527466658,14.4142370984464)+runif(n,10.6138892581208,90.0350801868732)+runif(n,11.9616807010173,37.7513845086908)+runif(n,2.18333761470522,9.35964365463246)+runif(n,5.81416622030032,20.3917426204488)+runif(n,1.14272053412952,5.4131709003908)+runif(n,7.96412234611592,23.9831299531262)+runif(n,0.821764031592406,3.14296035488058)+runif(n,17.0417687547552,63.3587514819735)+runif(n,51.47123996263,293.284893289207)+runif(n,35.0051314196974,135.689596573419)+runif(n,4.27524041044815,17.2423836051229)+runif(n,3.2267402343768,10.3192067283232)+runif(n,7.7634764692108,47.6019641649207)+runif(n,21.4117552458751,386.484971067772)+runif(n,3.796106640093,21.3386264608956)+runif(n,5.24120046741943,31.6541557314454)+runif(n,5.0983950075135,29.1869170663506)+runif(n,6.59970645990459,46.2179391888053)
  littering_rate2 <- runif(n,37.4938414007433,157.638077393861)+runif(n,1.21582122833157,5.31655827890283)+runif(n,0.379917101812949,1.59126112189875)+runif(n,1.55488291217416,9.91026820676877)+runif(n,3.08583810804767,118.146412052697)+runif(n,9.94596295781867,48.310270170745)+runif(n,10.2696382493446,41.1415058264323)+runif(n,1.83273250716997,5.44162416194681)+runif(n,2.21273872436956,7.25058335722598)+runif(n,4.52396169216355,19.8614170556771)+runif(n,5.34458674203312,46.2332767202556)+runif(n,6.07263439575046,24.5741990853116)+runif(n,9.49878947929143,34.9009840349431)+runif(n,7.45328738226053,22.6236404732207)+runif(n,2.3733376650426,14.0903466378138)+runif(n,8.84158586131609,45.7858610868298)+runif(n,2.07870764525775,9.92385184034394)+runif(n,1.73849260028415,10.6634360882762)+runif(n,0.950959001971771,6.15973106160371)+runif(n,2.68225253259667,11.0888510346556)+runif(n,0.0688264783334576,0.490838929344842)+runif(n,3.61562691700342,12.405626672217)+runif(n,10.3171105159518,33.9461318360796)+runif(n,20.8592151447112,140.719730814419)+runif(n,15.6187615746062,107.76773685327)+runif(n,29.9567992897924,195.758282016638)+runif(n,11.673138669368,33.0573845936565)+runif(n,3.3832796026396,18.7146726647915)+runif(n,2.41807492796867,13.4370212145566)+runif(n,1.49002839095732,9.03042284490711)+runif(n,17.7709956405927,57.5221268552684)+runif(n,7.60376827966723,26.9712058733691)+runif(n,45.3237775142256,136.210386760317)+runif(n,6.54186291752882,42.4503387584748)+runif(n,20.2764798063948,330.525516294046)+runif(n,59.7938866096068,204.998967688084)+runif(n,7.08883391865073,24.0549788662615)+runif(n,13.1818391752305,126.695397524458)+runif(n,7.77557562214391,20.6005694485165)+runif(n,0.732062163188044,3.46119397967027)+runif(n,4.40817589554693,22.9389690146355)+runif(n,2.67275212956516,26.2814732743399)+runif(n,5.02689819555045,67.6784186092757)+runif(n,2.81232766876368,10.489042111765)+runif(n,3.92575432267507,16.6805280586959)+runif(n,2.34146369553851,7.99813612938933)+runif(n,8.31824495528576,30.4562567119485)+runif(n,6.35639429651506,25.6938382733393)+runif(n,5.88936808050191,22.3388340033204)+runif(n,12.1613860464065,41.1638472480516)+runif(n,1.05648314510071,3.02606249268966)+runif(n,7.93246351055982,72.2363633760808)+runif(n,1.3759715762606,5.26138999580338)+runif(n,16.1362465486085,41.3826210314279)+runif(n,14.3962551848451,38.9850323565646)+runif(n,7.17751587258217,19.5855430806206)+runif(n,0.0389942774402566,0.17834981320444)+runif(n,0.791138675164865,15.9233102061626)
  
  roadpaint3 <- runif(n,221.7651,250.0755) #source 3: road paint shedding
  percent_solids3 <- runif(n,73.4800,93.5200)/100 #source 3: road paint shedding
  degradation_rate3 <- runif(n,44.0000,56.0000)/100 #source 3: road paint shedding
  
  fields4 <- runif(n,50.7600,57.2400) #source 4: artificial turf
  mass_infill4 <- runif(n,154.44,196.56) #source 4: artificial turf
  turf_shedding4 <- runif(n,0.01,0.04) #source 4: artificial turf
  
  plastic_construction5 <- runif(n,90.2373,101.7570) #source 5: construction foam
  proportion_foam5 <- runif(n,0.058,0.15) #source 5: construction foam
  foam_sheddingrate5 <- runif(n,2.992,3.808) #source 5: construction foam
  
  pellet_productionTO6 <- runif(n,146.6185,186.6053) #source 6: pellet loss
  pellet_loss6 <- runif(n,0.0001,0.0004) #source 6: pellet loss
  
  airports7 <- 1 #source 7: airplane tire dust
  aircraft_movements7 <- runif(n,132528.34,137937.66) #source 7: airplane tire dust
  airplane_shedding7 <- runif(n,244.64,311.36) #source 7: airplane tire dust
  
  households8 <- runif(n,1108313.58,1249800.42) #source 8: laundry washing 
  ownership_rate8 <- runif(n, 75.68, 96.32)/100 #source 8: laundry washing 
  wash_cycles8 <- runif(n,192.72, 245.28) #source 8: laundry washing 
  loadperwash8 <- runif(n,3,4) #source 8: laundry washing 
  laundry_sheddingrate8 <- runif(n,124,308) #source 8: laundry washing 
  percent_synthetic8 <- runif(n, 29.92, 38.08)/100 #source 8: laundry washing 
  WWTP_efficiency8 <- rnorm(n,0.989025,0.007325924) #source 8: laundry washing 
  
  households9 <- runif(n,1108313.58,1249800.42) #source 9: dryer vent emissions
  ownership_rate9 <- runif(n, 75.68, 96.32)/100 #source 9: dryer vent emissions
  dry_cycles9 <- runif(n,192.72,245.28) #source 9: dryer vent emissions
  loadperdry9 <- rnorm(n,0.438,0.017) #source 9: dryer vent emissions
  shedding_dryer9 <- rnorm(n,18,8) #source 9: dryer vent emissions
  percent_synthetic9 <- runif(n,29.92,38.08)/100 #source 9: dryer vent emissions
  
  households10 <- runif(n,1108313.58,1249800.42) #source 10: vehicle tire dust
  vehicles_perhousehold10 <- runif(n,1.078,1.122) #source 10: vehicle tire dust
  km_per_year10 <- runif(n,14080,17920) #source 10: vehicle tire dust
  tire_shedding10 <- rtri(n,0.05,0.25,0.1) #source 10: vehicle tire dust
  
  fishers11 <- runif(n, 103087.92,116248.08) #source 11: derelict fishing gear
  mass_fishinggear11 <- runif(n,1349.8160,1522.1329) #source 11: derelict fishing gear
  fishgear_lossrate11 <- runif(n,0.01,0.1) #source 11: derelict fishing gear
  
  aquaticvessels12 <- runif(n,6730.4,7589.6) #source 12: paint shedding from aquatic vessels
  ships12 <- runif(n, 150.4,169.6) #source 12: paint shedding from aquatic vessels
  litres_per_vessel12 <- runif(n,2,2.5) #source 12: paint shedding from aquatic vessels
  kg_per_litre12 <- runif(n,1,1.3) #source 12: paint shedding from aquatic vessels
  percentsolids12 <- runif(n,44,56)/100 #source 12: paint shedding from aquatic vessels
  sheddingrate12 <- runif(n, 0.88,1.12)/100 #source 12: paint shedding from aquatic vessels

  sum <- (housesTO_1*exterior_surface_area1*proportion_painted*mass_paint_area_1*kg_gallon_paint1*percent_solids1*house_shedding_rate1/1000)+(littering_rate1 + littering_rate2)+(roadpaint3*percent_solids3*degradation_rate3)+(fields4*mass_infill4*turf_shedding4)+(plastic_construction5*proportion_foam5*foam_sheddingrate5)+(pellet_productionTO6*pellet_loss6*1000)+(airports7*aircraft_movements7*airplane_shedding7/10^9)+(households8*ownership_rate8*wash_cycles8*loadperwash8*laundry_sheddingrate8*percent_synthetic8*(1-WWTP_efficiency8)/10^9)+(households9*ownership_rate9*dry_cycles9*loadperdry9*shedding_dryer9*percent_synthetic9/10^9)+(households10*vehicles_perhousehold10*km_per_year10*tire_shedding10)/10^9+(fishers11*mass_fishinggear11*fishgear_lossrate11/10^6)+(aquaticvessels12*litres_per_vessel12*kg_per_litre12*percentsolids12*sheddingrate12)/1000+(ships12*litres_per_vessel12*kg_per_litre12*percentsolids12*sheddingrate12*0.99)/1000
  #return result
  return(list("sum"=sum))
}
#add uncertainty pdf parameters to the list
param_list = list("n" = 1) #number of arguments matches that of func
#run the actual Monte Carlo simulations
set.seed(1234) #used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
MC_result <- MonteCarlo(func = inventory, nrep = 10000, param_list = param_list)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df <- MakeFrame(MC_result)
head(df)
#tbl <- tbl_df(df) #a subclass of data.frame; tibbles are the central data structure for the set of packages known as the tidyverse
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth = 2, color = "pink",size=5) + labs(title = "Histogram of inventory sums",x="emissions (T)",y="Frequency")
#making histogram of sum, and adding vertical means for mean and 95% CI
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
#+ theme(legend.position="top") #change legend position

#calculating 95% CIs for the histogram
norm.interval = function(data, variance = var(data), conf.level = 0.95)
{
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = mean(data)
  sdx = sqrt(variance/length(data))
  c(xbar - z*sdx, xbar + z*sdx)
}
norm.interval(df$sum)
summary(df$sum)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#Monte Carlo simulations for individual sources
##source 1: house paint shedding######1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111
source1 <- function(n) {
  #sample from the pdfs
  housesTO_1 <- runif(n,381071.3000,429718.7000) #source 1: house paint shedding
  exterior_surface_area1 <- rtri(n,2530,4382,3578) #confirmed #source 1: house paint shedding
  proportion_painted <- rtri(n,0.25,1,0.5) #NEW!
  mass_paint_area_1 <- runif(n,0.0088,0.0112) #source 1: house paint shedding
  kg_gallon_paint1 <- runif(n,2.72155,5.44311) #source 1: house paint shedding
  percent_solids1 <- runif(n,17.6000,22.4000)/100 #source 1: house paint shedding
  house_shedding_rate1 <- runif(n,0.01,0.04) #confirmed #source 1: house paint shedding
  #do the calculations
  sum <- (housesTO_1*exterior_surface_area1*proportion_painted*mass_paint_area_1*kg_gallon_paint1*percent_solids1*house_shedding_rate1/1000)
  #return result
  return(list("sum"=sum))
}
param_list = list("n" = 1) #number of arguments matches that of func
set.seed(1234) #used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
MC_result <- MonteCarlo(func = source1, nrep = 10000, param_list = param_list)
df <- MakeFrame(MC_result)
head(df)
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth = 1, color = "darksalmon") + labs(title = "Histogram of paint emissions from the exteriors of houses",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
summary(df$sum)
norm.interval(df$sum)
#
#
#
##source 2 littering or MMW source grouping###22222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222222
source2_littering <- function(n) #annual plastic litter emissions from TO
{
  #source 2 MMW or littering: using roads + sidewalks
  littering_rate1 <- runif(n,0.391438402177165,0.983602320576728)+runif(n,3.94655314204831,13.6422558847085)+runif(n,3.22745344538179,37.5565601021775)+runif(n,5.63778203324361,16.9081814240018)+runif(n,3.94694692611396,17.2594602897542)+runif(n,3.41760562393187,10.1794553811305)+runif(n,2.4882593515349,7.6158459166319)+runif(n,1.82799813120172,4.31025310236055)+runif(n,1.27903356356134,3.12534044988941)+runif(n,9.72955657205548,41.5018912339657)+runif(n,2.56475741790535,7.36889228061836)+runif(n,14.4560868079744,71.5038296980515)+runif(n,2.75594543011381,10.7029464154899)+runif(n,4.3160876458958,12.8098240743688)+runif(n,4.50047268166171,14.8135453900702)+runif(n,1.04995958405722,2.56233592893723)+runif(n,5.50366376082537,15.4600287445741)+runif(n,6.79237301432807,18.0815849309858)+runif(n,1.48871439755562,10.9460940344972)+runif(n,5.11817721462802,14.21037087283)+runif(n,2.95485915530633,7.31429191917501)+runif(n,10.1736610725576,28.9354438082811)+runif(n,6.05005206871066,22.3075551039628)+runif(n,3.09635391110603,11.305978704994)+runif(n,5.13583104490466,16.3690892194057)+runif(n,5.60025685718506,14.4346277007538)+runif(n,3.37054803767564,16.4835643523029)+runif(n,0.85352412720543,2.35244766824465)+runif(n,1.1830735668227,14.9633706334029)+runif(n,5.89950018795366,17.3480972585549)+runif(n,2.76696134208546,6.79733674532405)+runif(n,31.0483421709326,87.8437334878306)+runif(n,10.1168861607408,30.888188820349)+runif(n,8.6053402940873,29.0095092854447)+runif(n,13.3535149593952,44.2488764234017)+runif(n,7.32305055260027,21.2986935266053)+runif(n,10.9384809008475,30.6285293981027)+runif(n,1.72790454874013,6.3608639076703)+runif(n,0.29593682892258,0.749948011992112)+runif(n,0.0397287436913667,0.357165077664186)+runif(n,25.993051767541,63.2364753456513)+runif(n,3.17814633554849,11.1788252269504)+runif(n,0.0412789434175218,0.104033283107479)+runif(n,1.53135688506845,4.71925790528699)+runif(n,0.00467649805135976,0.0167001743268518)+runif(n,7.33517300414959,20.8169876014024)+runif(n,10.0921877935971,28.3991163791631)+runif(n,0.573225246687335,3.2487162699874)+runif(n,2.98006540016769,8.25268494091298)+runif(n,0.790726238906733,2.44027421744929)+runif(n,6.2059449699211,16.4550159483781)+runif(n,1.22284195418513,4.9451771623867)+runif(n,19.5056207366144,151.490046805987)+runif(n,1.0382421327266,5.64552833375915)+runif(n,4.66554571610607,42.5926036685556)+runif(n,2.4811656546548,6.80256162512416)+runif(n,2.24320482602513,22.8772118251065)+runif(n,2.37512502320795,10.6800248321611)+runif(n,3.7228435410862,17.49640326531)+runif(n,2.7963808390879,10.9791287121239)+runif(n,4.5022839250348,14.8818659086841)+runif(n,6.96428395241635,22.9423103508813)+runif(n,2.22767763036722,16.9865340556003)+runif(n,3.23102527466658,14.4142370984464)+runif(n,10.6138892581208,90.0350801868732)+runif(n,11.9616807010173,37.7513845086908)+runif(n,2.18333761470522,9.35964365463246)+runif(n,5.81416622030032,20.3917426204488)+runif(n,1.14272053412952,5.4131709003908)+runif(n,7.96412234611592,23.9831299531262)+runif(n,0.821764031592406,3.14296035488058)+runif(n,17.0417687547552,63.3587514819735)+runif(n,51.47123996263,293.284893289207)+runif(n,35.0051314196974,135.689596573419)+runif(n,4.27524041044815,17.2423836051229)+runif(n,3.2267402343768,10.3192067283232)+runif(n,7.7634764692108,47.6019641649207)+runif(n,21.4117552458751,386.484971067772)+runif(n,3.796106640093,21.3386264608956)+runif(n,5.24120046741943,31.6541557314454)+runif(n,5.0983950075135,29.1869170663506)+runif(n,6.59970645990459,46.2179391888053)
  littering_rate2 <- runif(n,37.4938414007433,157.638077393861)+runif(n,1.21582122833157,5.31655827890283)+runif(n,0.379917101812949,1.59126112189875)+runif(n,1.55488291217416,9.91026820676877)+runif(n,3.08583810804767,118.146412052697)+runif(n,9.94596295781867,48.310270170745)+runif(n,10.2696382493446,41.1415058264323)+runif(n,1.83273250716997,5.44162416194681)+runif(n,2.21273872436956,7.25058335722598)+runif(n,4.52396169216355,19.8614170556771)+runif(n,5.34458674203312,46.2332767202556)+runif(n,6.07263439575046,24.5741990853116)+runif(n,9.49878947929143,34.9009840349431)+runif(n,7.45328738226053,22.6236404732207)+runif(n,2.3733376650426,14.0903466378138)+runif(n,8.84158586131609,45.7858610868298)+runif(n,2.07870764525775,9.92385184034394)+runif(n,1.73849260028415,10.6634360882762)+runif(n,0.950959001971771,6.15973106160371)+runif(n,2.68225253259667,11.0888510346556)+runif(n,0.0688264783334576,0.490838929344842)+runif(n,3.61562691700342,12.405626672217)+runif(n,10.3171105159518,33.9461318360796)+runif(n,20.8592151447112,140.719730814419)+runif(n,15.6187615746062,107.76773685327)+runif(n,29.9567992897924,195.758282016638)+runif(n,11.673138669368,33.0573845936565)+runif(n,3.3832796026396,18.7146726647915)+runif(n,2.41807492796867,13.4370212145566)+runif(n,1.49002839095732,9.03042284490711)+runif(n,17.7709956405927,57.5221268552684)+runif(n,7.60376827966723,26.9712058733691)+runif(n,45.3237775142256,136.210386760317)+runif(n,6.54186291752882,42.4503387584748)+runif(n,20.2764798063948,330.525516294046)+runif(n,59.7938866096068,204.998967688084)+runif(n,7.08883391865073,24.0549788662615)+runif(n,13.1818391752305,126.695397524458)+runif(n,7.77557562214391,20.6005694485165)+runif(n,0.732062163188044,3.46119397967027)+runif(n,4.40817589554693,22.9389690146355)+runif(n,2.67275212956516,26.2814732743399)+runif(n,5.02689819555045,67.6784186092757)+runif(n,2.81232766876368,10.489042111765)+runif(n,3.92575432267507,16.6805280586959)+runif(n,2.34146369553851,7.99813612938933)+runif(n,8.31824495528576,30.4562567119485)+runif(n,6.35639429651506,25.6938382733393)+runif(n,5.88936808050191,22.3388340033204)+runif(n,12.1613860464065,41.1638472480516)+runif(n,1.05648314510071,3.02606249268966)+runif(n,7.93246351055982,72.2363633760808)+runif(n,1.3759715762606,5.26138999580338)+runif(n,16.1362465486085,41.3826210314279)+runif(n,14.3962551848451,38.9850323565646)+runif(n,7.17751587258217,19.5855430806206)+runif(n,0.0389942774402566,0.17834981320444)+runif(n,0.791138675164865,15.9233102061626)
  
  #do the calculations
  sum <- littering_rate1 + littering_rate2 #source 2: littering, illegal dumping, inadequately managed waste, and garbage day overflow
  #return result
  return(list("sum"=sum))
}
param_littering = list("n" = 1) #number of arguments matches that of func
#run the actual Monte Carlo simulations
#ALWAYS RUN SET.SEED BEFORE MONTE CARLO
set.seed(1234)
MC_littering <- MonteCarlo(func = source2_littering, nrep = 10000, param_list = param_littering)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_littering <- MakeFrame(MC_littering)
head(df_littering)
p <- ggplot(df_littering, aes(x=sum)) + geom_histogram(binwidth = 0.5, color = "orangered3") + labs(title = "Histogram of neighbourhood littering sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_littering$sum),color="blue",linetype="dashed",size=1) 
summary(df_littering$sum)
norm.interval(df_littering$sum)
#
#
#
##source 3: road paint shedding###3333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333333
source3 <- function(n) {
  #sample from the pdfs
  roadpaint3 <- runif(n,221.7651,250.0755) #source 3: road paint shedding
  percent_solids3 <- runif(n,73.4800,93.5200)/100 #source 3: road paint shedding
  degradation_rate3 <- runif(n,44.0000,56.0000)/100 #source 3: road paint shedding
  #do the calculations
  sum <- (roadpaint3*percent_solids3*degradation_rate3)
  #return result
  return(list("sum"=sum))
}
param_source3 = list("n" = 1) #number of arguments matches that of func
#run the actual Monte Carlo simulations
#ALWAYS RUN SET.SEED BEFORE MONTE CARLO
set.seed(1234)
MC_source3 <- MonteCarlo(func = source3, nrep = 10000, param_list = param_source3)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_source3 <- MakeFrame(MC_source3)
head(df_source3)
p <- ggplot(df_source3, aes(x=sum)) + geom_histogram(binwidth = 0.1, color = "darksalmon") + labs(title = "Histogram of road paint emissions",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_source3$sum),color="blue",linetype="dashed",size=1) 
summary(df_source3$sum)
norm.interval(df_source3$sum)
#
#  
#  
#  
##source 4: artificial turf or recreational source grouping###44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444
source4 <- function(n) {
  #sample from the pdfs
  fields4 <- runif(n,50.7600,57.2400) #source 4: artificial turf
  mass_infill4 <- runif(n,154.44,196.56) #source 4: artificial turf
  turf_shedding4 <- runif(n,0.01,0.04) #source 4: artificial turf
  #do the calculations
  sum <- (fields4*mass_infill4*turf_shedding4)
  #return result
  return(list("sum"=sum))
}
param_list = list("n" = 1) #number of arguments matches that of func
set.seed(1234) #used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
MC_result <- MonteCarlo(func = source4, nrep = 10000, param_list = param_list)
df <- MakeFrame(MC_result)
head(df)
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth=0.5,color="gold") + labs(title = "Histogram of emissions from artificial turf",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
summary(df$sum)
norm.interval(df$sum)
#
#
#
##source 5: construction foam###55555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555555
source5 <- function(n) {
  #sample from the pdfs
  plastic_construction5 <- runif(n,90.2373,101.7570) #source 5: construction foam
  proportion_foam5 <- runif(n,0.058,0.15) #source 5: construction foam
  foam_sheddingrate5 <- runif(n,2.992,3.808) #source 5: construction foam
  #do the calculations
  sum <- (plastic_construction5*proportion_foam5*foam_sheddingrate5)
  #return result
  return(list("sum"=sum))
}
param_list = list("n" = 1) #number of arguments matches that of func
set.seed(1234) #used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
MC_result <- MonteCarlo(func = source5, nrep = 10000, param_list = param_list)
df <- MakeFrame(MC_result)
head(df)
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth = 0.05, color = "green4") + labs(title = "Histogram of emissions from construction",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
summary(df$sum)
norm.interval(df$sum)
#
#
##source 6: pellet loss or industrial source grouping###6666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666666
source6 <- function(n) {
  #sample from the pdfs
  pellet_productionTO6 <- runif(n,146.6185,186.6053) #source 6: pellet loss
  pellet_loss6 <- runif(n,0.0001,0.0004) #source 6: pellet loss
  #do the calculations
  sum <- (pellet_productionTO6*pellet_loss6*1000)
  #return result
  return(list("sum"=sum))
}
param_list = list("n" = 1) #number of arguments matches that of func
set.seed(1234) #used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
MC_result <- MonteCarlo(func = source6, nrep = 10000, param_list = param_list)
df <- MakeFrame(MC_result)
head(df)
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth = 0.1, color = "green4") + labs(title = "Histogram of pellet losses",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
summary(df$sum)
norm.interval(df$sum)
#
#
##source 7: airplane tire dust###77777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777777
source7 <- function(n) {
  #sample from the pdfs
  airports7 <- 1 #source 7: airplane tire dust
  aircraft_movements7 <- runif(n,132528.34,137937.66) #source 7: airplane tire dust
  airplane_shedding7 <- runif(n,244.64,311.36) #source 7: airplane tire dust
  #do the calculations
  sum <- (airports7*aircraft_movements7*airplane_shedding7/10^9)
  #return result
  return(list("sum"=sum))
}
param_list = list("n" = 1) #number of arguments matches that of func
set.seed(1234) #used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
MC_result <- MonteCarlo(func = source7, nrep = 10000, param_list = param_list)
df <- MakeFrame(MC_result)
head(df)
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth=0.00001,color="darkorchid4") + labs(title = "Histogram of tire dust emissions from airplanes",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
summary(df$sum)
norm.interval(df$sum)
#
#
##source 8: laundry washing###888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888888
source8 <- function(n) {
  #sample from the pdfs
  households8 <- runif(n,1108313.58,1249800.42) #source 8: laundry washing 
  ownership_rate8 <- runif(n, 75.68, 96.32)/100 #source 8: laundry washing 
  wash_cycles8 <- runif(n,192.72, 245.28) #source 8: laundry washing 
  loadperwash8 <- runif(n,3,4) #source 8: laundry washing 
  laundry_sheddingrate8 <- runif(n,124,308) #source 8: laundry washing 
  percent_synthetic8 <- runif(n, 29.92, 38.08)/100 #source 8: laundry washing 
  #WWTP_efficiency8 <- runif(n,0.983,0.999)
  WWTP_efficiency8 <- rnorm(n,0.989025,0.007325924) #source 8: laundry washing
  #do the calculations
  sum <- (households8*ownership_rate8*wash_cycles8*loadperwash8*laundry_sheddingrate8*percent_synthetic8*(1-WWTP_efficiency8)/10^9)
    #return result
    return(list("sum"=sum))
}
param_list = list("n" = 1) #number of arguments matches that of func
set.seed(1234) #used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
MC_result <- MonteCarlo(func = source8, nrep = 50000, param_list = param_list)
df <- MakeFrame(MC_result)
head(df)
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth = 0.01, color="dodgerblue4") + labs(title = "Histogram of fiber emissions from laundry washing",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
summary(df$sum)
norm.interval(df$sum)
#
#
##source 9: dryer vent emissions###99999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999
source9 <- function(n) {
  #sample from the pdfs
  households9 <- runif(n,1108313.58,1249800.42) #source 9: dryer vent emissions
  ownership_rate9 <- runif(n, 75.68, 96.32)/100 #source 9: dryer vent emissions
  dry_cycles9 <- runif(n,192.72,245.28) #source 9: dryer vent emissions
  loadperdry9 <- rnorm(n,0.438,0.017) #source 9: dryer vent emissions
  shedding_dryer9 <- rnorm(n,18,8) #source 9: dryer vent emissions
  percent_synthetic9 <- runif(n,29.92,38.08)/100 #source 9: dryer vent emissions
  #do the calculations
  sum <- (households9*ownership_rate9*dry_cycles9*loadperdry9*shedding_dryer9*percent_synthetic9/10^9)
    #return result
    return(list("sum"=sum))
}
param_list = list("n" = 1) #number of arguments matches that of func
set.seed(1234) #used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
MC_result <- MonteCarlo(func = source9, nrep = 10000, param_list = param_list)
df <- MakeFrame(MC_result)
head(df)
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth = 0.005, color = "dodgerblue4") + labs(title = "Histogram of fiber emissions from dryer vents",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
summary(df$sum)
norm.interval(df$sum)
#
#
##source 10: vehicle tire dust###10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10.10
source10 <- function(n) {
  #sample from the pdfs
  households10 <- runif(n,1108313.58,1249800.42) #source 10: vehicle tire dust
  vehicles_perhousehold10 <- runif(n,1.078,1.122) #source 10: vehicle tire dust
  km_per_year10 <- runif(n,14080,17920) #source 10: vehicle tire dust
  tire_shedding10 <- rtri(n,0.05,0.25,0.1) #source 10: vehicle tire dust
  #do the calculations
  sum <- (households10*vehicles_perhousehold10*km_per_year10*tire_shedding10)/10^9
    #return result
    return(list("sum"=sum))
}
param_list = list("n" = 1) #number of arguments matches that of func
set.seed(1234) #used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
MC_result <- MonteCarlo(func = source10, nrep = 10000, param_list = param_list)
df <- MakeFrame(MC_result)
head(df)
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth = 0.01, color = "darkorchid4") + labs(title = "Histogram of tire dust emissions from vehicles",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
summary(df$sum)
norm.interval(df$sum)
#
#
##source 11: derelict fishing gear####11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11.11
source11 <- function(n) {
  #sample from the pdfs
  fishers11 <- runif(n, 103087.92,116248.08) #source 11: derelict fishing gear
  mass_fishinggear11 <- runif(n,1349.8160,1522.1329) #source 11: derelict fishing gear
  fishgear_lossrate11 <- runif(n,0.01,0.1) #source 11: derelict fishing gear
  #do the calculations
  sum <- (fishers11*mass_fishinggear11*fishgear_lossrate11/10^6)
    #return result
    return(list("sum"=sum))
}
param_list = list("n" = 1) #number of arguments matches that of func
set.seed(1234) #used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
MC_result <- MonteCarlo(func = source11, nrep = 10000, param_list = param_list)
df <- MakeFrame(MC_result)
head(df)
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth = 0.03, color = "skyblue") + labs(title = "Histogram of plastic emissions due to derelict fishing gear",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
summary(df$sum)
norm.interval(df$sum)
#
#
##source 12: paint shedding from aquatic vessels###12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12.12
source12 <- function(n) {
  #sample from the pdfs
  aquaticvessels12 <- runif(n,6730.4,7589.6) #source 12: paint shedding from aquatic vessels
  ships12 <- runif(n, 150.4,169.6) #source 12: paint shedding from aquatic vessels
  litres_per_vessel12 <- runif(n,2,2.5) #source 12: paint shedding from aquatic vessels
  kg_per_litre12 <- runif(n,1,1.3) #source 12: paint shedding from aquatic vessels
  percentsolids12 <- runif(n,44,56)/100 #source 12: paint shedding from aquatic vessels
  sheddingrate12 <- runif(n, 0.88,1.12)/100 #source 12: paint shedding from aquatic vessels
  #do the calculations
  sum <- (aquaticvessels12*litres_per_vessel12*kg_per_litre12*percentsolids12*sheddingrate12)/1000+(ships12*litres_per_vessel12*kg_per_litre12*percentsolids12*sheddingrate12*0.99)/1000
    #return result
    return(list("sum"=sum))
}
param_list = list("n" = 1) #number of arguments matches that of func
set.seed(1234) #used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
MC_result <- MonteCarlo(func = source12, nrep = 10000, param_list = param_list)
df <- MakeFrame(MC_result)
head(df)
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth = 0.0001, color = "darksalmon") + labs(title = "Histogram of paint emissions from aquatic vessels",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
summary(df$sum)
norm.interval(df$sum)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#Monte Carlo simulations for subgroups of sources
###########################################1. terrestrial#############################################
#defining the terrestrial function
terrestrial <- function(n) {
  #terrestrial function -> paint + tire dust + clothes + recreational + industrial + MMW 
  housesTO_1 <- runif(n,381071.3000,429718.7000) #source 1: house paint shedding
  exterior_surface_area1 <- rtri(n,2530,4382,3578) #confirmed #source 1: house paint shedding
  proportion_painted <- rtri(n,0.25,1,0.5) #NEW!
  mass_paint_area_1 <- runif(n,0.0088,0.0112) #source 1: house paint shedding
  kg_gallon_paint1 <- runif(n,2.72155,5.44311) #source 1: house paint shedding
  percent_solids1 <- runif(n,17.6000,22.4000)/100 #source 1: house paint shedding
  house_shedding_rate1 <- runif(n,0.01,0.04) #confirmed #source 1: house paint shedding
  
  #source 2 MMW or littering, one pdf for each TO neighbourhood. There are 140 neighbourhoods in total.
  littering_rate1 <- runif(n,397.502016351606,927.504704820413)+runif(n,10.699751934427,24.9660878469963)+runif(n,5.25291921691202,12.2568115061281)+runif(n,14.3468737680014,33.4760387920033)+runif(n,44.0317698128369,102.740796229953)+runif(n,87.5784601297096,204.349740302656)+runif(n,87.4442835998641,204.036661733016)+runif(n,23.170655956716,54.0648638990039)+runif(n,27.1178091289184,63.2748879674762)+runif(n,76.105736848521,177.580052646549)+runif(n,53.8527646935297,125.656450951569)+runif(n,53.9091175020308,125.787940838072)+runif(n,78.6687625281949,183.560445899121)+runif(n,65.5672515608754,152.990253642043)+runif(n,25.4930739569539,59.4838392328925)+runif(n,100.767296016416,235.12369070497)+runif(n,18.4484532789611,43.0463909842425)+runif(n,13.8434926641646,32.3014828830507)+runif(n,9.22979339246122,21.5361845824095)+runif(n,25.3247365955849,59.0910520563649)+runif(n,0.792500360263126,1.84916750728063)+runif(n,49.6218438350182,115.784302281709)+runif(n,128.416867811906,299.639358227781)+runif(n,179.340080547651,418.460187944519)+runif(n,157.264831149923,366.951272683153)+runif(n,338.814665474202,790.567552773138)+runif(n,96.0029822096063,224.006958489081)+runif(n,39.4015812757318,91.9370229767076)+runif(n,33.2507379417433,77.5850551974011)+runif(n,14.6345877428065,34.1473713998818)+runif(n,142.970205149489,333.597145348807)+runif(n,56.4902819870618,131.810657969811)+runif(n,445.626740241009,1039.79572722902)+runif(n,85.7044460435094,199.977040768189)+runif(n,186.598856854706,435.39733266098)+runif(n,518.85964973926,1210.67251605827)+runif(n,53.4324247878053,124.675657838212)+runif(n,145.715947500231,340.003877500539)+runif(n,76.1750962319646,177.741891207917)+runif(n,5.87689199085338,13.7127479786579)+runif(n,67.715109454386,158.001922060234)+runif(n,23.5442747685615,54.9366411266436)+runif(n,46.789122862598,109.174620012729)+runif(n,24.1984975063152,56.4631608480688)+runif(n,37.7711367238996,88.1326523557657)+runif(n,15.1262365802622,35.2945520206118)+runif(n,49.6597137986795,115.872665530252)+runif(n,53.3249582575935,124.424902601051)+runif(n,60.2420731627258,140.564837379694)+runif(n,120.505945168513,281.18053872653)+runif(n,6.6116275828391,15.4271310266246)+runif(n,73.837120499791,172.286614499512)+runif(n,7.05387859001614,16.459050043371)+runif(n,152.862844527736,356.679970564717)+runif(n,108.483972658161,253.129269535709)+runif(n,50.3381293463959,117.45563514159)+runif(n,0.205547455631332,0.479610729806442)+runif(n,5.71137111939742,13.3265326119273)+runif(n,1.37417075225264,3.20639842192283)+runif(n,18.7099874432076,43.6566373674844)+runif(n,17.8361868295533,41.6177692689577)+runif(n,27.5629563424926,64.3135647991495)+runif(n,19.5493134016674,45.6150646038905)+runif(n,14.7963878723435,34.5249050354683)+runif(n,10.2952920406658,24.0223480948869)+runif(n,8.11850407453677,18.9431761739191)+runif(n,5.52638218410829,12.8948917629193)+runif(n,41.9536434816069,97.8918347904161)
  littering_rate2 <- runif(n,10.9609059132375,25.5754471308874)+runif(n,130.679978368462,304.919949526412)+runif(n,15.2417949845202,35.5641882972139)+runif(n,12.9980710122798,30.3288323619862)+runif(n,12.7518050143346,29.754211700114)+runif(n,3.15804274856005,7.36876641330678)+runif(n,16.4737273650244,38.4386971850569)+runif(n,12.6457252433969,29.5066922345927)+runif(n,17.989435443429,41.9753493680009)+runif(n,14.9569453117237,34.8995390606886)+runif(n,9.90831228205603,23.1193953247974)+runif(n,43.4876961484012,101.471291012936)+runif(n,21.0045544527542,49.0106270564264)+runif(n,13.0375810645145,30.4210224838671)+runif(n,18.5929670583788,43.3835898028838)+runif(n,17.6283833027628,41.1328943731132)+runif(n,16.6421846603138,38.8317642073988)+runif(n,3.74604733539388,8.74077711591906)+runif(n,8.23463908389331,19.2141578624177)+runif(n,37.1319225175097,86.6411525408559)+runif(n,15.0517496137019,35.1207490986378)+runif(n,176.327907422699,411.431783986298)+runif(n,59.0790495497573,137.8511156161)+runif(n,40.339494000333,94.125486000777)+runif(n,63.098986987505,147.230969637512)+runif(n,35.5944861667461,83.0538010557409)+runif(n,38.8947789776547,90.7544842811943)+runif(n,7.69375626207439,17.9520979448402)+runif(n,1.78627591400487,4.16797713267802)+runif(n,0.249953927553911,0.583225830959127)+runif(n,172.90016957494,403.433729008193)+runif(n,11.7075782366884,27.3176825522728)+runif(n,0.37547105142338,0.876099119987886)+runif(n,9.84605265381454,22.9741228589006)+runif(n,0.0260460980053165,0.0607742286790718)+runif(n,26.0194517898957,60.7120541764232)+runif(n,90.0037088200734,210.008653913505)+runif(n,3.39920621432124,7.93148116674957)+runif(n,19.7663881727489,46.1215724030808)+runif(n,4.96474988577366,11.5844164001385)+runif(n,33.3715153970808,77.8668692598552)+runif(n,9.43728927544159,22.020341642697)+runif(n,196.247239237829,457.910224888267)+runif(n,11.4161974690749,26.6377940945082)+runif(n,34.2995218270043,80.0322175963435)+runif(n,20.9106498425426,48.7915162992661)+runif(n,27.5898766023959,64.3763787389238)+runif(n,15.9260039576986,37.1606759012967)+runif(n,28.5798628283483,66.6863465994794)+runif(n,23.4233260642563,54.6544274832648)+runif(n,39.0736711421601,91.1718993317068)+runif(n,62.2745421523632,145.307265022181)+runif(n,15.6426105382613,36.4994245892763)+runif(n,29.3794554464414,68.5520627083634)+runif(n,175.125038299948,408.625089366546)+runif(n,113.051689236908,263.78727488612)+runif(n,21.679214445773,50.5848337068036)+runif(n,55.2143509938747,128.833485652374)+runif(n,9.67816959812832,22.5823957289661)+runif(n,67.4466334115827,157.37547796036)+runif(n,8.56377336632884,19.9821378547673)+runif(n,144.626482171953,337.461791734557)+runif(n,1100.87407187605,2568.70616771079)+runif(n,286.445572316547,668.373002071942)+runif(n,40.0349676836421,93.4149245951649)+runif(n,33.5314929968179,78.2401503259085)+runif(n,117.088766735234,273.207122382213)+runif(n,269.956327771447,629.898098133376)+runif(n,32.4210625599399,75.649145973193)+runif(n,47.9275917100397,111.831047323426)+runif(n,57.1476876922562,133.344604615264)+runif(n,84.5461567814468,197.274365823376)
  
  roadpaint3 <- runif(n,221.7651,250.0755) #source 3: road paint shedding
  percent_solids3 <- runif(n,73.4800,93.5200)/100 #source 3: road paint shedding
  degradation_rate3 <- runif(n,44.0000,56.0000)/100 #source 3: road paint shedding
  
  fields4 <- runif(n,50.7600,57.2400) #source 4: artificial turf
  mass_infill4 <- runif(n,154.44,196.56) #source 4: artificial turf
  turf_shedding4 <- runif(n,0.01,0.04) #source 4: artificial turf
  
  plastic_construction5 <- runif(n,90.2373,101.7570) #source 5: construction foam
  proportion_foam5 <- runif(n,0.058,0.15) #source 5: construction foam
  foam_sheddingrate5 <- runif(n,2.992,3.808) #source 5: construction foam
  
  pellet_productionTO6 <- runif(n,146.6185,186.6053) #source 6: pellet loss
  pellet_loss6 <- runif(n,0.0001,0.0004) #source 6: pellet loss
  
  airports7 <- 1 #source 7: airplane tire dust
  aircraft_movements7 <- runif(n,132528.34,137937.66) #source 7: airplane tire dust
  airplane_shedding7 <- runif(n,244.64,311.36) #source 7: airplane tire dust
  
  households8 <- runif(n,1108313.58,1249800.42) #source 8: laundry washing 
  ownership_rate8 <- runif(n, 75.68, 96.32)/100 #source 8: laundry washing 
  wash_cycles8 <- runif(n,192.72, 245.28) #source 8: laundry washing 
  loadperwash8 <- runif(n,3,4) #source 8: laundry washing 
  laundry_sheddingrate8 <- runif(n,124,308) #source 8: laundry washing 
  percent_synthetic8 <- runif(n, 29.92, 38.08)/100 #source 8: laundry washing 
  WWTP_efficiency8 <- rnorm(n,0.989025,0.007325924) #source 8: laundry washing 
  
  households9 <- runif(n,1108313.58,1249800.42) #source 9: dryer vent emissions
  ownership_rate9 <- runif(n, 75.68, 96.32)/100 #source 9: dryer vent emissions
  dry_cycles9 <- runif(n,192.72,245.28) #source 9: dryer vent emissions
  loadperdry9 <- rnorm(n,0.438,0.017) #source 9: dryer vent emissions
  shedding_dryer9 <- rnorm(n,18,8) #source 9: dryer vent emissions
  percent_synthetic9 <- runif(n,29.92,38.08)/100 #source 9: dryer vent emissions
  
  households10 <- runif(n,1108313.58,1249800.42) #source 10: vehicle tire dust
  vehicles_perhousehold10 <- runif(n,1.078,1.122) #source 10: vehicle tire dust
  km_per_year10 <- runif(n,14080,17920) #source 10: vehicle tire dust
  tire_shedding10 <- rtri(n,0.05,0.25,0.1) #source 10: vehicle tire dust
  #sample from the pdfs
  
  #do the calculations
  sum_terrestrial <- (housesTO_1*exterior_surface_area1*proportion_painted*mass_paint_area_1*kg_gallon_paint1*percent_solids1*house_shedding_rate1/1000)+(littering_rate1 + littering_rate2)+(roadpaint3*percent_solids3*degradation_rate3)+(fields4*mass_infill4*turf_shedding4)+(plastic_construction5*proportion_foam5*foam_sheddingrate5)+(pellet_productionTO6*pellet_loss6*1000)+(airports7*aircraft_movements7*airplane_shedding7/10^9)+(households8*ownership_rate8*wash_cycles8*loadperwash8*laundry_sheddingrate8*percent_synthetic8*(1-WWTP_efficiency8)/10^9)+(households9*ownership_rate9*dry_cycles9*loadperdry9*shedding_dryer9*percent_synthetic9/10^9)+(households10*vehicles_perhousehold10*km_per_year10*tire_shedding10)/10^9
  #return result
  return(list("sum"=sum_terrestrial))
}
#add uncertainty pdf parameters to the list
param_terrestrial = list("n" = 1) #number of arguments matches that of func
#run the actual Monte Carlo simulations
set.seed(1234) #ALWAYS RUN SET.SEED BEFORE MONTE CARLO
MC_terrestrial <- MonteCarlo(func = terrestrial, nrep = 10000, param_list = param_terrestrial)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_terrestrial <- MakeFrame(MC_terrestrial)
head(df_terrestrial)
p <- ggplot(df_terrestrial, aes(x=sum)) + geom_histogram(binwidth = 1, color = "black") + labs(title = "Histogram of terrestrial sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_terrestrial$sum),color="blue",linetype="dashed",size=1) 
summary(df_terrestrial$sum)
norm.interval(df_terrestrial$sum)
#
#
#
#
#
#
#
#
#
################################################2. aquatic#################################################################################
aquatic <- function(n) {
  #sample from the pdfs
  fishers11 <- runif(n, 103087.92,116248.08) #source 11: derelict fishing gear
  mass_fishinggear11 <- runif(n,1349.8160,1522.1329) #source 11: derelict fishing gear
  fishgear_lossrate11 <- runif(n,0.01,0.1) #source 11: derelict fishing gear
  
  aquaticvessels12 <- runif(n,6730.4,7589.6) #source 12: paint shedding from aquatic vessels
  ships12 <- runif(n, 150.4,169.6) #source 12: paint shedding from aquatic vessels
  litres_per_vessel12 <- runif(n,2,2.5) #source 12: paint shedding from aquatic vessels
  kg_per_litre12 <- runif(n,1,1.3) #source 12: paint shedding from aquatic vessels
  percentsolids12 <- runif(n,44,56)/100 #source 12: paint shedding from aquatic vessels
  sheddingrate12 <- runif(n, 0.88,1.12)/100 #source 12: paint shedding from aquatic vessels
  #do the calculations
  sum <- (fishers11*mass_fishinggear11*fishgear_lossrate11/10^6)+(aquaticvessels12*litres_per_vessel12*kg_per_litre12*percentsolids12*sheddingrate12)/1000+(ships12*litres_per_vessel12*kg_per_litre12*percentsolids12*sheddingrate12*0.99)/1000
  #return result
  return(list("sum"=sum))
}
param_list = list("n" = 1) #number of arguments matches that of func
set.seed(1234) #used value from Steph's code -> MUST RUN THIS BEFORE RUNNING MONTE CARLO SIMULATIONS!
MC_result <- MonteCarlo(func = aquatic, nrep = 10000, param_list = param_list)
df <- MakeFrame(MC_result)
head(df)
p <- ggplot(df, aes(x=sum)) + geom_histogram(binwidth = 0.03, color = "skyblue") + labs(title = "Histogram of aquatic emissions",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df$sum),color="blue",linetype="dashed",size=1) 
summary(df$sum)
norm.interval(df$sum)
#
#
#
#
#
#
#
#
#
#
###########################################3. paint#############################################
#defining the paint function
paint <- function(n) { #road markings + building paint + aquatic vessel paint
  #sample from the pdfs
  housesTO_1 <- runif(n,381071.3000,429718.7000) #source 1: house paint shedding
  exterior_surface_area1 <- rtri(n,2530,4382,3578) #confirmed #source 1: house paint shedding
  proportion_painted <- rtri(n,0.25,1,0.5) #NEW!
  mass_paint_area_1 <- runif(n,0.0088,0.0112) #source 1: house paint shedding
  kg_gallon_paint1 <- runif(n,2.72155,5.44311) #source 1: house paint shedding
  percent_solids1 <- runif(n,17.6000,22.4000)/100 #source 1: house paint shedding
  house_shedding_rate1 <- runif(n,0.01,0.04) #confirmed #source 1: house paint shedding
  
  roadpaint3 <- runif(n,221.7651,250.0755) #source 3: road paint shedding
  percent_solids3 <- runif(n,73.4800,93.5200)/100 #source 3: road paint shedding
  degradation_rate3 <- runif(n,44.0000,56.0000)/100 #source 3: road paint shedding
  
  aquaticvessels12 <- runif(n,6730.4,7589.6) #source 12: paint shedding from aquatic vessels
  ships12 <- runif(n, 150.4,169.6) #source 12: paint shedding from aquatic vessels
  litres_per_vessel12 <- runif(n,2,2.5) #source 12: paint shedding from aquatic vessels
  kg_per_litre12 <- runif(n,1,1.3) #source 12: paint shedding from aquatic vessels
  percentsolids12 <- runif(n,44,56)/100 #source 12: paint shedding from aquatic vessels
  sheddingrate12 <- runif(n, 0.88,1.12)/100 #source 12: paint shedding from aquatic vessels
  #do the calculations
  sum <- (housesTO_1*exterior_surface_area1*proportion_painted*mass_paint_area_1*kg_gallon_paint1*percent_solids1*house_shedding_rate1/1000)+(roadpaint3*percent_solids3*degradation_rate3)+(aquaticvessels12*litres_per_vessel12*kg_per_litre12*percentsolids12*sheddingrate12)/1000+(ships12*litres_per_vessel12*kg_per_litre12*percentsolids12*sheddingrate12*0.99)/1000
  #return result
  return(list("sum"=sum))
}
#add uncertainty pdf parameters to the list
param_paint = list("n" = 1) #number of arguments matches that of func
#run the actual Monte Carlo simulations
set.seed(1234) #ALWAYS RUN SET.SEED BEFORE MONTE CARLO
MC_paint <- MonteCarlo(func = paint, nrep = 10000, param_list = param_paint)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_paint <- MakeFrame(MC_paint)
head(df_paint)
p <- ggplot(df_paint, aes(x=sum)) + geom_histogram(binwidth = 1, color = "darksalmon") + labs(title = "Histogram of paint sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_paint$sum),color="blue",linetype="dashed",size=1) 
summary(df_paint$sum)
norm.interval(df_paint$sum)
#
#
#
#
#
#
#
###########################################4. tires#############################################
#defining the tires function
tires <- function(n) { #airplanes + car tires
  #sample from the pdfs
  airports7 <- 1 #source 7: airplane tire dust
  aircraft_movements7 <- runif(n,132528.34,137937.66) #source 7: airplane tire dust
  airplane_shedding7 <- runif(n,244.64,311.36) #source 7: airplane tire dust
  
  households10 <- runif(n,1108313.58,1249800.42) #source 10: vehicle tire dust
  vehicles_perhousehold10 <- runif(n,1.078,1.122) #source 10: vehicle tire dust
  km_per_year10 <- runif(n,14080,17920) #source 10: vehicle tire dust
  tire_shedding10 <- rtri(n,0.05,0.25,0.1) #source 10: vehicle tire dust
  #do the calculations
  sum <- (airports7*aircraft_movements7*airplane_shedding7/10^9)+(households10*vehicles_perhousehold10*km_per_year10*tire_shedding10)/10^9
  #return result
  return(list("sum"=sum))
}
#add uncertainty pdf parameters to the list
param_tires = list("n" = 1) #number of arguments matches that of func
#run the actual Monte Carlo simulations
set.seed(1234) #ALWAYS RUN SET.SEED BEFORE MONTE CARLO
MC_tires <- MonteCarlo(func = tires, nrep = 10000, param_list = param_tires)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_tires <- MakeFrame(MC_tires)
head(df_tires)
p <- ggplot(df_tires, aes(x=sum)) + geom_histogram(binwidth = 0.03, color = "darkorchid3") + labs(title = "Histogram of tire dust sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_tires$sum),color="blue",linetype="dashed",size=1) 
summary(df_tires$sum)
norm.interval(df_tires$sum)
#
#
#
#
#
#
###########################################5. clothes#############################################
#defining the clothes function
clothes <- function(n) { #laundry + dryers
  #sample from the pdfs
  households8 <- runif(n,1108313.58,1249800.42) #source 8: laundry washing 
  ownership_rate8 <- runif(n, 75.68, 96.32)/100 #source 8: laundry washing 
  wash_cycles8 <- runif(n,192.72, 245.28) #source 8: laundry washing 
  loadperwash8 <- runif(n,3,4) #source 8: laundry washing 
  laundry_sheddingrate8 <- runif(n,124,308) #source 8: laundry washing 
  percent_synthetic8 <- runif(n, 29.92, 38.08)/100 #source 8: laundry washing 
  WWTP_efficiency8 <- rnorm(n,0.989025,0.007325924) #source 8: laundry washing 
  
  households9 <- runif(n,1108313.58,1249800.42) #source 9: dryer vent emissions
  ownership_rate9 <- runif(n, 75.68, 96.32)/100 #source 9: dryer vent emissions
  dry_cycles9 <- runif(n,192.72,245.28) #source 9: dryer vent emissions
  loadperdry9 <- rnorm(n,0.438,0.017) #source 9: dryer vent emissions
  shedding_dryer9 <- rnorm(n,18,8) #source 9: dryer vent emissions
  percent_synthetic9 <- runif(n,29.92,38.08)/100 #source 9: dryer vent emissions
  #do the calculations
  sum <- (households8*ownership_rate8*wash_cycles8*loadperwash8*laundry_sheddingrate8*percent_synthetic8*(1-WWTP_efficiency8)/10^9)+(households9*ownership_rate9*dry_cycles9*loadperdry9*shedding_dryer9*percent_synthetic9/10^9)
  #return result
  return(list("sum"=sum))
}
#add uncertainty pdf parameters to the list
param_clothes = list("n" = 1) #number of arguments matches that of func
#run the actual Monte Carlo simulations
set.seed(1234) #ALWAYS RUN SET.SEED BEFORE MONTE CARLO
MC_clothes <- MonteCarlo(func = clothes, nrep = 10000, param_list = param_clothes)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_clothes <- MakeFrame(MC_clothes)
head(df_clothes)
p <- ggplot(df_clothes, aes(x=sum)) + geom_histogram(binwidth = 0.01, color = "dodgerblue4") + labs(title = "Histogram of clothes sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_clothes$sum),color="blue",linetype="dashed",size=1) 
summary(df_clothes$sum)
norm.interval(df_clothes$sum)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#TROUBLESHOOTING TROUBLESHOOTING TROUBLESHOOTING - trying to make histogram SMOOTHER; FIXED LITTERING PDF#############################################
#troubleshooting part 2: add sources one at a time to paint + tire dust function; outdated - did not add proportion painted parameter
#function 1: paint + tire dust
paint_tiredust <- function(n) {
  #sample from the pdfs
  exterior_surface_area <- rtri(n,2530,4382,3578)
  house_shedding <- runif(n,0.01,0.04)
  littering_rate <- runif(n,1000,33000) #rnorm(n,12656,14271) 
  turf_shedding <- runif(n,0.01,0.04)
  proportion_foam <- runif(n,0.058,0.15)
  pellet_loss <- runif(n,0.0001,0.0004)
  loadperwash <- runif(n,3,4)
  shedding_laundry <- runif(n,124,308)
  loadperdry <- rnorm(n,0.438,0.017)
  shedding_dryer <- rnorm(n,18,8)
  tire_shedding <- rtri(n,0.05,0.25,0.1)
  fishinggear_loss <- runif(n,0.01,0.1)
  litres_paint <- runif(n,2,3) 
  litres_paint2 <- runif(1,2,2.5)
  mass_paint_perarea <- runif(1,1,1.3) 
  #do the calculations
  # sum <- (405395*exterior_surface_area/100*0.45359237*0.2*house_shedding/1000) #source 1: house paint shedding
  # + (littering_rate) #source 2: littering, illegal dumping, inadequately managed waste, and garbage day overflow
  # + (235.9202662*0.835*0.50) #source 3: road paint shedding
  # + (54*27000*(6.5/1000)*turf_shedding) #source 4: shedding from artificial turf
  # + (95.9971663589714*proportion_foam*3.4) #source 5: foam snow during construction 
  # + (4667*0.0357*pellet_loss*1000) #source 6: pellet spills from plastic industry
  # + (135233*278/10^9) #source 7: shedding of tire dust from airplanes
  # + (1179057*0.86*219*loadperwash*shedding_laundry*0.45/10^9) #source 8: shedding of fibers from laundry
  # + (1179057*0.86*289*loadperdry*shedding_dryer*0.34/10^9) #source 9: fibers from dryers
  #  + (1179057*1.3*16000*tire_shedding)/10^9 #source 10: wearing of vehicle tires
  # + (109668*1435.974459546*fishinggear_loss/10^6) #source 11: derelict fishing gear
  # + (7160)*(litres_paint*0.119826404)*(0.01)/1000 #source 12: shedding of paint from aquatic vessels
  sum <- (135233*278/(10^9))+(1179057*1.3*16000*tire_shedding)/(10^9) + (405395*exterior_surface_area/100*0.45359237*0.2*house_shedding/1000)+(235.9202662*0.835*0.50)+(7160)*(litres_paint*0.119826404)*(0.01)/1000+(160*litres_paint2*mass_paint_perarea*0.5*0.01)/1000
  #return result
  return(list("sum"=sum))
}
param_paint_tiredust = list("n" = 1) #number of arguments matches that of func
#run the actual Monte Carlo simulations
#ALWAYS RUN SET.SEED BEFORE MONTE CARLO
set.seed(1234)
MC_paint_tiredust <- MonteCarlo(func = paint_tiredust, nrep = 1000, param_list = param_paint_tiredust)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_paint_tiredust <- MakeFrame(MC_paint_tiredust)
head(df_paint_tiredust)
p <- ggplot(df_paint_tiredust, aes(x=sum)) + geom_histogram(binwidth = 1, color = "black") + labs(title = "Histogram of tire dust sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_paint_tiredust$sum),color="blue",linetype="dashed",size=1) 
summary(df_paint_tiredust$sum)
norm.interval(df_paint_tiredust$sum)
#
#
#
#
#
#
#
paint_tiresclothes <- function(n) {
  #sample from the pdfs
  exterior_surface_area <- rtri(n,2530,4382,3578)
  house_shedding <- runif(n,0.01,0.04)
  littering_rate <- runif(n,1000,33000) #rnorm(n,12656,14271) 
  turf_shedding <- runif(n,0.01,0.04)
  proportion_foam <- runif(n,0.058,0.15)
  pellet_loss <- runif(n,0.0001,0.0004)
  loadperwash <- runif(n,3,4)
  shedding_laundry <- runif(n,124,308)
  loadperdry <- rnorm(n,0.438,0.017)
  shedding_dryer <- rnorm(n,18,8)
  tire_shedding <- rtri(n,0.05,0.25,0.1)
  fishinggear_loss <- runif(n,0.01,0.1)
  litres_paint <- runif(n,2,3) 
  litres_paint2 <- runif(1,2,2.5)
  mass_paint_perarea <- runif(1,1,1.3) 
  #do the calculations
  # sum <- (405395*exterior_surface_area/100*0.45359237*0.2*house_shedding/1000) #source 1: house paint shedding
  # + (littering_rate) #source 2: littering, illegal dumping, inadequately managed waste, and garbage day overflow
  # + (235.9202662*0.835*0.50) #source 3: road paint shedding
  # + (54*27000*(6.5/1000)*turf_shedding) #source 4: shedding from artificial turf
  # + (95.9971663589714*proportion_foam*3.4) #source 5: foam snow during construction 
  # + (4667*0.0357*pellet_loss*1000) #source 6: pellet spills from plastic industry
  # + (135233*278/10^9) #source 7: shedding of tire dust from airplanes
  # + (1179057*0.86*219*loadperwash*shedding_laundry*0.45/10^9) #source 8: shedding of fibers from laundry
  # + (1179057*0.86*289*loadperdry*shedding_dryer*0.34/10^9) #source 9: fibers from dryers
  #  + (1179057*1.3*16000*tire_shedding)/10^9 #source 10: wearing of vehicle tires
  # + (109668*1435.974459546*fishinggear_loss/10^6) #source 11: derelict fishing gear
  # + (7160)*(litres_paint*0.119826404)*(0.01)/1000 #source 12: shedding of paint from aquatic vessels
  sum <- (135233*278/(10^9))+(1179057*1.3*16000*tire_shedding)/(10^9)+(405395*exterior_surface_area/100*0.45359237*0.2*house_shedding/1000)+(235.9202662*0.835*0.50)+(7160)*(litres_paint*0.119826404)*(0.01)/1000+(160*litres_paint2*mass_paint_perarea*0.5*0.01)/1000+(1179057*0.86*219*loadperwash*shedding_laundry*0.45/10^9*(1-0.989025))+(1179057*0.86*289*loadperdry*shedding_dryer*0.34/10^9)
  #return result
  return(list("sum"=sum)) #+(54*27000*(6.5/1000)*turf_shedding)
}
param_1 = list("n" = 1) #number of arguments matches that of func
#run the actual Monte Carlo simulations
#ALWAYS RUN SET.SEED BEFORE MONTE CARLO
set.seed(1234)
MC_1 <- MonteCarlo(func = paint_tiresclothes, nrep = 1000, param_list = param_1)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_1 <- MakeFrame(MC_1)
head(df_1)
p <- ggplot(df_1, aes(x=sum)) + geom_histogram(binwidth = 1, color = "black") + labs(title = "Histogram of tire dust sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_1$sum),color="blue",linetype="dashed",size=1) 
summary(df_1$sum)
norm.interval(df_1$sum)
#
#
#
#
#
#
#
paint_tiresclothes_rec <- function(n) {
  #sample from the pdfs
  exterior_surface_area <- rtri(n,2530,4382,3578)
  house_shedding <- runif(n,0.01,0.04)
  littering_rate <- runif(n,1000,33000) #rnorm(n,12656,14271) 
  turf_shedding <- runif(n,0.01,0.04)
  proportion_foam <- runif(n,0.058,0.15)
  pellet_loss <- runif(n,0.0001,0.0004)
  loadperwash <- runif(n,3,4)
  shedding_laundry <- runif(n,124,308)
  loadperdry <- rnorm(n,0.438,0.017)
  shedding_dryer <- rnorm(n,18,8)
  tire_shedding <- rtri(n,0.05,0.25,0.1)
  fishinggear_loss <- runif(n,0.01,0.1)
  litres_paint <- runif(n,2,3) 
  litres_paint2 <- runif(1,2,2.5)
  mass_paint_perarea <- runif(1,1,1.3) 
  #do the calculations
  # sum <- (405395*exterior_surface_area/100*0.45359237*0.2*house_shedding/1000) #source 1: house paint shedding
  # + (littering_rate) #source 2: littering, illegal dumping, inadequately managed waste, and garbage day overflow
  # + (235.9202662*0.835*0.50) #source 3: road paint shedding
  # + (54*27000*(6.5/1000)*turf_shedding) #source 4: shedding from artificial turf
  # + (95.9971663589714*proportion_foam*3.4) #source 5: foam snow during construction 
  # + (4667*0.0357*pellet_loss*1000) #source 6: pellet spills from plastic industry
  # + (135233*278/10^9) #source 7: shedding of tire dust from airplanes
  # + (1179057*0.86*219*loadperwash*shedding_laundry*0.45/10^9) #source 8: shedding of fibers from laundry
  # + (1179057*0.86*289*loadperdry*shedding_dryer*0.34/10^9) #source 9: fibers from dryers
  #  + (1179057*1.3*16000*tire_shedding)/10^9 #source 10: wearing of vehicle tires
  # + (109668*1435.974459546*fishinggear_loss/10^6) #source 11: derelict fishing gear
  # + (7160)*(litres_paint*0.119826404)*(0.01)/1000 #source 12: shedding of paint from aquatic vessels
  sum <- (135233*278/(10^9))+(1179057*1.3*16000*tire_shedding)/(10^9)+(405395*exterior_surface_area/100*0.45359237*0.2*house_shedding/1000)+(235.9202662*0.835*0.50)+(7160)*(litres_paint*0.119826404)*(0.01)/1000+(160*litres_paint2*mass_paint_perarea*0.5*0.01)/1000+(1179057*0.86*219*loadperwash*shedding_laundry*0.45/10^9*(1-0.989025))+(1179057*0.86*289*loadperdry*shedding_dryer*0.34/10^9)+(54*27000*(6.5/1000)*turf_shedding)
  #return result
  return(list("sum"=sum)) 
}
param_1 = list("n" = 1) #number of arguments matches that of func
#run the actual Monte Carlo simulations
#ALWAYS RUN SET.SEED BEFORE MONTE CARLO
set.seed(1234)
MC_1 <- MonteCarlo(func = paint_tiresclothes_rec, nrep = 1000, param_list = param_1)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_1 <- MakeFrame(MC_1)
head(df_1)
p <- ggplot(df_1, aes(x=sum)) + geom_histogram(binwidth = 1, color = "black") + labs(title = "Histogram of tire dust sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_1$sum),color="blue",linetype="dashed",size=1) 
summary(df_1$sum)
norm.interval(df_1$sum)
#
#
#
#
#
#
#
#
paint_tiresclothes_recConstruction_aquatic <- function(n) {
  #sample from the pdfs
  exterior_surface_area <- rtri(n,2530,4382,3578)
  house_shedding <- runif(n,0.01,0.04)
  littering_rate <- runif(n,1000,33000) #rnorm(n,12656,14271) 
  turf_shedding <- runif(n,0.01,0.04)
  proportion_foam <- runif(n,0.058,0.15)
  pellet_loss <- runif(n,0.0001,0.0004)
  loadperwash <- runif(n,3,4)
  shedding_laundry <- runif(n,124,308)
  loadperdry <- rnorm(n,0.438,0.017)
  shedding_dryer <- rnorm(n,18,8)
  tire_shedding <- rtri(n,0.05,0.25,0.1)
  fishinggear_loss <- runif(n,0.01,0.1)
  litres_paint <- runif(n,2,3) 
  litres_paint2 <- runif(1,2,2.5)
  mass_paint_perarea <- runif(1,1,1.3) 
  #do the calculations
  # sum <- (405395*exterior_surface_area/100*0.45359237*0.2*house_shedding/1000) #source 1: house paint shedding
  # + (littering_rate) #source 2: littering, illegal dumping, inadequately managed waste, and garbage day overflow
  # + (235.9202662*0.835*0.50) #source 3: road paint shedding
  # + (54*27000*(6.5/1000)*turf_shedding) #source 4: shedding from artificial turf
  # + (95.9971663589714*proportion_foam*3.4) #source 5: foam snow during construction 
  # + (4667*0.0357*pellet_loss*1000) #source 6: pellet spills from plastic industry
  # + (135233*278/10^9) #source 7: shedding of tire dust from airplanes
  # + (1179057*0.86*219*loadperwash*shedding_laundry*0.45/10^9) #source 8: shedding of fibers from laundry
  # + (1179057*0.86*289*loadperdry*shedding_dryer*0.34/10^9) #source 9: fibers from dryers
  #  + (1179057*1.3*16000*tire_shedding)/10^9 #source 10: wearing of vehicle tires
  # + (109668*1435.974459546*fishinggear_loss/10^6) #source 11: derelict fishing gear
  # + (7160)*(litres_paint*0.119826404)*(0.01)/1000 #source 12: shedding of paint from aquatic vessels
  sum <- (135233*278/(10^9))+(1179057*1.3*16000*tire_shedding)/(10^9)+(405395*exterior_surface_area/100*0.45359237*0.2*house_shedding/1000)+(235.9202662*0.835*0.50)+(7160)*(litres_paint*0.119826404)*(0.01)/1000+(160*litres_paint2*mass_paint_perarea*0.5*0.01)/1000+(1179057*0.86*219*loadperwash*shedding_laundry*0.45/10^9*(1-0.989025))+(1179057*0.86*289*loadperdry*shedding_dryer*0.34/10^9)+(54*27000*(6.5/1000)*turf_shedding)+ (95.9971663589714*proportion_foam*3.4)+ (4667*0.0357*pellet_loss*1000)+ (109668*1435.974459546*fishinggear_loss/10^6)
  #return result
  return(list("sum"=sum)) 
}
param_1 = list("n" = 1) #number of arguments matches that of func
#run the actual Monte Carlo simulations
#ALWAYS RUN SET.SEED BEFORE MONTE CARLO
set.seed(1234)
MC_1 <- MonteCarlo(func = paint_tiresclothes_recConstruction_aquatic, nrep = 1000, param_list = param_1)
#don't need to generate table. Go straight to making a dataframe in order to plot results
#visualize results 
#with code from: http://www.sthda.com/english/wiki/ggplot2-histogram-plot-quick-start-guide-r-software-and-data-visualization
df_1 <- MakeFrame(MC_1)
head(df_1)
p <- ggplot(df_1, aes(x=sum)) + geom_histogram(binwidth = 1, color = "black") + labs(title = "Histogram of tire dust sums",x="emissions (T)",y="Frequency")
p + geom_vline(xintercept=mean(df_1$sum),color="blue",linetype="dashed",size=1) 
summary(df_1$sum)
norm.interval(df_1$sum)
#
#
#
#
#I want to re-order; factor() worked! Barplots for emissions inventory summary infographic
library(readxl)
library(ggplot2)
library(dplyr)
library(forcats)
All <- read_excel("C:/Users/alice/OneDrive - University of Toronto/Alice/Temp/Emissions Inventory/All_sources_barplot.xlsx")
Micro <- read_excel("C:/Users/alice/OneDrive - University of Toronto/Alice/Temp/Emissions Inventory/Microplastics_barplot.xlsx")
All$sources <- factor(All$sources,levels=c("exteriorhousepaint","artificialturf","roadmarkings","pelletspills","construction","vehicletiredust","laundrywashing","dryers","aquaticvesselpaint","airplanetiredust","littering","derelictfishinggear"))
ggplot(All,aes(fill=sources,y=emissions,x=z)) + geom_bar(position='stack',stat='identity') + scale_fill_manual(values=c("red1","yellow","firebrick2","green1","springgreen4","orchid","slateblue1","royalblue2","pink","darkorchid1","orange","skyblue1")) + theme(axis.title.x = element_text(size = 15),axis.title.y = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y=element_text(size=15))
#
Micro$sources <- factor(Micro$sources,levels=c("exteriorhousepaint","artificialturf","roadmarkings","pelletspills","construction","vehicletiredust","laundrywashing","dryers","aquaticvesselpaint","airplanetiredust"))
ggplot(Micro,aes(fill=sources,y=emissions,x=z)) + geom_bar(position='stack',stat='identity') + scale_fill_manual(values=c("red1","yellow","firebrick2","green1","springgreen4","orchid","slateblue1","royalblue2","pink","darkorchid1","orange","skyblue1")) + theme(axis.title.x = element_text(size = 15),axis.title.y = element_text(size=15),axis.text.x = element_text(size=15),axis.text.y=element_text(size=15))
#"exteriorhousepaint","artificialturf","roadmarkings","pelletspills","construction","vehicletiredust","laundrywashing","dryers","aquaticvesselpaint","airplanetiredust","littering","derelictfishinggear"
#red, yellow, red, green, green, purple, navy, navy, red, purple, orange, light blue
#"red1","yellow","firebrick2","green1","springgreen4","orchid","slateblue1","royalblue2","pink","darkorchid1","orange","skyblue1"
#
#
#
#GAM of littering rate against population density, education, income, unemployment rate, Tweet density, litter receptable density
#see which covariates are significant predictors of plastic littering rate
#And then, is there another city that has littering rate data? How well does our model explain that data?  
#*helpful for other cities: how to get this data if you don’t have litter data
library(mgcv)
Litter <- read_excel("C:/Users/alice/OneDrive - University of Toronto/Alice/Temp/Emissions Inventory/Litter_Covariates_Standardized.xlsx")
head(Litter)
LitterNoNa <- Litter[complete.cases(Litter),]
is.na(LitterNoNa$PlasticLitter)
panel.cor <- function(x,y,digits=2,prefix="",cex.cor,...) #with r values from cor()
{
  usr <- par("usr");on.exit(par(usr)) 
  par(usr=c(0,1,0,1)) 
  r<-abs(cor(x,y)) 
  txt<-format(c(r,0.123456789),digits=digits)[1] 
  txt<-paste0(prefix,txt) 
  if(missing(cex.cor)) 
    cex.cor<-0.8/strwidth(txt) 
  text(0.5,0.5,txt,cex=cex.cor*r) 
} #A total of 8 covariates including start_year_sampling, without considering inverse distance weighted population
pairs(PlasticLitter ~ UnemployRt + Pop_km2 + HighSchl + Income + Tweets_km2 + Receptacle_km2+AtLeastUni, upper.panel=panel.cor,data=LitterNoNa)
#Try 4 combinations of Tweet_km2 and Receptable_km2, to see which residual combination gives the best AIC
Tweet.Receptacle.res <- lm(Tweets_km2~Receptacle_km2,data=LitterNoNa)$residual #residuals of whatever receptacle cannot explain in Tweets - use to replace Tweets
Receptacle.Tweet.res <- lm(Receptacle_km2~Tweets_km2,data=LitterNoNa)$residual #residuals of whatever tweets cannot explain in receptacle - use to replace receptacle
M.1 <- gam(PlasticLitter ~ Tweets_km2 + Tweet.Receptacle.res, family = tw,data=LitterNoNa) #why is this necessary?
M.2 <- gam(PlasticLitter ~ Receptacle_km2 + Tweet.Receptacle.res, family = tw,data=LitterNoNa) #this should do better
M.3 <- gam(PlasticLitter ~ Tweets_km2 + Receptacle.Tweet.res, family = tw,data=LitterNoNa) #this should do better
M.4 <- gam(PlasticLitter ~ Receptacle_km2 + Receptacle.Tweet.res, family = tw,data=LitterNoNa) #also why is this necessary?
#whichever combination produces the lowest AIC is the one to use in the overall model
AIC(M.1)
AIC(M.2) #AICs are all the same = -5.3783... so just choose one? 
AIC(M.3) #Tweets_km2 alone AIC = -6.653728. Receptacle_km2 alone AIC = -6.505717 
AIC(M.4) #Lowest AIC is best, so just include Tweets_km2. Don't include receptacle_km2 in model.
#if Tweet density is a significant predictor, then that automatically means receptacle density is a good predictor
set.seed(1234) #choose family of distributions based on histogram of littering rates:
#tw estimates 'p' parameter when fitting; 1 = Poisson, 2 = Gamma
#Both distributions can fit nicely to right-skewed data with lots of zeros (zero-inflation)
#That is why Tweedie family of distributions is useful - it is versatile, adapts to the situation at hand
#https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/Tweedie.html
M.all_Litter <- gam(PlasticLitter ~ UnemployRt + Pop_km2 + HighSchl + Income + Tweets_km2 + AtLeastUni, family=tw, data=LitterNoNa)
M.all_LitterGamma <- gam(PlasticLitter ~ UnemployRt + Pop_km2 + HighSchl + Income + Tweets_km2 + AtLeastUni, family=Gamma(link="log"), data=LitterNoNa)
M.all_LitterGaussian <- gam(PlasticLitter ~ UnemployRt + Pop_km2 + HighSchl + Income + Tweets_km2 + AtLeastUni, family=gaussian, data=LitterNoNa)
library(MuMIn)
model.sel(M.all_Litter,M.all_LitterGamma,M.all_LitterGaussian, rank = AIC)
#M.all_Litter <- gam(PlasticLitter ~ s(UnemployRt) + s(Pop_km2) + s(HighSchl) + s(Income) + s(Tweets_km2) + s(AtLeastUni), family=tw, data=LitterNoNa)
M.all_Litter 
summary(M.all_Litter) 
plot(M.all_Litter, residuals=TRUE)
gam.check(M.all_Litter, ncol=1, nrow=1)
plot(M.all_Litter) #only works when there are smooth terms in model, i.e. s(covariate)
#null model
M.null_Litter <- gam(PlasticLitter~1,family=tw,data=LitterNoNa)
M.null_Litter
summary(M.null_Litter)
#AIC scores
AIC(M.all_Litter)
AIC(M.null_Litter)
#comparing AIC scores of all possible permutations of the covariates in the overall model; lowest AIC scores listed first
#force include the two confounding variables and the measure of sampling effort in the best model using subset
options(na.action = "na.fail")
#M.all_ROVdredge <- dredge(M.all_ROV,subset = ~ net_mesh,delta<4) #force include one variable
library(MuMIn)
M.all_Litterdredge <- dredge(M.all_Litter)
M.all_Litterdredge
M.best <- gam(PlasticLitter~Pop_km2,family=tw,data=LitterNoNa)
summary(M.best)
AIC(M.best)
median(LitterNoNa$Pop_km2)
#comparing original data to predictions
library(ggplot2)
LitterNoNa$Litter_predict <- predict(M.all_Litter,newdata=LitterNoNa,type="response")# %>% as_tibble() %>% bind_cols(LitterNoNa)
LitterNoNa$Litter_predict
ggplot(LitterNoNa, aes(x=PlasticLitter)) + geom_histogram(color="black",fill="white")
ggplot(LitterNoNa, aes(x=Litter_predict)) + geom_histogram(color="black",fill="white")
