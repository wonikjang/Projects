library(data.table)
library(readr)
library(dplyr)
library(texreg)
library(ggplot2)
library(gridExtra)
library(stargazer)
library(broom)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)

######################################################################## Separate regression for each store at one time : Market share of high alcohol content products by tax
store<-fread("stores_month.csv",header=T)
store$yearquarter<-paste(store$year,store$quarter,sep="")
store<-data.table(store)
midwest<-store[fips_state_descr=='IL'|fips_state_descr=='IA'|fips_state_descr=='IN'|fips_state_descr=='WI'|fips_state_descr=='MI'|fips_state_descr=='MO',]
midwest<-midwest[,!"V1",with=F]
midwest[,region:=ifelse(fips_state_descr=='IL','IL','Midwest') ]
midwest$yearmonth<-as.numeric(midwest$yearmonth)

midwest1<- midwest %>% group_by(store_code_uc,fips_state_descr,fips_county_code,yearmonth,region,category,abv_cat) %>% summarise(volume=sum(volume))
midwest11<- midwest %>% group_by(store_code_uc,fips_state_descr,fips_county_code,yearmonth,region,category) %>% summarise(volumesum=sum(volume))
midwest2<-midwest111[,c("store_code_uc","region","category","abv_cat","treat","share","Dec"),with=F]
mshigh<-midwest2[abv_cat=='high',]
gin2<-mshigh[category=='Gin',]

midwestnest<-gin2 %>% group_by(store_code_uc,region)%>% nest()

# Fit models : 
# "tax" is focal variable for the effects of spirits tax increase
# Treat "Dec" as seasonality control 
tax_model<-function(df){
  lm(share ~ treat + Dec, data=df) 
}

models<- midwestnest %>% mutate( model= data %>% map(tax_model) )
head(models)
# Broom
models <- models %>% mutate( 
  glance = model %>% map(broom::glance),
  rsq = glance %>% map_dbl("r.squared"),
  tidy = model %>% map(broom::tidy),
  augment = model %>% map(broom::augment)
)

# Unnest 
models %>% unnest(tidy) %>% 
  select(store_code_uc,region,term,estimate,rsq) %>% spread(term,estimate) %>% 
  ggplot( aes(treat,fill = region,color=region)) + 
  geom_density(alpha=0.1)+theme_bw()+geom_vline(xintercept = 0,color="black")+ggtitle("Distribution of estimated tax parameters in Gin")


############################################################## Store map
library(maps)
### 1 way 
setwd("/Users/wonikJang/Dropbox/Wonik/Liquor/data_store")
countyweek<-fread("county_week.csv",header=TRUE)
countyweek<-countyweek[,!"V1",with=FALSE]
countyweek<-countyweek[!is.na(category),]
countyweek[,abv_cat:= ifelse(alcohol_abv==40,'medium',ifelse(alcohol_abv<40,'low','high')), ]
# Remove the observations that haven't category infromation( vodka, gin...) 
head(countyweek)
countyweek
cross<-countyweek %>% group_by(fips_state_descr,fips_county_descr,category,abv_cat) %>% summarise(sales=sum(sales),volume=sum(volume),nobs=sum(nobs))
head(cross)

statemap<-cross %>% group_by(fips_state_descr,fips_county_descr) %>% summarise(nobs=sum(nobs))
statemap$fips_county_descr<-tolower(statemap$fips_county_descr)

state<-read.csv("state_fips_code.csv")
colnames(statemap)[1]<-'fips_stste_descr'
statemap2<-merge(statemap,state,by='Abbreviation',all.x=T)
colnames(statemap2)[2]<-'county.name'
colnames(statemap2)[1]<-'state.abb'
library(choroplethr)
library(choroplethrMaps)
data(county.regions)
statemap3<-merge(statemap2,county.regions,by=c('state.abb','county.name'),all.y=T)
unique(statemap3$state.name)
statemap3<-data.table(statemap3)
statemap3<-statemap3[,lapply(.SD,function(x){ifelse(is.na(x),0,x)})]
statemap3[,store:=ifelse(nobs==0,0,1)]
colnames(statemap3)[3]<-'value'
statemapfin<-statemap3[,c("value","region"),with=FALSE]

statemapfin<-statemapfin[!is.na(region),]
county_choropleth(statemapfin,title= "Stores at county level in US", legend = "Stores")
county_choropleth(statemapfin,title= "Stores at county level in Midwest", legend = "Stores",
                  state_zoom = c("wisconsin", "michigan", "indiana","illinois","missouri","iowa"))

################################################################ Price of top 2 upc and data summary
setwd("/Users/wonikJang/Desktop/spirits")
spir<-fread("spirupc.csv",header=T)
head(spirupc)
spirupc<-data.table(spirupc)
spirupc<-spirupc[!is.na(category),]
spirupcabv<-spirupc[!is.na(alcohol_abv),]
nrow(spirupcabv) / nrow(spirupc) # 99.92%  of data has information about alcohol contents 


spirsum<-spirupcabv %>% mutate(sales=(price/prmult)*units, volume=(units*size*multi)) %>% group_by(category,size)%>%
  summarise(sales =sum(sales), volume =sum(volume), nobs = n() )

# category 
spirsum2<-spirsum %>% group_by(category) %>% summarise(volumesum=sum(volume))
sum(spirsum2$volumesum)
spirsum2$per<-round(100*spirsum2$volumesum/448537282200 ,2)

# size 
spirsum2<-spirsum %>% group_by(size) %>% summarise(volumesum=sum(volume))
sum(spirsum2$volumesum)
spirsum2$per<-round(100*spirsum2$volumesum/448537282200 ,2)
spirupcabv<-data.table(spirupcabv)
spirupcabv[,yearmonth:=substring(week_end,1,6)]

spirupc2<-spirupcabv %>% mutate(sales=(price/prmult)*units, volume=(units*size*multi)) %>% group_by(store_code_uc,upc,yearmonth,category)%>%
  summarise(sales =sum(sales), volume =sum(volume), nobs = n() )


upctype<-spirupc2 %>% group_by(upc,category) %>% summarise(volume=sum(volume))
upctype<-data.table(upctype)
upctype2 = upctype[order(-volume, category), head(.SD, 2), by = .( category)]
upctype2[order(upc,category)]

######################################################################## Regression with type fixed effects 
head(midstore)
midprice<-midstore %>% group_by(store_code_uc,yearmonth,region,category,upc) %>% summarise(price_avg=mean(price))
head(midprice)
midprice<-data.table(midprice)
midprice[,tax:=ifelse(yearmonth>=200909,1,0)]
#midprice[,IL:=ifelse(region=='IL',1,0)]
#midprice[,tax_IL:=tax*IL]

midprice$region<-as.factor(midprice$region)
midprice <- within(midprice, region<- relevel(region, ref = 'Midwest'))
midprice[,gin:=ifelse(category=='Gin',1,0)]
midprice[,rum:=ifelse(category=='Rum',1,0)]
midprice[,vodka:=ifelse(category=='Vodka',1,0)]
midprice[,scotch:=ifelse(category=='Scotch',1,0)]

lmprice<-lm(log(price_avg)~gin+rum+vodka+scotch+tax*region,data=midprice )
summary(lmprice)

library(stargazer)
### ind + timefixed effects
indtime<-rbindlist(list(ginmat,rummat,scotchmat,whiskeymat,vodkamat))
indtimefit<-lm(log(volume)~ .,data=indall)
store$yearquarter<-paste(store$year,store$quarter,sep="")
store<-data.table(store)
midwest<-store[fips_state_descr=='IL'|fips_state_descr=='IA'|fips_state_descr=='IN'|fips_state_descr=='WI'|fips_state_descr=='MI'|fips_state_descr=='MO',]
midwest %>% group_by(fips_state_descr) %>% summarise( count=n_distinct(store_code_uc))
storemid<-midwest %>% group_by(store_code_uc,fips_state_descr,fips_county_code,yearmonth,category) %>% summarise(sales=sum(sales),volume=sum(volume))
stargazer(indfit,indtimefit, column.labels = c("ind_fix","ind_time_fix"),keep=c("treat_time","typeGin","typeRum","typeWhiskey","typeScotch"),align=TRUE, type="html")

