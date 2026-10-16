library(faosws)
library(faoswsUtil)
library(openxlsx)
library(data.table)
library(dplyr)


`%!in%` = Negate(`%in%`)

R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")

if(CheckDebug()){
  message("Not on server, so setting up environment...")

  library(faoswsModules)
  SETT <- ReadSettings("sws.yml")

  GetTestEnvironment(
    baseUrl = SETT[["server"]],
    token = SETT[["token"]]
  )
}

geoKeys = GetCodeList(domain = "trade", dataset = "total_trade_cpc_m49",
                      dimension = "geographicAreaM49")[type == "country", code]
geoDim <- Dimension(name = "geographicAreaM49",
                    keys = geoKeys)

itemKeys = GetCodeList(domain = "trade", dataset = "total_trade_cpc_m49", "measuredItemCPC")
itemKeys = itemKeys[, code]

eleDim <- c('5610', '5910', '5622', '5922') %>% Dimension(name = "measuredElementTrade", keys = .)

# Define item dimension
itemDim <- Dimension(name = "measuredItemCPC", keys = itemKeys)

# Define time dimension
timeDim <- Dimension(name = "timePointYears", keys = as.character(2000:2025))

key <- DatasetKey(
  domain = "trade",
  dataset = "total_trade_cpc_m49",
  dimensions = list(
    geographicAreaM49 = geoDim,
    measuredElementSuaFbs = eleDim,
    measuredItemFbsSua = itemDim,
    timePointYears = timeDim
  )
)


#tradedata <- GetData(key)
#saveRDS(tradedata, file="tradedatasws1.rds")
tradedata <- readRDS('tradedatasws1.rds')
trade_countries <- read.xlsx('trade_countries.xlsx')

### Remove 1249 China (excl. Hong Kong and Macao) and other non validated countries ###

tradedata <- tradedata[geographicAreaM49!='1249',]
tradedata <- tradedata[geographicAreaM49 %in% unique(trade_countries$m49),]

### Remove T0010 and F1882  ###

total_merchandise_trade <- tradedata[measuredItemCPC=="T0010"]
total_agriculture_trade <- tradedata[measuredItemCPC=="F1882"]
tradedata <- tradedata[measuredItemCPC %!in% c("T0010", "F1882"), ]

### Item groups are not needed now.
# itemgroups <- fread("domainvargroupvar.csv", colClasses = "character", sep=",")
# names(itemgroups) <- gsub(" ","", names(itemgroups))
# itemgroups <- itemgroups[domain_code=="TP"&var_type=="item"]

# tradedata_named <- nameData(domain = "trade",
#                             dataset = "total_trade_cpc_m49", tradedata, except = 'timePointYears')
#


tradedata_dcasted <- dcast.data.table(tradedata, geographicAreaM49 + measuredItemCPC + timePointYears ~
                                        measuredElementTrade, value.var = "Value")

export_price0 <- tradedata_dcasted[!(`5922`==0|`5910`==0|is.na(`5922`)|is.na(`5910`))][
    timePointYears %in% c("2014", "2015", "2016"),
    .(Base_price_export = (sum(`5922`)/sum(`5910`))*1000),
    .(geographicAreaM49, measuredItemCPC)]


tradedata_dcasted <- merge(tradedata_dcasted, export_price0, by = c("geographicAreaM49", "measuredItemCPC"),
                   all.x=TRUE)


import_price0 <- tradedata_dcasted[!(`5622`==0|`5610`==0|is.na(`5622`)|is.na(`5610`))][
  timePointYears %in% c("2014", "2015", "2016"),
    .(Base_price_import = (sum(`5622`)/sum(`5610`))*1000),
    .(geographicAreaM49, measuredItemCPC)]

## tradedata <- merge(tradedata, price, by = c("Area_Code_M49", "Item_Code_CPC", "Year"),
##                    all.x=TRUE)
tradedata_dcasted <- merge(tradedata_dcasted, import_price0, by = c("geographicAreaM49", "measuredItemCPC"),
                   all.x=TRUE)

## Where there is no quantity,
tradedata_dcasted[`5922`!=0&!is.na(`5922`)&`5910`==0, `5910` := NA]
tradedata_dcasted[`5622`!=0&!is.na(`5622`)&`5610`==0, `5622` := NA]

#tradedata[, Export_value_real := Export_value*100/Export_price] # If using index
#tradedata[, Import_value_real := Import_value*100/Import_price] #if using index
tradedata_dcasted[, Export_value_real := `5910`*Base_price_export]
tradedata_dcasted[, Import_value_real := `5610`*Base_price_import]

# deflator_import_country <-  tradedata_dcasted[!is.na(Import_value_real),
#                        .(deflator_import_country = sum(`5622`, na.rm=TRUE)/
#                            sum(Import_value_real, na.rm=TRUE)),
#                        .(geographicAreaM49, timePointYears)]
#
# deflator_import_global <- tradedata_dcasted[!is.na(Import_value_real),
#                             .(deflator_import_global =sum(`5622`, na.rm=TRUE)/
#                                 sum(Import_value_real, na.rm=TRUE)),
#                             .(timePointYears)]

# tradedata_dcasted <- merge(tradedata_dcasted, deflator_import_country, by=c("geographicAreaM49", "timePointYears"))
# tradedata_dcasted <- merge(tradedata_dcasted, deflator_import_global, by=c("timePointYears"))
# tradedata_dcasted[is.na(Import_value_real), Import_value_real := `5622`/deflator_import_country]
# tradedata_dcasted[is.na(Import_value_real)|!is.finite(Import_value_real),
#           Import_value_real := `5622`/deflator_import_global]
# tradedata_dcasted[, deflator_import_country := NULL]
# tradedata_dcasted[, deflator_import_global := NULL]

# deflator_export_country <- tradedata_dcasted[!is.na(Export_value_real),
#                       .(deflator_export_country =sum(`5922`, na.rm=TRUE)/
#                           sum(Export_value_real, na.rm=TRUE)),
#                       .(geographicAreaM49, timePointYears)]
#
#
# deflator_export_global <- tradedata_dcasted[!is.na(Export_value_real),# & Export_price!=0,
#                             .(deflator_export_global =sum(`5922`, na.rm=TRUE)/
#                                 sum(Export_value_real, na.rm=TRUE)),
#                             .(timePointYears)]
# tradedata_dcasted <- merge(tradedata_dcasted, deflator_export_country, c("geographicAreaM49", "timePointYears"))
# tradedata_dcasted <- merge(tradedata_dcasted, deflator_export_global, by=c("timePointYears"))
# tradedata_dcasted[is.na(Export_value_real),
#           Export_value_real := `5922`/deflator_export_country]
# tradedata_dcasted[is.na(Export_value_real)|!is.finite(Import_value_real),
#                   Export_value_real := `5922`/deflator_export_global]
# tradedata_dcasted[, deflator_export_country := NULL]
# tradedata_dcasted[, deflator_export_global := NULL]


### Create aggregations ###

### Aggregations ###
f_codes <- fread('F_codes.csv')
f_codes$`Item Code (CPC)` <- gsub(f_codes$`Item Code (CPC)`, pattern = "'", replacement = "")

# to find the first level aggregations (quantity sum)
quantity_aggregations1 <- f_codes[Aggregate==1, `Item Code (CPC)`]

# to find all the quantities
quantity_aggregations2 <- f_codes[Aggregate %in% c(1,2,3), `Item Code (CPC)`]

aggregation_composition <- setDT(read.xlsx('aggregates_composition.xlsx'))
aggregation_composition <- aggregation_composition[Aggregation.type=="item" & Group.code %in% quantity_aggregations2,]
aggregation_composition[, c("Domain", "Aggregation.type", "Child.name", "Link.code"):= NULL]
aggregation_composition <- aggregation_composition[!duplicated(aggregation_composition), ]
aggregation_composition[, count:= .N, .(Group.code, Child.code, Group.name)]
aggregation_composition <- aggregation_composition[!(count==2 & Factor==1),]
aggregation_composition[, count:= NULL]

# tradedata_aggregated <- merge(tradedata, aggregation_composition, by.x = 'measuredItemCPC', by.y = 'Child.code', allow.cartesian = TRUE)
# tradedata_aggregated$Factor <- as.numeric(tradedata_aggregated$Factor)
# tradedata_aggregated[, new_value:= ifelse(measuredElementTrade %in% c('5610', '5910'),Value*Factor, Value)]
#
# tradedata_aggregated <- tradedata_aggregated[, .(Value= sum(new_value)), .(geographicAreaM49, Group.code, Group.name, measuredElementTrade, timePointYears)]
#
# tradedata_aggregated[, Group.name:= NULL]
#
# setnames(tradedata_aggregated, 'Group.code', 'measuredItemCPC')
# tradedata_aggregated[, flagObservationStatus:= NA]
# tradedata_aggregated[, flagMethod:= NA]
#
# setcolorder(tradedata_aggregated, c("geographicAreaM49", "measuredElementTrade", "measuredItemCPC", "timePointYears", "Value", "flagObservationStatus", "flagMethod"))
#
# tradedata2 <- rbind(tradedata, tradedata_aggregated)



## aggregation for real values calculation:


tradedata_realvalues <- merge(tradedata_dcasted, aggregation_composition, by.x = 'measuredItemCPC', by.y = 'Child.code', allow.cartesian = TRUE)
tradedata_realvalues$Factor <- as.numeric(tradedata_realvalues$Factor)
tradedata_realvalues[, factorised_5610 := `5610`*Factor]
tradedata_realvalues[, factorised_5910 := `5910`*Factor]


tradedata_realvalues[, Import_value_real := factorised_5610*Base_price_import]
tradedata_realvalues[, Export_value_real := factorised_5910*Base_price_export]

tradedata_realvalues_agg <- tradedata_realvalues[, lapply(.SD, sum, na.rm=TRUE), by=c("geographicAreaM49", "timePointYears", "Group.code", "Group.name"),
                                                 .SDcols=c("factorised_5610", "factorised_5910", "Export_value_real", "Import_value_real")]

tradedata_realvalues_agg <- tradedata_realvalues_agg[Group.code %!in% quantity_aggregations1, factorised_5610:= NA ]
tradedata_realvalues_agg <- tradedata_realvalues_agg[Group.code %!in% quantity_aggregations1, factorised_5910:= NA ]



## OUTLIERS ##

detect_outliers_mad <- function(x) {
  med <- median(x, na.rm = TRUE)
  mad_val <- mad(x, constant = 1, na.rm = TRUE)

  return(abs(x - med) > 3 * mad_val)
}

tradedata_realvalues_agg[, outlier_export_amongeArea := detect_outliers_mad(Export_value_real),
   by = .(timePointYears, Group.code)]

tradedata_realvalues_agg[, outlier_export_amongeTime := detect_outliers_mad(Export_value_real),
                         by = .(geographicAreaM49, Group.code)]



outliers_mad_dt <- dt[outlier_mad == TRUE]

####

library(ggplot2)

global_agg <- tradedata_realvalues_agg[, lapply(.SD, sum, na.rm=TRUE), by=c("timePointYears", "Group.code", "Group.name"),
                                                               .SDcols=c("factorised_5610", "factorised_5910", "Export_value_real", "Import_value_real")]

global_agg[, outlier_ex := {
  med <- median(Export_value_real, na.rm = TRUE)
  mad_val <- mad(Export_value_real, constant = 1, na.rm = TRUE)
  abs(Export_value_real - med) > 3 * mad_val
}, by = Group.code]



global_export_outliers <- global_agg[outlier_ex== TRUE,]
unique(global_export_outliers$Group.name)

global_agg[, outlier_im := {
  med <- median(Import_value_real, na.rm = TRUE)
  mad_val <- mad(Import_value_real, constant = 1, na.rm = TRUE)
  abs(Import_value_real - med) > 3 * mad_val
}, by = Group.code]

global_import_outliers <- global_agg[outlier_im== TRUE,]

#### INDEX CALCULATION ####

## Import and Export Quantity Index
## Quantity_t / Quantity_0 * 100









