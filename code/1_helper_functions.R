
m.eff = function(data,vars) {
  a = data
  Ccenter = apply(a[a$IDS_RAPID==0,vars],2,mean,na.rm=TRUE)
  Csd = apply(a[a$IDS_RAPID==0,vars],2,sd,na.rm=TRUE)
  a[,vars] = scale(a[,vars],center=Ccenter,scale=Csd)
  meaneff = apply(a[,vars],1,sum,na.rm=TRUE)/apply(a[,vars],1,function(x) sum(!is.na(x)))
  meaneff/sd(meaneff[a$IDS_RAPID==0], na.rm=TRUE)
}



######################################################
#             Functions for shocks data              #
######################################################


# Prepare data frame that links villages to conflict or event ocurrences.
 prepdata <- function(dataset,m, vars) {
   # @dataset an object of the type "SpatialPointsDataFrame" with geo location of conflicts
   # @m radius in meters
   # @vars variables from @dataset to be contained in the output eg. c("LOCATION", "YEAR",...)
  v <- do.call(rbind,lapply(1:nrow(vill),
                            function(x)
                              data.frame(x,
                                         adjacent = (distGeo(vill@coords[x,], dataset) <= m),
                                         dataset[,vars]))) %>%
       dplyr::mutate(IDV = vill$IDV[x])

  # selects only confl within buffer < m
  v %<>% dplyr::filter(adjacent == 1) %>%
    dplyr::select(IDV, vars)
  v
 }

 # Compute deviations comparing EVENTS in prior year and average count in past 15 years

 anomal <- function(ID, short = 12*5 , long = 5, frlong = "2002/08/01", data_event, data_sudates = dates ) {
   #@ID: IDV
   #@short: m prior months
   #@long: y prior years excluding m
   #@frlong: starting point of y priors years
   #@data_event: data set with events/conlfl
   #@data_sudates: data frame with IDV and dates of step C

   survey_month <- as.yearmon(data_sudates$date[data_sudates$IDV == ID])
   short_start  <- as.Date(survey_month - short/12)
   short_end    <- as.Date(survey_month - 1/12)
   long_start   <- as.Date(survey_month - short/12 - long)
   long_end     <- as.Date(survey_month - short/12 - 1/12)

   #check:
   #survey_month <- as.yearmon("1972-09-27")
   #survey_month
   # short=48
   # short_end    <- as.Date(survey_month - 1/12)
   #as.yearmon(short_end)

   interval_data <- data_event %>% subset(IDV == ID) %>%
     transform(DATE = as.Date(DATE)) %>%
     dplyr::summarise(shortEvents = sum(EVENTS[DATE >= short_start & DATE <= short_end], na.rm = TRUE)/(short/12),
                      longEvents  = sum(EVENTS[DATE >= long_start & DATE <= long_end], na.rm = TRUE)/(long),
                      diffEvents = shortEvents - longEvents)

   out <- data.frame(
   	 survey_month  = survey_month ,
     t_minus_4_start = as.character(short_start),
     t_minus_4_end = as.character(short_end),
     t_minus_8_start = as.character(long_start),
     t_minus_8_end = as.character(long_end),
     meanEvents_t_minus_4 = interval_data$shortEvents,
     meanEvents_t_minus_8 = interval_data$longEvents,
     Confl_Shock = interval_data$diffEvents
   )
   #  sseq <- data.frame(IDV = ID,
   #                    DATE = seq.Date(from = as.Date(as.yearmon(data_sudates$date[data_sudates$IDV == ID])-short/12),
   #                                    to = as.Date(as.yearmon(data_sudates$date[data_sudates$IDV == ID])  - 1/12), # we could include a few months during the time rapid was implemented
   #                                    by = "month"))
   #
   # lseq <- data.frame(IDV = ID,
   #                    DATE = seq.Date(from = as.Date(as.yearmon(data_sudates$date[data_sudates$IDV == ID])- short/12 - long) ,
   #                                    to = as.Date(as.yearmon(data_sudates$date[data_sudates$IDV == ID])- short/12 - 1/12) ,
   #                                    by ="month")) %>%
   #   mutate(YEAR = format(DATE, "%Y"))
   #
   # sdata <- dplyr::filter(data_event, IDV == ID) %>%
   #         transform(DATE = as.Date(DATE) ) %>%
   #         merge(., sseq, by = c("DATE", "IDV"), all.y = TRUE   ) %>%
   #         transform(FATALITIES =ifelse(is.na(FATALITIES), 0, FATALITIES),
   #                   EVENTS =ifelse(is.na(EVENTS), 0, EVENTS),
   #                   LOCATION =ifelse(is.na(LOCATION), 0, LOCATION)) %>%
   #         dplyr::group_by(YEAR) %>%
   #         dplyr::summarise(EVENTS = sum(EVENTS) )
   #
   # ldata <-  dplyr::filter(data_event, IDV == ID) %>%
   #         transform(DATE = as.Date(DATE) ) %>%
   #         merge(., lseq, by = c("DATE","YEAR", "IDV"), all.y = TRUE   ) %>%
   #         transform(FATALITIES =ifelse(is.na(FATALITIES), 0, FATALITIES),
   #                   EVENTS =ifelse(is.na(EVENTS), 0, EVENTS),
   #                   LOCATION =ifelse(is.na(LOCATION), 0, LOCATION)) %>%
   #         dplyr::group_by(YEAR) %>%
   #         dplyr::summarise(count = sum(EVENTS) )
   #
   # out <- data.frame(
   #                   as.character(as.yearmon(as.Date(data_sudates$date[data_sudates$IDV == ID]- short/12))), # short term begins
   #                   as.character(as.yearmon(as.Date(data_sudates$date[data_sudates$IDV == ID]  - 1/12))),
   #                   as.character(as.yearmon(as.Date(data_sudates$date[data_sudates$IDV == ID]- short/12 - long))),#2/12 - long) )),
   #                   as.character(as.yearmon(as.Date(data_sudates$date[data_sudates$IDV == ID]- short/12 - 1/12 - long))),#3/12) )),
   #                   # short term ends -- one month before survey
   #                   mean(sdata$EVENTS),
   #                   mean(ldata$count),
   #                   mean(sdata$EVENTS) - mean(ldata$count) )

   # colnames(out) <- c(  "t minus 4 start", "t minus 4_end",  "t minus 8_start", "t minus 8_end","mean(EVENTS in t minus 4)", "mean(EVENTS in t minus 8)", "Confl_Shock")

   out
 }




 # get prior months before stepD for each village
 prior_stepD <- function(ID, sdates = dates, short = 12, data_rain){
   survey_month <- as.yearmon(sdates$date[sdates$IDV == ID])
   short_start  <- as.Date(survey_month - short/12)
   short_end    <- as.Date(survey_month - 1/12)

   data_rain <- data_rain %>% subset(IDV == ID & !is.na(GPCP_ANOMALIES)) %>%
     transform(GPCP_ANOMALIES = abs(GPCP_ANOMALIES)) %>%
     dplyr::summarise(avg_rain_dev = mean(GPCP_ANOMALIES[DATE >= short_start & DATE <= short_end], na.rm = TRUE))

   data_rain$IDV <- ID
   data_rain

   # sseq <- data.frame(IDV = ID,
   #                    DATE = seq.Date(from = as.Date(as.yearmon(sdates$date[sdates$IDV == ID])-short/12),
   #                                    to = as.Date(as.yearmon(sdates$date[sdates$IDV == ID])  - 1/12), # we could include a few months during the time rapid was implemented
   #                                    by = "month"))
 }





 ######################################################
 #         Functions for spatial analysis             #
 ######################################################

 #' @param y dependent variable. e.g. C1, G1, G2, G3, G4, G
 #' @param rawdata unfiltered data
 #'
 geo_objects <- function(y,  rawdata){
   # prep data
   geodata <- dplyr::select(rawdata, IDV, MED_EW, MED_NS, rain, confl ,age,age2,lit , male, IDS_CHEF, IDS_RAPID ,y)%>%
   filter(complete.cases(.))
   geodata <- SpatialPointsDataFrame(coords      = geodata[,c("MED_EW","MED_NS")],
                                   data        = geodata,
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

   # compute distance matrix
   distG <- do.call(rbind, (lapply(1:nrow(geodata), function (x)  (distGeo(geodata@coords[x,], geodata@coords) ))))

   # Create weight matrix
   WG <- matrix(0, nrow = nrow(geodata), ncol = nrow(geodata))

   for (i in 1:nrow(geodata)) {
     nbG <- which(distG[i,] <= 15000 & distG[i,] > 0)
     idv_nbG <- which(geodata$IDV==geodata$IDV[i])
     idv_nbG <- idv_nbG[-i]

     WG[i,idv_nbG] <- 1
     WG[i,nbG] <-  round(1/(distG[i , nbG ]),2)
     WG[i,] <-  WG[i,]/sum(WG[i,], na.rm = TRUE)
   }

   # convert matrix to neighbor list
   listwdn <- mat2listw(WG)

   out <- list()
   out$WG <- WG
   out$listwdnG<-   listwdn
   out$geodata <- geodata

   out
 }

 get_ci <- function(y, my_data , listw ,level = 0.95, shock= c("rain", "confl")  ){
   # @shock: rain or confl
   # @y: dependent variable. e.g. G, C1
   # @listw; object of typ listw from package spdep
   # @level: confidence interval level


   formla <- paste(y,"~",  " rain + confl +IDS_RAPID + age+age2+lit + male + as.factor(IDS_CHEF)")
   arlm_object <- errorsarlm(formula = formla, data =  my_data, listw =  listw, zero.policy = TRUE)
   v <- do.call(rbind, lapply(shock, function (s) paste0("I(x - lambda * WX)"  , s, collapse = "" )))
   se <-  arlm_object$rest.se[v]

   a <- (1 - level)/2
   a <- c(a, 1 - a)
   fac <- qt(a, nrow( my_data) -arlm_object$parameters )
   ci <- arlm_object$coefficients[shock] + se%*%t(fac)

   return(cbind( ci[,1], arlm_object$coefficients[shock],ci[,2] ))
 }


 stargazer.arlm <- function(y, data, listw, out_vars = c("rain", "confl", "age", "age2", "lit", "male")){
   #@y: dependent variable. e.g. C1, G1, G2, G3, G4, G
   #@data: data for regression; filtered; no incomplete cases; use geoobject() to obtain data
   #@listw: object of type listw; see spdep package
   #@out_vars: variables shown in output

   formla <- paste(y,"~",  " rain + confl + age+age2+lit + male + as.factor(IDS_CHEF) +IDS_RAPID ")
   arlm_object1 <- errorsarlm(formula = formla, data = data, listw = listw, zero.policy = TRUE)

   # Regressors
   n <- length(out_vars)*2 + 4
   out <- matrix(" ", n, 1)
   summ.arlm <- summary.sarlm(arlm_object1 , Nagelkerke=TRUE)
   pvalues <- summ.arlm$Coef[out_vars,4]
   stars <- ifelse(pvalues < 0.01 ,"***",
                   ifelse(pvalues < 0.05 & pvalues>=0.01, "**",
                          ifelse(pvalues < 0.1 & pvalues>=0.05, "*", "")))
   out[seq(1,n-4,2)] <- paste0(round(summ.arlm$coefficients[out_vars],3),   stars)
   out[seq(2,n-4,2)] <- paste0("(", round(summ.arlm$Coef[out_vars,2],3),")")

   # Spatial Correlation
   lp.value <-  summ.arlm$LR1$p.value
   out[n-3] <- paste0(round(arlm_object1$lambda,3), ifelse(lp.value < 0.01 ,"***",
                                                           ifelse(lp.value < 0.05 & lp.value>=0.01, "**",
                                                                  ifelse(lp.value < 0.1 & lp.value>=0.05, "*", ""))))
   out[n-2] <- paste0("(", round(arlm_object1$lambda.se,3),")")

   # N and R2
   out[n-1] <- length(arlm_object1$y)
   out[n] <- round(summ.arlm$NK,3)

   out

 }

 shocks_appx <- function(y, hh ){
   print(y)
   #@ hh data set
   gobj <- geo_objects(y, rawdata = hh )

   formla <- paste(y,"~",  " rain + confl + age+age2+lit + male + as.factor(IDS_CHEF) +IDS_RAPID ")
   arlm_object <- errorsarlm(formula = formla, data = gobj$geodata, listw = gobj$listwdn, zero.policy = TRUE)
   summ.arlm <- summary.sarlm(arlm_object)

   out <- rep("",4)
   pvalues <- summ.arlm$Coef[c("rain", "confl"),4]
   stars <- ifelse(pvalues < 0.01 ,"***",
                   ifelse(pvalues < 0.05 & pvalues>=0.01, "**",
                          ifelse(pvalues < 0.1 & pvalues>=0.05, "*", "")))
   out[c(1,3)] <- paste0(round(summ.arlm$coefficients[c("rain", "confl")],3),   stars)
   out[c(2,4)] <- paste0("(", round(summ.arlm$Coef[c("rain", "confl"),2],3),")")

   out
 }

# END #
