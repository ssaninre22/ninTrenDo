# ********************************************************** #
# ********** Google Trends Database Building Tool ********** #
# ********************************************************** #

library(gtrendsR)
library(plyr)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)
library(lubridate)
library(timetk)
library(tempdisagg)
library(tsbox)

# keyword         <- "hotel"
# country         <- "CO"
# input.sdate     <- "2015-01-01"
# input.edate     <- "2021-04-16"
# input.frequency <- "m" # d:daily; w:weekly; m:monthly
# input.delta     <- 6 # Num. months, (depend on freq selected) each window
# input.ol.win    <- 1 # Num. months of overlapped periods
# input.type      <- "web" # Web, news, images, youtube, froogle
# input.categ     <- "0"
# td.method       <- "denton" #denton=denton-cholette,chow-lin=chow-lin-maxlog

long_gt <- function(keyword=NULL,country="",input.sdate,input.edate,
                           input.frequency="d",input.delta=6,input.ol.win=1,
                           input.type="web",input.categ="0"){
  # Initial checks ----
  if(!is.Date(input.sdate)){input.sdate <- as.Date(input.sdate)}
  if(!is.Date(input.edate)){input.edate <- as.Date(input.edate)}
  
  stopifnot(
    is.vector(keyword),length(keyword)<2,
    is.numeric(input.delta),is.numeric(input.ol.win),
    input.delta>input.ol.win
  )
  
  if(!country%in%c(unique(gtrendsR::countries$country_code),
                   unique(gtrendsR::countries$sub_code),
                   unique(gtrendsR::countries$name))&country!=""){
    stop("Country code not valid. Please use 'data(countries)' to 
         retreive valid codes.)")
  }
  if(!all(input.categ %in% categories[, "id"])){
    stop("Category code not valid. Please use 'data(categories)' 
         to retreive valid codes.",call. = FALSE)
  }
  if(input.sdate>input.edate){
    stop("Start date could not be lower than End date.",call. = FALSE)
  }
  if(year(input.sdate)<2005){
    stop("Initial year could not be before 2005",call. = F)
  }
  
  # Daily Gtrends ----
  if(input.frequency=="d"){
    num.months <- time_length(interval(input.sdate,input.edate),"months")
    if(num.months>9){
      edate <- ifelse(input.edate>=Sys.Date(),
                      input.edate-(input.edate-Sys.Date())-days(1),
                      input.edate) %>% as.Date(origin = '1970-01-01')
      sdate <- input.sdate
      delta <- input.delta
      delta.txt <- ifelse(delta==1,"1 month",paste0(delta," months"))
      ol.win <- input.ol.win
      ol.win.txt <- ifelse(ol.win==1,"1 month",paste0(ol.win," months"))
      full.win.txt <- paste0(ol.win+delta," months")
      
      # Backward date sequence
      n.windows <- ceiling(ceiling(num.months)/delta)
      bwd_sdate <- edate-months(n.windows*delta)
      edate0 <- edate-months(ol.win)
      
      sdate0 <- ifelse(bwd_sdate<sdate,bwd_sdate,
                       ifelse(bwd_sdate==sdate,sdate,
                              bwd_sdate-months(1))) %>% 
        as.Date(origin = '1970-01-01')
      
      pm <- tibble(s = seq(ymd(sdate0), ymd(edate0), by = delta.txt), 
                   e = seq(ymd(sdate0), ymd(edate0), by = delta.txt) + 
                     months(delta)+months(ol.win) -days(1))
      
      if(suppressWarnings(year(as.Date(as.numeric(pm[1,1]),
                                       origin = "1970-01-01"))<2005)){
        pm[1,1] <- as.Date("2005-01-01")
      }
      if(suppressWarnings(pm[nrow(pm),2]>=edate)){
        pm[nrow(pm),2] <- edate
      }
      
      # GTrends loop
      res <- data.frame(date=seq(sdate0,edate,1))
      cnames <- c("date")
      max.ol.mat <- matrix(NA,nrow = (nrow(pm)-1),ncol=ncol(pm))
      for(i in 1:nrow(pm)){
        print(paste0("GT sample = ",i,"/",nrow(pm)))
        # Download Gtrends data
        periods <- paste(as.Date(as.numeric(pm[i,1]),origin = '1970-01-01'), 
                         as.Date(as.numeric(pm[i,2]),origin = '1970-01-01'))
        
        temp <- gtrends(keyword = keyword, geo = country,time = periods,
                        onlyInterest = T,category = input.categ,
                        gprop = input.type)$interest_over_time %>% 
          dplyr::select(date,hits) %>% mutate(date=as.Date(date))
        
        cnames <- c(cnames,paste0("smpl",i))
        res <- res %>% merge(y=temp,by="date",all.x = T)
        colnames(res) <- cnames
        
        # Compute max
        
        if(i>1){
          x.overlap <- na.omit(res[,i:(i+1)])
          max.ol.mat[(i-1),1:2] <- apply(x.overlap,2,max,na.rm=T)
          
        }
        Sys.sleep(ceiling(runif(1,min=4,max=6)))
      }
      
      max.ol.mat <- rbind(c(100,100,1),
                          cbind(max.ol.mat,max.ol.mat[,2]/max.ol.mat[,1]))
      for(j in 1:nrow(pm)){
        res[,(j+1)] <- res[,(j+1)]/max.ol.mat[j,3]
        
      }
      # Overlap period indicator
      res <- res %>% 
        mutate(
          overlap = ifelse((rowSums(!is.na(res),na.rm = T)-1)==2,1,0)
        )
      
      res.full <- data.frame(date=res$date,overlap=res$overlap,
                             gt_full=rowMeans(res %>% 
                                                dplyr::select(-date,-overlap),
                                              na.rm=T))
      
      res.final <- res.full %>% filter(date>=input.sdate,date<=input.edate)
      
      g1 <- ggplot(res.final)+geom_line(aes(x=date,y=gt_full,col=gt_full))+
        geom_area(aes(x = date,y=100*overlap),alpha=0.15)+
        scale_color_continuous(low = "lightblue", high = "darkblue")+
        theme_bw()+labs(x="",y=paste0("Relative Volume Search: ",keyword),
                        caption = "Shaded areas refer to 
                        overlapping periods")+
        theme(legend.position = "bottom",legend.title = element_blank())
    }else{
      periods <- paste(as.Date(as.numeric(input.sdate),origin = '1970-01-01'), 
                       as.Date(as.numeric(input.edate),origin = '1970-01-01'))
      
      res <- gtrends(keyword = keyword, geo = country,time = periods,
                     onlyInterest = T,category = input.categ,
                     gprop = input.type)$interest_over_time %>% 
        dplyr::select(date,hits) %>% mutate(date=as.Date(date))
      
      res.full <- data.frame(date=res$date,gt_full=res$hits)
      res.final <- res.full %>% filter(date>=input.sdate,date<=input.edate)
      
      g1 <- ggplot(res.final)+geom_line(aes(x=date,y=gt_full,col=gt_full))+
        scale_color_continuous(low = "lightblue", high = "darkblue")+
        theme_bw()+labs(x="",y=paste0("Relative Volume Search: ",keyword))+
        theme(legend.position = "bottom",legend.title = element_blank())
      
    }
  }
  
  # Weekly Gtrends ----
  if(input.frequency=="w"){
    num.years <- time_length(interval(input.sdate,input.edate),
                             unit = "years")
    if(num.years>5){
      edate <- ifelse(floor_date(input.edate,"week")>=Sys.Date(),
                      floor_date(input.edate,"week")-
                        (floor_date(input.edate,"week")-Sys.Date())-days(1),
                      floor_date(input.edate,"week")) %>%
        as.Date(origin = '1970-01-01')
      
      
      sdate <- floor_date(input.sdate,"week")
      delta <- input.delta
      delta.txt <- ifelse(delta==1,"1 year",paste0(delta," years"))
      ol.win <- input.ol.win
      ol.win.txt <- ifelse(ol.win==1,"1 year",paste0(ol.win," years"))
      full.win.txt <- paste0(ol.win+delta," years")
      
      # Backward date sequence
      n.windows <- ceiling(ceiling(num.years)/delta)
      bwd_sdate <- floor_date(edate-years(n.windows*delta),"week")
      edate0 <- edate-years(ol.win)
      
      sdate0 <- ifelse(bwd_sdate<sdate,bwd_sdate,
                       ifelse(bwd_sdate==sdate,sdate,
                              bwd_sdate-years(1))) %>% 
        as.Date(origin = '1970-01-01')
      
      pm <- tibble(s = seq(ymd(sdate0), ymd(edate0), by = delta.txt), 
                   e = seq(ymd(sdate0), ymd(edate0), by = delta.txt) + 
                     years(delta)+years(ol.win)-1)
      
      if(suppressWarnings(year(as.Date(as.numeric(pm[1,1]),
                                       origin = "1970-01-01"))<2005)){
        pm[1,1] <- as.Date("2005-01-01")
      }
      if(suppressWarnings(pm[nrow(pm),2]>=edate)){
        pm[nrow(pm),2] <- edate
      }
      
      # GTrends loop
      res <- data.frame(date=seq(sdate0,edate,"1 week"))
      cnames <- c("date")
      max.ol.mat <- matrix(NA,nrow = (nrow(pm)-1),ncol=ncol(pm))
      for(i in 1:nrow(pm)){
        print(paste0("GT sample = ",i,"/",nrow(pm)))
        # Download Gtrends data
        periods <- paste(as.Date(as.numeric(pm[i,1]),origin = '1970-01-01'), 
                         as.Date(as.numeric(pm[i,2]),origin = '1970-01-01'))
        
        temp <- gtrends(keyword = keyword, geo = country,time = periods,
                        onlyInterest = T,category = input.categ,
                        gprop = input.type)$interest_over_time %>% 
          dplyr::select(date,hits) %>% mutate(date=as.Date(date))
        
        cnames <- c(cnames,paste0("smpl",i))
        res <- res %>% merge(y=temp,by="date",all.x = T)
        colnames(res) <- cnames
        
        # Compute max
        
        if(i>1){
          x.overlap <- na.omit(res[,i:(i+1)])
          max.ol.mat[(i-1),1:2] <- apply(x.overlap,2,max,na.rm=T)
          
        }
        Sys.sleep(ceiling(runif(1,min=4,max=6)))
      }
      
      max.ol.mat <- rbind(c(100,100,1),
                          cbind(max.ol.mat,max.ol.mat[,2]/max.ol.mat[,1]))
      for(j in 1:nrow(pm)){
        res[,(j+1)] <- res[,(j+1)]/max.ol.mat[j,3]
        
      }
      # Overlap period indicator
      res <- res %>% 
        mutate(
          overlap = ifelse((rowSums(!is.na(res),na.rm = T)-1)==2,1,0)
        )
      
      res.full <- data.frame(date=res$date,overlap=res$overlap,
                             gt_full=rowMeans(res %>% 
                                                dplyr::select(-date,-overlap),
                                              na.rm=T))
      
      res.final <- res.full %>% filter(date>=input.sdate,date<=input.edate)
      
      g1 <- ggplot(res.final)+geom_line(aes(x=date,y=gt_full,col=gt_full))+
        geom_area(aes(x = date,y=100*overlap),alpha=0.15)+
        scale_color_continuous(low = "lightblue", high = "darkblue")+
        theme_bw()+labs(x="",y=paste0("Relative Volume Search: ",keyword),
                        caption = "Shaded areas refer to 
                        overlapping periods")+
        theme(legend.position = "bottom",legend.title = element_blank())
      
    }else{
      periods <- paste(as.Date(as.numeric(input.sdate),origin = '1970-01-01'), 
                       as.Date(as.numeric(input.edate),origin = '1970-01-01'))
      
      res <- gtrends(keyword = keyword, geo = country,time = periods,
                     onlyInterest = T,category = input.categ,
                     gprop = input.type)$interest_over_time %>% 
        dplyr::select(date,hits) %>% mutate(date=as.Date(date))
      
      res.full <- data.frame(date=res$date,gt_full=res$hits)
      res.final <- res.full %>% filter(date>=input.sdate,date<=input.edate)
      
      g1 <- ggplot(res.final)+geom_line(aes(x=date,y=gt_full,col=gt_full))+
        scale_color_continuous(low = "lightblue", high = "darkblue")+
        theme_bw()+labs(x="",y=paste0("Relative Volume Search: ",keyword))+
        theme(legend.position = "bottom",legend.title = element_blank())
    }
  }
  # Monthly Gtrends ----
  if(input.frequency=="m"){
    periods <- paste(as.Date(as.numeric(input.sdate),origin = '1970-01-01'), 
                     as.Date(as.numeric(input.edate),origin = '1970-01-01'))
    
    res <- gtrends(keyword = keyword, geo = country,time = periods,
                   onlyInterest = T,category = input.categ,
                   gprop = input.type)$interest_over_time %>% 
      dplyr::select(date,hits) %>% mutate(date=as.Date(date))
    
    res.full <- data.frame(date=res$date,gt_full=res$hits)
    res.final <- res.full %>% filter(date>=input.sdate,date<=input.edate)
    
    # Summarize by month. If weekly or daily data present
    res.final <- res.final %>% 
      timetk::summarise_by_time(.date_var = date,
                                gt_full=mean(gt_full,na.rm=T),
                                .by = "month")
    
    g1 <- ggplot(res.final)+geom_line(aes(x=date,y=gt_full,col=gt_full))+
      scale_color_continuous(low = "lightblue", high = "darkblue")+
      theme_bw()+labs(x="",y=paste0("Relative Volume Search: ",keyword))+
      theme(legend.position = "bottom",legend.title = element_blank())
  }
  
  # Quarterly Gtrends ----
  if(input.frequency=="q"){
    periods <- paste(as.Date(as.numeric(input.sdate),origin = '1970-01-01'), 
                     as.Date(as.numeric(input.edate),origin = '1970-01-01'))
    
    res <- gtrends(keyword = keyword, geo = country,time = periods,
                   onlyInterest = T,category = input.categ,
                   gprop = input.type)$interest_over_time %>% 
      dplyr::select(date,hits) %>% mutate(date=as.Date(date))
    
    res.full <- data.frame(date=res$date,gt_full=res$hits)
    res.final <- res.full %>% filter(date>=input.sdate,date<=input.edate)
    
    # Summarize by month. If weekly or daily data present
    res.final <- res.final %>% 
      timetk::summarise_by_time(.date_var = date,
                                gt_full=mean(gt_full,na.rm=T),
                                .by = "quarter")
    
    g1 <- ggplot(res.final)+geom_line(aes(x=date,y=gt_full,col=gt_full))+
      scale_color_continuous(low = "lightblue", high = "darkblue")+
      theme_bw()+labs(x="",y=paste0("Relative Volume Search: ",keyword))+
      theme(legend.position = "bottom",legend.title = element_blank())
  }
  
  ret <- list(final=res.final,plot=g1)
  return(ret)
}

long_gt_ltc <- function(keyword=NULL,country="",input.sdate,input.edate,
                           input.frequency="d",td.method="chow-lin",
                           input.delta=6,input.ol.win=1,
                           input.type="web",input.categ="0"){
  # Initial checks ----
  if(!is.Date(input.sdate)){input.sdate <- as.Date(input.sdate)}
  if(!is.Date(input.edate)){input.edate <- as.Date(input.edate)}
  
  stopifnot(
    is.vector(keyword),length(keyword)<2,
    is.numeric(input.delta),is.numeric(input.ol.win),
    input.delta>input.ol.win
  )
  
  if(!country%in%c(unique(gtrendsR::countries$country_code),
                   unique(gtrendsR::countries$sub_code),
                   unique(gtrendsR::countries$name))&country!=""){
    stop("Country code not valid. Please use 'data(countries)' to 
         retreive valid codes.)")
  }
  if(!all(input.categ %in% categories[, "id"])){
    stop("Category code not valid. Please use 'data(categories)' 
         to retreive valid codes.",call. = FALSE)
  }
  if(input.sdate>input.edate){
    stop("Start date could not be lower than End date.",call. = FALSE)
  }
  if(year(input.sdate)<2005){
    stop("Initial year could not be before 2005",call. = F)
  }
  
  # Daily Gtrends ----
  if(input.frequency=="d"){
    num.months <- time_length(interval(input.sdate,input.edate),"months")
    num.years <- time_length(interval(input.sdate,input.edate),"years")
    if(num.months>9){
      edate <- ifelse(input.edate>=Sys.Date(),
                      input.edate-(input.edate-Sys.Date())-days(1),
                      input.edate) %>% as.Date(origin = '1970-01-01')
      sdate <- input.sdate
      delta <- input.delta
      delta.txt <- ifelse(delta==1,"1 month",paste0(delta," months"))
      ol.win <- input.ol.win
      ol.win.txt <- ifelse(ol.win==1,"1 month",paste0(ol.win," months"))
      full.win.txt <- paste0(ol.win+delta," months")
      
      # Backward date sequence
      n.windows <- ceiling(ceiling(num.months)/delta)
      bwd_sdate <- edate-months(n.windows*delta)
      edate0 <- edate-months(ol.win)
      
      sdate0 <- ifelse(bwd_sdate<sdate,bwd_sdate,
                       ifelse(bwd_sdate==sdate,sdate,
                              bwd_sdate-months(1))) %>% 
        as.Date(origin = '1970-01-01')
      
      pm <- tibble(s = seq(ymd(sdate0), ymd(edate0), by = delta.txt), 
                   e = seq(ymd(sdate0), ymd(edate0), by = delta.txt) + 
                     months(delta)+months(ol.win) -days(1))
      
      if(suppressWarnings(year(as.Date(as.numeric(pm[1,1]),
                                       origin = "1970-01-01"))<2005)){
        pm[1,1] <- as.Date("2005-01-01")
      }
      if(suppressWarnings(pm[nrow(pm),2]>=edate)){
        pm[nrow(pm),2] <- edate
      }
      
      # GTrends loop
      res <- data.frame(date=seq(sdate0,edate,1))
      cnames <- c("date")
      max.ol.mat <- matrix(NA,nrow = (nrow(pm)-1),ncol=ncol(pm))
      for(i in 1:nrow(pm)){
        print(paste0("GT sample = ",i,"/",nrow(pm)))
        # Download Gtrends data
        periods <- paste(as.Date(as.numeric(pm[i,1]),origin = '1970-01-01'), 
                         as.Date(as.numeric(pm[i,2]),origin = '1970-01-01'))
        
        temp <- gtrends(keyword = keyword, geo = country,time = periods,
                        onlyInterest = T,category = input.categ,
                        gprop = input.type)$interest_over_time %>% 
          dplyr::select(date,hits) %>% mutate(date=as.Date(date))
        
        cnames <- c(cnames,paste0("smpl",i))
        res <- res %>% merge(y=temp,by="date",all.x = T)
        colnames(res) <- cnames
        
        # Compute max
        
        if(i>1){
          x.overlap <- na.omit(res[,i:(i+1)])
          max.ol.mat[(i-1),1:2] <- apply(x.overlap,2,max,na.rm=T)
          
        }
        Sys.sleep(ceiling(runif(1,min=2,max=5)))
      }
      
      max.ol.mat <- rbind(c(100,100,1),
                          cbind(max.ol.mat,max.ol.mat[,2]/max.ol.mat[,1]))
      for(j in 1:nrow(pm)){
        res[,(j+1)] <- res[,(j+1)]/max.ol.mat[j,3]
        
      }
      # Overlap period indicator
      res <- res %>% 
        mutate(
          overlap = ifelse((rowSums(!is.na(res),na.rm = T)-1)==2,1,0)
        )
      
      res.full <- data.frame(date=res$date,overlap=res$overlap,
                             gt_full=rowMeans(res %>% 
                                                dplyr::select(-date,-overlap),
                                              na.rm=T))
      
      res.final <- res.full %>% filter(date>=input.sdate,date<=input.edate)

      # Monthly long-term data
      if(num.years>5){
        print(paste0("Correction with monthly trend in process - Method: ",
                     td.method))
        mdates <- paste(input.sdate,input.edate)
        mres <- gtrends(keyword = keyword, geo = country,time = mdates,
                        onlyInterest = T,category = input.categ,
                        gprop = input.type)$interest_over_time %>% 
          dplyr::select(date,hits) %>% mutate(date=as.Date(date)) 
          
        
        lf.res <- ts(mres$hits,start = c(year(first(mres$date)),
                                         month(first(mres$date))),
                     end = c(year(last(mres$date)),
                             month(last(mres$date))),frequency = 12)
        
        
        hf.res <- res.final %>% select(-overlap) %>% 
          rename(time="date",value="gt_full")
          
        if(td.method=="denton"){
          td.res <- tempdisagg::td(lf.res~0+hf.res,conversion="average",
                                   method = "denton-cholette")
        }else{
          td.res <- tempdisagg::td(lf.res~1+hf.res,conversion="mean")
        }
        
        res.f.corr <- predict(td.res)
        res.f.corr$overlap <- res.final$overlap
        res.f.corr <- res.f.corr %>% rename(date="time",gt_full="value")
        
        mres <- mres %>% mutate(date=ceiling_date(date,"month")-15)
        
        g1 <- ggplot()+
          geom_line(data=res.f.corr,aes(x=date,y=gt_full,col=gt_full))+
          geom_area(data=res.f.corr,aes(x =date,y=max(gt_full)*overlap),
                    alpha=0.15)+
          geom_line(data=mres,aes(x=date,y=hits),col="red")+
          scale_color_continuous(low = "lightblue", high = "darkblue")+
          theme_bw()+
          labs(x="",y=paste0("Corrected RVS: ",keyword),
               caption = "Shaded areas refer to overlapping periods. 
               Red line is monthly search")+
          theme(legend.position = "bottom",legend.title = element_blank())
        
      }else{
        print(paste0("No correction done - horizon is lower than 5 years"))
        res.f.corr <- res.final
        
        g1 <- ggplot(res.final)+geom_line(aes(x=date,y=gt_full,col=gt_full))+
          geom_area(aes(x = date,y=max(value)*overlap),alpha=0.15)+
          scale_color_continuous(low = "lightblue", high = "darkblue")+
          theme_bw()+labs(x="",y=paste0("Relative Volume Search: ",keyword),
                          caption = "Shaded areas refer to 
                        overlapping periods")+
          theme(legend.position = "bottom",legend.title = element_blank())
        
      }
      
    }else{
      periods <- paste(as.Date(as.numeric(input.sdate),origin = '1970-01-01'), 
                       as.Date(as.numeric(input.edate),origin = '1970-01-01'))
      
      res <- gtrends(keyword = keyword, geo = country,time = periods,
                     onlyInterest = T,category = input.categ,
                     gprop = input.type)$interest_over_time %>% 
        dplyr::select(date,hits) %>% mutate(date=as.Date(date))
      
      res.full <- data.frame(date=res$date,gt_full=res$hits)
      res.final <- res.full %>% filter(date>=input.sdate,date<=input.edate)
      
      g1 <- ggplot(res.final)+geom_line(aes(x=date,y=gt_full,col=gt_full))+
        scale_color_continuous(low = "lightblue", high = "darkblue")+
        theme_bw()+labs(x="",y=paste0("Relative Volume Search: ",keyword))+
        theme(legend.position = "bottom",legend.title = element_blank())
      
      res.f.corr <- res.final
    }
  }
  
  # Weekly Gtrends ----
  if(input.frequency=="w"){
    num.years <- time_length(interval(input.sdate,input.edate),
                             unit = "years")
    if(num.years>5){
      edate <- ifelse(floor_date(input.edate,"week")>=Sys.Date(),
                      floor_date(input.edate,"week")-
                        (floor_date(input.edate,"week")-Sys.Date())-days(1),
                      floor_date(input.edate,"week")) %>%
        as.Date(origin = '1970-01-01')
      
      
      sdate <- floor_date(input.sdate,"week")
      delta <- input.delta
      delta.txt <- ifelse(delta==1,"1 year",paste0(delta," years"))
      ol.win <- input.ol.win
      ol.win.txt <- ifelse(ol.win==1,"1 year",paste0(ol.win," years"))
      full.win.txt <- paste0(ol.win+delta," years")
      
      # Backward date sequence
      n.windows <- ceiling(ceiling(num.years)/delta)
      bwd_sdate <- floor_date(edate-years(n.windows*delta),"week")
      edate0 <- edate-years(ol.win)
      
      sdate0 <- ifelse(bwd_sdate<sdate,bwd_sdate,
                       ifelse(bwd_sdate==sdate,sdate,
                              bwd_sdate-years(1))) %>% 
        as.Date(origin = '1970-01-01')
      
      pm <- tibble(s = seq(ymd(sdate0), ymd(edate0), by = delta.txt), 
                   e = seq(ymd(sdate0), ymd(edate0), by = delta.txt) + 
                     years(delta)+years(ol.win)-1)
      
      if(suppressWarnings(year(as.Date(as.numeric(pm[1,1]),
                                       origin = "1970-01-01"))<2005)){
        pm[1,1] <- as.Date("2005-01-01")
      }
      if(suppressWarnings(pm[nrow(pm),2]>=edate)){
        pm[nrow(pm),2] <- edate
      }
      
      # GTrends loop
      res <- data.frame(date=seq(sdate0,edate,"1 week"))
      cnames <- c("date")
      max.ol.mat <- matrix(NA,nrow = (nrow(pm)-1),ncol=ncol(pm))
      for(i in 1:nrow(pm)){
        print(paste0("GT sample = ",i,"/",nrow(pm)))
        # Download Gtrends data
        periods <- paste(as.Date(as.numeric(pm[i,1]),origin = '1970-01-01'), 
                         as.Date(as.numeric(pm[i,2]),origin = '1970-01-01'))
        
        temp <- gtrends(keyword = keyword, geo = country,time = periods,
                        onlyInterest = T,category = input.categ,
                        gprop = input.type)$interest_over_time %>% 
          dplyr::select(date,hits) %>% mutate(date=as.Date(date))
        
        cnames <- c(cnames,paste0("smpl",i))
        res <- res %>% merge(y=temp,by="date",all.x = T)
        colnames(res) <- cnames
        
        # Compute max
        
        if(i>1){
          x.overlap <- na.omit(res[,i:(i+1)])
          max.ol.mat[(i-1),1:2] <- apply(x.overlap,2,max,na.rm=T)
          
        }
        Sys.sleep(ceiling(runif(1,min=2,max=5)))
      }
      
      max.ol.mat <- rbind(c(100,100,1),
                          cbind(max.ol.mat,max.ol.mat[,2]/max.ol.mat[,1]))
      for(j in 1:nrow(pm)){
        res[,(j+1)] <- res[,(j+1)]/max.ol.mat[j,3]
        
      }
      # Overlap period indicator
      res <- res %>% 
        mutate(
          overlap = ifelse((rowSums(!is.na(res),na.rm = T)-1)==2,1,0)
        )
      
      res.full <- data.frame(date=res$date,overlap=res$overlap,
                             gt_full=rowMeans(res %>% 
                                                dplyr::select(-date,-overlap),
                                              na.rm=T))
      
      res.final <- res.full %>% filter(date>=input.sdate,date<=input.edate)

      # Monthly long-term data
      if(num.years>5){
        mdates <- paste(input.sdate,input.edate)
        mres <- gtrends(keyword = keyword, geo = country,time = mdates,
                        onlyInterest = T,category = input.categ,
                        gprop = input.type)$interest_over_time %>% 
          dplyr::select(date,hits) %>% mutate(date=as.Date(date))
        
        lf.res <- ts(mres$hits,start = c(year(first(mres$date)),
                                         month(first(mres$date))),
                     end = c(year(last(mres$date)),
                             month(last(mres$date))),frequency = 12)
        
        hf.res <- res.final %>% select(-overlap) %>% 
          rename(time="date",value="gt_full")
        
        if(td.method=="denton"){
          td.res <- tempdisagg::td(lf.res~0+hf.res,conversion="mean",
                                   method = "denton-cholette")
        }else{
          td.res <- tempdisagg::td(lf.res~1+hf.res,conversion="mean")
        }
        
        res.f.corr <- predict(td.res)
        res.f.corr$overlap <- res.final$overlap
        res.f.corr <- res.f.corr %>% rename(date="time",gt_full="value")
        res.f.corr <- res.f.corr %>% mutate(date=as.Date(date))
        
        mres <- mres %>% mutate(date=ceiling_date(date,"month")-15)
        
        g1 <- ggplot()+
          geom_line(data=res.f.corr,aes(x=date,y=gt_full,col=gt_full))+
          geom_area(data=res.f.corr,aes(x =date,y=max(gt_full)*overlap),
                    alpha=0.15)+
          geom_line(data=mres,aes(x=date,y=hits),col="red")+
          scale_color_continuous(low = "lightblue", high = "darkblue")+
          theme_bw()+
          labs(x="",y=paste0("Corrected RVS: ",keyword),
               caption = "Shaded areas refer to overlapping periods. 
               Red line is monthly search")+
          theme(legend.position = "bottom",legend.title = element_blank())
        
      }else{
        print(paste0("No correction done - horizon is lower than 5 years"))
        res.f.corr <- res.final
        
        g1 <- ggplot(res.f.corr)+geom_line(aes(x=date,y=gt_full,col=gt_full))+
          geom_area(aes(x = date,y=max(value)*overlap),alpha=0.15)+
          scale_color_continuous(low = "lightblue", high = "darkblue")+
          theme_bw()+labs(x="",y=paste0("Relative Volume Search: ",keyword),
                          caption = "Shaded areas refer to 
                        overlapping periods")+
          theme(legend.position = "bottom",legend.title = element_blank())
      }
      
      
    }else{
      periods <- paste(as.Date(as.numeric(input.sdate),origin = '1970-01-01'), 
                       as.Date(as.numeric(input.edate),origin = '1970-01-01'))
      
      res <- gtrends(keyword = keyword, geo = country,time = periods,
                     onlyInterest = T,category = input.categ,
                     gprop = input.type)$interest_over_time %>% 
        dplyr::select(date,hits) %>% mutate(date=as.Date(date))
      
      res.full <- data.frame(date=res$date,gt_full=res$hits)
      res.final <- res.full %>% filter(date>=input.sdate,date<=input.edate)
      
      g1 <- ggplot(res.final)+geom_line(aes(x=date,y=gt_full,col=gt_full))+
        scale_color_continuous(low = "lightblue", high = "darkblue")+
        theme_bw()+labs(x="",y=paste0("Relative Volume Search: ",keyword))+
        theme(legend.position = "bottom",legend.title = element_blank())
      res.f.corr <- res.final
    }
  }
  
  # Monthly Gtrends ----
  if(input.frequency=="m"){
    print("Neither interpolation nor correction done in monthly searches")
    periods <- paste(as.Date(as.numeric(input.sdate),origin = '1970-01-01'), 
                     as.Date(as.numeric(input.edate),origin = '1970-01-01'))
    
    res <- gtrends(keyword = keyword, geo = country,time = periods,
                   onlyInterest = T,category = input.categ,
                   gprop = input.type)$interest_over_time %>% 
      dplyr::select(date,hits) %>% mutate(date=as.Date(date))
    
    res.full <- data.frame(date=res$date,gt_full=res$hits)
    res.final <- res.full %>% filter(date>=input.sdate,date<=input.edate)
    
    # Summarize by month. If weekly or daily data present
    res.final <- res.final %>% 
      timetk::summarise_by_time(.date_var = date,
                                gt_full=mean(gt_full,na.rm=T),
                                .by = "month")
    
    g1 <- ggplot(res.final)+geom_line(aes(x=date,y=gt_full,col=gt_full))+
      scale_color_continuous(low = "lightblue", high = "darkblue")+
      theme_bw()+labs(x="",y=paste0("Relative Volume Search: ",keyword))+
      theme(legend.position = "bottom",legend.title = element_blank())
    res.f.corr <- res.final
  }
  
  # Quarterly Gtrends ----
  if(input.frequency=="q"){
    print("Neither interpolation nor correction done in quarterly searches")
    periods <- paste(as.Date(as.numeric(input.sdate),origin = '1970-01-01'), 
                     as.Date(as.numeric(input.edate),origin = '1970-01-01'))
    
    res <- gtrends(keyword = keyword, geo = country,time = periods,
                   onlyInterest = T,category = input.categ,
                   gprop = input.type)$interest_over_time %>% 
      dplyr::select(date,hits) %>% mutate(date=as.Date(date))
    
    res.full <- data.frame(date=res$date,gt_full=res$hits)
    res.final <- res.full %>% filter(date>=input.sdate,date<=input.edate)
    
    # Summarize by month. If weekly or daily data present
    res.final <- res.final %>% 
      timetk::summarise_by_time(.date_var = date,
                                gt_full=mean(gt_full,na.rm=T),
                                .by = "quarter")
    
    g1 <- ggplot(res.final)+geom_line(aes(x=date,y=gt_full,col=gt_full))+
      scale_color_continuous(low = "lightblue", high = "darkblue")+
      theme_bw()+labs(x="",y=paste0("Relative Volume Search: ",keyword))+
      theme(legend.position = "bottom",legend.title = element_blank())
    res.f.corr <- res.final
  }
  
  ret <- list(final=res.f.corr,plot=g1)
  return(ret)
}


