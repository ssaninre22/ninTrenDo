#' Long term Google Trends queries by overlapped windows
#'
#' @description The long_gt method performs Google Trends queries for the 'keyword' argument for
#' the period spanned between starting and ending date by overlapping windows. The function
#' purpose is to produce long term high frequency Google Trends series by overlapping
#' queries on the same 'keyword' but different time span. Regular queries of short 
#' duration periods could also be performed.
#' 
#' @param keyword A character vector with the actual Google Trends query keywords. Multiple
#'                keywords are NOT possible right now.
#' @param geo    A character vector denoting geographic regions for the query, default to
#'              all for global queries. Multiple regions are NOT possible right now.
#' @param input.sdate Starting date for the query search. Should be Date format "Y-m-d".
#' @param input.edate Ending date for the query search. Should be Date format "Y-m-d".
#' @param input.frequency Frequency of the resulting query. Valid options are:
#' * "d" - Daily (default).
#' * "w" - Weekly.
#' * "m" - Monthly.
#' * "q" - Quarterly.
#' @param input.delta An integer indicating the window length of the query in months 
#'                    (if input.frequency="d") or years (if input.frequency="w"). 
#'                    Notice that it will be not used in another case.
#' @param input.ol.win An integer indicating the overlapping window length. 
#'                     Default is 1 (month/year).
#' @param input.type  A character string defining the Google product for which the trend 
#'                    query if preformed. Valid options are: "web" (default), "news",
#'                    "images", "froogle", "youtube".
#' @param input.categ A character denoting the category, defaults to “0”.
#' @return An object of class list containing two objects: A data.frame object with 
#'         columns date, Google Trends indicator and an overlapped period dummy (if 
#'         daily or weekly query is performed). A ggplot figure of the resulting time
#'         series with shaded areas showing overlapping periods.
#' @details The function purpose is to produce long term high frequency Google Trends 
#' series by overlapping queries of the same 'keyword' but with different time span. 
#' Regular queries of short periods could also be performed. The function is time consuming
#' due to the need to perform different queries, and a random sleep time between 2 and 5
#' seconds for each query step in order to prevent that Google blocks the IP.
#' @author Sebastian Sanin-Restrepo
#' @seealso long_gt_ltc
#' @importFrom dplyr %>% 
#' @export
#' @examples
#' # Search for daily data on word hotel in Colombia from 2014 to 2021
#'  res <- long_gt(keyword = "hotel",geo="CO",
#'                 input.sdate = as.Date("2014-01-01"),
#'                 input.edate = as.Date("2021-04-16"),
#'                 input.frequency = "d",input.delta = 6,
#'                 input.ol.win = 1,input.type="web",input.categ=0)


long_gt <- function(keyword=NULL,geo="",input.sdate,input.edate,
                    input.frequency="d",input.delta=6,input.ol.win=1,
                    input.type="web",input.categ=0) {
  # option 2
  hits <- overlap <- gt_full <- NULL
  
  
  # Initial checks ----
  if(!lubridate::is.Date(input.sdate)){input.sdate <- as.Date(input.sdate)}
  if(!lubridate::is.Date(input.edate)){input.edate <- as.Date(input.edate)}
  
  stopifnot(
    is.vector(keyword),length(keyword)<2,
    is.numeric(input.delta),is.numeric(input.ol.win),
    input.delta>input.ol.win
  )
  
  if(!geo%in%c(unique(gtrendsR::countries$country_code),
                   unique(gtrendsR::countries$sub_code),
                   unique(gtrendsR::countries$name))&geo!=""){
    stop("Country code not valid. Please use 'data(countries)' to 
         retreive valid codes.)")
  }
  if(!input.categ %in% gtrendsR::categories[, "id"]){
    stop("Category code not valid. Please use 'data(categories)' 
         to retreive valid codes.",call. = FALSE)
  }
  if(input.sdate>input.edate){
    stop("Start date could not be lower than End date.",call. = FALSE)
  }
  
  if(lubridate::year(input.sdate)<2005){
    stop("Initial year could not be before 2005",call. = F)
  }
  
  # Daily Gtrends ----
  if(input.frequency=="d"){
    num.months <- lubridate::time_length(
      lubridate::interval(input.sdate,input.edate),"months")
    if(num.months>9){
      edate <- ifelse(input.edate>=Sys.Date(),
                      input.edate-(input.edate-Sys.Date())-lubridate::days(1),
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
      
      pm <- dplyr::tibble(s = seq(lubridate::ymd(sdate0),
                                   lubridate::ymd(edate0), by = delta.txt), 
                   e = seq(lubridate::ymd(sdate0),
                           lubridate::ymd(edate0), by = delta.txt) + 
                     months(delta)+months(ol.win) -lubridate::days(1))
      
      if(suppressWarnings(lubridate::year(as.Date(as.numeric(pm[1,1]),
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
        
        temp <- gtrendsR::gtrends(keyword = keyword, geo = geo,time = periods,
                        onlyInterest = T,category = input.categ,
                        gprop = input.type)$interest_over_time %>% 
          dplyr::select(date,hits) %>% dplyr::mutate(date=as.Date(date))
        
        cnames <- c(cnames,paste0("smpl",i))
        res <- res %>% merge(y=temp,by="date",all.x = T)
        colnames(res) <- cnames
        
        # Compute max
        
        if(i>1){
          x.overlap <- stats::na.omit(res[,i:(i+1)])
          max.ol.mat[(i-1),1:2] <- apply(x.overlap,2,max,na.rm=T)
          
        }
        Sys.sleep(ceiling(stats::runif(1,min=4,max=6)))
      }
      
      max.ol.mat <- rbind(c(100,100,1),
                          cbind(max.ol.mat,max.ol.mat[,2]/max.ol.mat[,1]))
      for(j in 1:nrow(pm)){
        res[,(j+1)] <- res[,(j+1)]/max.ol.mat[j,3]
      }
      # Overlap period indicator
      res <- res %>% 
        dplyr::mutate(
          overlap = ifelse((rowSums(!is.na(res),na.rm = T)-1)==2,1,0)
        )
      
      res.full <- data.frame(date=res$date,overlap=res$overlap,
                             gt_full=rowMeans(res %>% 
                                                dplyr::select(-date,-overlap),
                                              na.rm=T))
      
      res.final <- res.full %>% dplyr::filter(date>=input.sdate,date<=input.edate)
      
      g1 <- ggplot2::ggplot(res.final)+
        ggplot2::geom_line(ggplot2::aes(x=date,y=gt_full,col=gt_full))+
        ggplot2::geom_area(ggplot2::aes(x = date,y=100*overlap),alpha=0.15)+
        ggplot2::scale_color_continuous(low = "lightblue", high = "darkblue")+
        ggplot2::theme_bw()+
        ggplot2::labs(x="",y=paste0("Relative Volume Search: ",keyword),
                        caption = "Shaded areas refer to 
                        overlapping periods")+
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())
    }else{
      periods <- paste(as.Date(as.numeric(input.sdate),origin = '1970-01-01'), 
                       as.Date(as.numeric(input.edate),origin = '1970-01-01'))
      
      res <- gtrendsR::gtrends(keyword = keyword, geo = geo,time = periods,
                     onlyInterest = T,category = input.categ,
                     gprop = input.type)$interest_over_time %>% 
        dplyr::select(date,hits) %>% dplyr::mutate(date=as.Date(date))
      
      res.full <- data.frame(date=res$date,gt_full=res$hits)
      res.final <- res.full %>% dplyr::filter(date>=input.sdate,date<=input.edate)
      
      g1 <- ggplot2::ggplot(res.final)+
        ggplot2::geom_line(ggplot2::aes(x=date,y=gt_full,col=gt_full))+
        ggplot2::scale_color_continuous(low = "lightblue", high = "darkblue")+
        ggplot2::theme_bw()+
        ggplot2::labs(x="",y=paste0("Relative Volume Search: ",keyword))+
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())
      
    }
  }
  
  # Weekly Gtrends ----
  if(input.frequency=="w"){
    num.years <- lubridate::time_length(
      lubridate::interval(input.sdate,input.edate),
                             unit = "years")
    if(num.years>5){
      edate <- ifelse(lubridate::floor_date(input.edate,"week")>=Sys.Date(),
                      lubridate::floor_date(input.edate,"week")-
                        (lubridate::floor_date(input.edate,"week")-
                           Sys.Date())-lubridate::days(1),
                      lubridate::floor_date(input.edate,"week")) %>%
        as.Date(origin = '1970-01-01')
      
      
      sdate <- lubridate::floor_date(input.sdate,"week")
      delta <- input.delta
      delta.txt <- ifelse(delta==1,"1 year",paste0(delta," years"))
      ol.win <- input.ol.win
      ol.win.txt <- ifelse(ol.win==1,"1 year",paste0(ol.win," years"))
      full.win.txt <- paste0(ol.win+delta," years")
      
      # Backward date sequence
      n.windows <- ceiling(ceiling(num.years)/delta)
      bwd_sdate <- 
        lubridate::floor_date(edate-lubridate::years(n.windows*delta),"week")
      edate0 <- edate-lubridate::years(ol.win)
      
      sdate0 <- ifelse(bwd_sdate<sdate,bwd_sdate,
                       ifelse(bwd_sdate==sdate,sdate,
                              bwd_sdate-lubridate::years(1))) %>% 
        as.Date(origin = '1970-01-01')
      
      pm <- dplyr::tibble(s = seq(lubridate::ymd(sdate0), 
                           lubridate::ymd(edate0), by = delta.txt), 
                   e = seq(lubridate::ymd(sdate0), 
                           lubridate::ymd(edate0), by = delta.txt) + 
                     lubridate::years(delta)+lubridate::years(ol.win)-1)
      
      if(suppressWarnings(lubridate::year(as.Date(as.numeric(pm[1,1]),
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
        
        temp <- gtrendsR::gtrends(keyword = keyword, geo = geo,time = periods,
                        onlyInterest = T,category = input.categ,
                        gprop = input.type)$interest_over_time %>% 
          dplyr::select(date,hits) %>% dplyr::mutate(date=as.Date(date))
        
        cnames <- c(cnames,paste0("smpl",i))
        res <- res %>% merge(y=temp,by="date",all.x = T)
        colnames(res) <- cnames
        
        # Compute max
        
        if(i>1){
          x.overlap <- stats::na.omit(res[,i:(i+1)])
          max.ol.mat[(i-1),1:2] <- apply(x.overlap,2,max,na.rm=T)
          
        }
        Sys.sleep(ceiling(stats::runif(1,min=4,max=6)))
      }
      
      max.ol.mat <- rbind(c(100,100,1),
                          cbind(max.ol.mat,max.ol.mat[,2]/max.ol.mat[,1]))
      for(j in 1:nrow(pm)){
        res[,(j+1)] <- res[,(j+1)]/max.ol.mat[j,3]
        
      }
      # Overlap period indicator
      res <- res %>% 
        dplyr::mutate(
          overlap = ifelse((rowSums(!is.na(res),na.rm = T)-1)==2,1,0)
        )
      
      res.full <- data.frame(date=res$date,overlap=res$overlap,
                             gt_full=rowMeans(res %>% 
                                                dplyr::select(-date,-overlap),
                                              na.rm=T))
      
      res.final <- res.full %>% dplyr::filter(date>=input.sdate,date<=input.edate)
      
      g1 <- ggplot2::ggplot(res.final)+
        ggplot2::geom_line(ggplot2::aes(x=date,y=gt_full,col=gt_full))+
        ggplot2::geom_area(ggplot2::aes(x = date,y=100*overlap),alpha=0.15)+
        ggplot2::scale_color_continuous(low = "lightblue", high = "darkblue")+
        ggplot2::theme_bw()+
        ggplot2::labs(x="",y=paste0("Relative Volume Search: ",keyword),
                        caption = "Shaded areas refer to 
                        overlapping periods")+
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())
      
    }else{
      periods <- paste(as.Date(as.numeric(input.sdate),origin = '1970-01-01'), 
                       as.Date(as.numeric(input.edate),origin = '1970-01-01'))
      
      res <- gtrendsR::gtrends(keyword = keyword, geo = geo,time = periods,
                     onlyInterest = T,category = input.categ,
                     gprop = input.type)$interest_over_time %>% 
        dplyr::select(date,hits) %>% dplyr::mutate(date=as.Date(date))
      
      res.full <- data.frame(date=res$date,gt_full=res$hits)
      res.final <- res.full %>% dplyr::filter(date>=input.sdate,date<=input.edate)
      
      g1 <- ggplot2::ggplot(res.final)+
        ggplot2::geom_line(ggplot2::aes(x=date,y=gt_full,col=gt_full))+
        ggplot2::scale_color_continuous(low = "lightblue", high = "darkblue")+
        ggplot2::theme_bw()+
        ggplot2::labs(x="",y=paste0("Relative Volume Search: ",keyword))+
        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_blank())
    }
  }
  # Monthly Gtrends ----
  if(input.frequency=="m"){
    periods <- paste(as.Date(as.numeric(input.sdate),origin = '1970-01-01'), 
                     as.Date(as.numeric(input.edate),origin = '1970-01-01'))
    
    res <- gtrendsR::gtrends(keyword = keyword, geo = geo,time = periods,
                   onlyInterest = T,category = input.categ,
                   gprop = input.type)$interest_over_time %>% 
      dplyr::select(date,hits) %>% dplyr::mutate(date=as.Date(date))
    
    res.full <- data.frame(date=res$date,gt_full=res$hits)
    res.final <- res.full %>% dplyr::filter(date>=input.sdate,date<=input.edate)
    
    # Summarize by month. If weekly or daily data present
    res.final <- res.final %>% 
      timetk::summarise_by_time(.date_var = date,
                                gt_full=mean(gt_full,na.rm=T),
                                .by = "month")
    
    g1 <- ggplot2::ggplot(res.final)+
      ggplot2::geom_line(ggplot2::aes(x=date,y=gt_full,col=gt_full))+
      ggplot2::scale_color_continuous(low = "lightblue", high = "darkblue")+
      ggplot2::theme_bw()+
      ggplot2::labs(x="",y=paste0("Relative Volume Search: ",keyword))+
      ggplot2::theme(legend.position = "bottom",
                     legend.title = ggplot2::element_blank())
  }
  
  # Quarterly Gtrends ----
  if(input.frequency=="q"){
    periods <- paste(as.Date(as.numeric(input.sdate),origin = '1970-01-01'), 
                     as.Date(as.numeric(input.edate),origin = '1970-01-01'))
    
    res <- gtrendsR::gtrends(keyword = keyword, geo = geo,time = periods,
                   onlyInterest = T,category = input.categ,
                   gprop = input.type)$interest_over_time %>% 
      dplyr::select(date,hits) %>% dplyr::mutate(date=as.Date(date))
    
    res.full <- data.frame(date=res$date,gt_full=res$hits)
    res.final <- res.full %>% dplyr::filter(date>=input.sdate,date<=input.edate)
    
    # Summarize by month. If weekly or daily data present
    res.final <- res.final %>% 
      timetk::summarise_by_time(.date_var = date,
                                gt_full=mean(gt_full,na.rm=T),
                                .by = "quarter")
    
    g1 <- ggplot2::ggplot(res.final)+
      ggplot2::geom_line(ggplot2::aes(x=date,y=gt_full,col=gt_full))+
      ggplot2::scale_color_continuous(low = "lightblue", high = "darkblue")+
      ggplot2::theme_bw()+
      ggplot2::labs(x="",y=paste0("Relative Volume Search: ",keyword))+
      ggplot2::theme(legend.position = "bottom",
                     legend.title = ggplot2::element_blank())
  }
  
  ret <- list(final=res.final,plot=g1)
  return(ret)
}
