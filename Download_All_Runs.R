
## GET PASS FILE
        Directory <- "/Users/winstonsaunders/Documents/Ski_data2/"
        valid_pass<-read.csv(paste0(Directory,"valid_pass.csv"))
        colnames(valid_pass)<-c("count", "pass")

## SEASONS
        Season<-NULL
        
        ## create function to define ski season
        seasonfunction<- function(x) {
                if (x<11) year1 <- paste0("0",as.character(x-1)) else year1<-as.character(x-1)
                if (x<10) year2 <- paste0("0",as.character(x)) else year2<-as.character(x)
                return(paste0(year1, "-", year2))                         
                                          
        }
        
        ## rip them off
        for (i in 1:16) Season[i] <- seasonfunction(i)
        
        

## HIT THE PASSES
    set.seed(8675309)
    ## create pass sample of 121 passes from list of approximately 850
    pass_sample<- sample(valid_pass$count, 121, replace=FALSE)
    
    for(PassIndex in pass_sample){
        ## assign pass
        ## Pass<-"MBA6360970"
        Pass<-as.character(valid_pass$pass[PassIndex])
        ## clear skier record
        skier_day_record<-NULL
        cat("getting data for pass ", Pass, "\n")  
        
        ##loop thru years. Note the number corresponds to last date in season (e.g. j=12 ~ season = "11-12")
        for (j in 13:16) { 
            ## wait time of 1 to 2 secs between queries to not overwhelm server
            Sys.sleep(1.0*runif(1)+1.)
            ##MAKE THE HYPERLINK
            link <- paste0("http://track.mtbachelor.com/tyt.asp?passmediacode=", Pass,"&season=", Season[j], "&currentday=null" )

            cat("season", Season[j], " pass ", Pass, "\r")
    
            ## GET currentday DATA FOR A Specific Season

            thepage = readLines(link)

            ## get just the ski dates
            ski_date_info<-grepl("\t\t\t<td><a href=", thepage)

            ski_date_links <- thepage[ski_date_info]
        

            ## cut up the data to get web page links and dates
            ## creates a data frame with the ski date and a link to the ski days.

            ##cut the appropriate junk off the front
            ski_date_links <- lapply(ski_date_links, function(x) {y<- regexpr("href=", x)
                                         substring(x,y+6,nchar(x)) })
      

            ## Get rid of the first line (which is junk)
            ski_date_links<-ski_date_links[-1]  
        

            ## get the date
            ski_date <- lapply(ski_date_links, function(x) {y<- regexpr("\">", x)
                                                 z<- regexpr("</a", x)
                                                   substring(x,y+2,z-1) })

            #print(as.matrix(ski_date, ncols=1))

            ski_day_data<-as.data.frame(as.matrix(ski_date, ncols=1))


            ## create the link by hacking up the html
            ski_date_links <- lapply(ski_date_links, function(x) {y<-regexpr("tyt", x)
                                                        z<- regexpr("\\\">", x)
                                                      substring(x,y,z-1) })
            ski_data_links <- lapply(ski_date_links, function(x) paste0("http://track.mtbachelor.com/", x))
        
            ## convert to data frame
            ski_data_links <-as.data.frame(as.matrix(ski_data_links, ncols=1))
        
            ## create a column of Pass for later id
            pass_id <- rep(Pass, dim(ski_day_data)[1])
            season <- rep(Season[j], dim(ski_day_data)[1])
        
            ## bind to date
        
            ski_day_data<-cbind(pass_id, season, ski_day_data, ski_data_links)
            ## name columns
            colnames(ski_day_data)<-c("pass_id","season", "date", "link")
        
            skier_day_record<-rbind(skier_day_record, ski_day_data)
        }
        
        ##the data look like this
        #      pass_id season       date                                                                                            link
#         1 MBA6360970  12-13 12/16/2012 http://track.mtbachelor.com/tyt.asp?passmediacode=MBA6360970&season=12-13&currentday=12/16/2012
#         2 MBA6360970  12-13 12/21/2012 http://track.mtbachelor.com/tyt.asp?passmediacode=MBA6360970&season=12-13&currentday=12/21/2012
#         3 MBA6360970  12-13 12/27/2012 http://track.mtbachelor.com/tyt.asp?passmediacode=MBA6360970&season=12-13&currentday=12/27/2012
#         4 MBA6360970  12-13 12/28/2012 http://track.mtbachelor.com/tyt.asp?passmediacode=MBA6360970&season=12-13&currentday=12/28/2012
#         5 MBA6360970  12-13   1/4/2013 http://track.mtbachelor.com/tyt.asp?passmediacode=MBA6360970&season=12-13&currentday=01/04/2013
#         6 MBA6360970  12-13   1/5/2013 http://track.mtbachelor.com/tyt.asp?passmediacode=MBA6360970&season=12-13&currentday=01/05/2013

        ## GET THE SKI RUN DATA FROM INDIVIDUAL SKI DAYS
        number_of_ski_days<-1:dim(skier_day_record)[1]
        
        cat("number of ski days for ", Pass, "is", dim(skier_day_record)[1],  "\n")
        
        every_ski_run<-NULL
        
        ##GET DATA FOR EACH SKI DAY
        for (k in number_of_ski_days){
            
            
            ## wait time of 1 to 2 secs between queries to not overwhelm server
            Sys.sleep(1.0*runif(1)+1.)
            
            ## readlines from server
            day_runs_data<-readLines(as.character(skier_day_record$link[k]))
            
            cat(as.character(skier_day_record$link[k]), "\r")
            
            ## Keep just the lines of interest
                ## filter for tables
                aa<- grepl("\t\t\t<td",day_runs_data)
                ## Get rid of some nuisance lines
                bb<- grepl("\t\t\t\t<td",day_runs_data)
        
                day_runs_data <- day_runs_data[aa&!bb]
                ##get rid of header and summary rows at beginning and the end of the table
                day_runs_data <- day_runs_data[5:(4*floor(length(day_runs_data)/4-1))] 
                
                ##get rid of html tags
                runs_data <- lapply(day_runs_data, function(x) {y<- regexpr(">", x)[1]
                                                        z<- regexpr("</td>", x)[1]
                                                        substring(x,y+1,z-1) })
                
            ##create matrix
            runs_data<-matrix(runs_data, ncol=4, byrow=TRUE, 
                          dimnames=list(1:(length(runs_data)/4),c("time", "chair", "vertical_feet", "vertical_meters")))
            ## convert to data frame
            runs_data<-as.data.frame(runs_data)
            
            ## Add a season column
            runs_data$season<-rep(skier_day_record$season[k], nrow(runs_data))
        
            ## clean up the data frame
        
                ## turn the vertical into numbers
                runs_data$vertical_feet<-as.numeric(gsub(",","", runs_data$vertical_feet))
                runs_data$vertical_meters<-as.numeric(gsub(",","", runs_data$vertical_meters))
        
                ## create a date column in POSIXlt
                runs_data$time<-strptime(runs_data$time, "%m/%d/%Y %I:%M:%S %p")
                ## create individual data columns
                runs_data$year<-runs_data$time$year+1900
                runs_data$month<-runs_data$time$mon+1
                runs_data$day<-runs_data$time$mday
                runs_data$time_of_day<-strftime(runs_data$time, format="%H:%M:%S")
                runs_data$date<-strftime(runs_data$time, format="%Y-%m-%d")
                runs_data$pass_id<-rep(skier_day_record$pass_id[k], dim(runs_data)[1])
        
                
                every_ski_run<-rbind(every_ski_run, runs_data)
            }
        
#        The data will look like this
#                           time       chair vertical_feet vertical_meters season year month day time_of_day       date    pass_id
#         1  2012-12-16 09:08:05 Pine Marten          1367             416  12-13 2012    12  16    09:08:05 2012-12-16 MBA6360970
#         2  2012-12-16 09:28:44 Pine Marten          1367             416  12-13 2012    12  16    09:28:44 2012-12-16 MBA6360970
#         3  2012-12-16 09:54:01    Skyliner          1306             398  12-13 2012    12  16    09:54:01 2012-12-16 MBA6360970
#         4  2012-12-16 10:10:17 Pine Marten          1367             416  12-13 2012    12  16    10:10:17 2012-12-16 MBA6360970       
        ## condition data to get file ready to save
        every_ski_run$time<-as.character(every_ski_run$time)
        every_ski_run$chair<-as.character(every_ski_run$chair)
        ## file directory
        Store_Directory <- "/Users/winstonsaunders/Documents/Ski_Data2/Pass_data/"
        
        write.csv(every_ski_run, paste0(Store_Directory,Pass,".csv"), row.names=F)
        cat(nrow(every_ski_run), " runs for Pass ", Pass, "saved to ",paste0(Store_Directory,Pass,".csv"), "\n")

    }  

print("Program Ended")

