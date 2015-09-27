
## GET PASS FILE
        Directory <- "/Users/winstonsaunders/Documents/Ski_data2/"
        valid_pass<-read.csv(paste0(Directory,"valid_pass.csv"))
        colnames(valid_pass)<-c("count", "pass")
        
        ## filter bad passes (manually selected based on download problems)
        
            valid_pass<-valid_pass[valid_pass$pass!="MBA6361570", ]
            valid_pass<-valid_pass[valid_pass$pass!="MBA6365030", ]
            valid_pass<-valid_pass[valid_pass$pass!="MBA6365539", ]
            valid_pass<-valid_pass[valid_pass$pass!="MBAXXX", ]
            valid_pass<-valid_pass[valid_pass$pass!="MBAXXX", ]
        
        
        
        
        
        cat("There are ", nrow(valid_pass), " ski passes \n")

## SEASONS
        Season<-NULL
        
        ## create function to define ski season
        seasonfunction<- function(x) {
                if (x<11) year1 <- paste0("0",as.character(x-1)) else year1<-as.character(x-1)
                if (x<10) year2 <- paste0("0",as.character(x)) else year2<-as.character(x)
                return(paste0(year1, "-", year2))                         
                                          
        }
        
        ## rip them off
        for (i in 1:15) Season[i] <- seasonfunction(i)
    
    ## get data on existing passes from the Pass_data directory    
    existing_data<-list.files(paste0(Directory, "Pass_data/"))
    existing_data<-gsub(".csv", "", existing_data)
    
    
    cat("existing data is for ", length(existing_data), " passes \n")
    
    valid_pass_reduced<-valid_pass[!is.element(valid_pass$pass,existing_data),]

## HIT THE PASSES
    set.seed(8675309)
    ## create pass sample of n_ski_sample passes from list of approximately 850
    
    n_ski_sample <- 10
    pass_sample<- sample(valid_pass_reduced$count, n_ski_sample, replace=FALSE)
    
    print(pass_sample)
    
    for(PassIndex in pass_sample){
        ## assign pass
        Pass<-as.character(valid_pass$pass[PassIndex])
        ## clear skier record
        skier_day_record<-NULL
        cat("getting data for pass ", Pass, "      ", format(Sys.time(),"%a %b %d %X %Y"),"\n")    
        
        ## wait up to one minute for next pass hit
        Sys.sleep(55*runif(1)+5)
        
        for (j in 6:15) { 
            ## wait time of 2 to 12 secs between queries to not overwhelm server
            Sys.sleep(2*runif(1)+0.5)
            ##MAKE THE HYPERLINK
            link <- paste0("http://track.mtbachelor.com/tyt.asp?passmediacode=", Pass,"&season=", Season[j], "&currentday=null" )

            cat("season", Season[j], " pass ", Pass, "\r")
    
            ## GET currentday DATA FOR A Specific Season

            thepage <- readLines(link)

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
        
            ## bind to date
        
            ski_day_data<-cbind(pass_id, ski_day_data, ski_data_links)
            ## name columns
            colnames(ski_day_data)<-c("pass_id","date", "link")
        
            skier_day_record<-rbind(skier_day_record, ski_day_data)
        }

        ## GET THE SKI RUN DATA FROM INDIVIDUAL SKI DAYS
        number_of_ski_days<-1:dim(skier_day_record)[1]
        
        cat("number of ski days for ", Pass, "is", dim(skier_day_record)[1],  "\n")
        
        every_ski_run<-NULL
        
        ##GET DATA FOR EACH SKI DAY
        for (k in number_of_ski_days){
            
            
            ## wait time of 1 to 2 secs between queries to not overwhelm server
            Sys.sleep(2*runif(1)+0.5)
            
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
        
        
        ## condition data to get file ready to save
        every_ski_run$time<-as.character(every_ski_run$time)
        every_ski_run$chair<-as.character(every_ski_run$chair)
        ## file directory
        Store_Directory <- "/Users/winstonsaunders/Documents/Ski_Data2/Pass_data/"
        
        write.csv(every_ski_run, paste0(Store_Directory,Pass,".csv"), row.names=F)
        cat(nrow(every_ski_run), " runs for Pass ", Pass, "saved to ",paste0(Store_Directory,Pass,".csv"), "\n")

    }  

print("Program Ended")

