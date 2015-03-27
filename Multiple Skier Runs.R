## EXPLORE FOR PASSES
##
##

## CREATE PASS NUMBERS

Pass_Char<-"MBA"
Pass_Number <- 6360970+1

Pass <- paste0(Pass_Char, as.character(Pass_Number))

print(Pass)

## CREATE SEASON LABELS
        Season<-NULL

        ## create function to define ski season
        seasonfunction<- function(x) {
                if (x<11) year1 <- paste0("0",as.character(x-1)) else year1<-as.character(x-1)
                if (x<10) year2 <- paste0("0",as.character(x)) else year2<-as.character(x)
                return(paste0(year1, "-", year2))                         
        
        }

        ## rip them off
        for (i in 1:15) Season[i] <- seasonfunction(i)

# GET SKIER RECORDS

skier_day_record<-NULL

        ## HIT THE SEASONS
        for (j in 06:15) { 
        
                ##MAKE THE HYPERLINK
                link <- paste0("http://track.mtbachelor.com/tyt.asp?passmediacode=", Pass,"&season=", Season[13], "&currentday=null" )
        
                page_pull = readLines(link)
                
                if (page_pull[145] != "\tNo information available for this season.") {
                        
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
                        
                                aaa<-as.matrix(ski_date, ncols=1)
                                print(aaa[1])
                        
                                ski_day_data<-as.data.frame(as.matrix(ski_date, ncols=1))
                        
                        
                        ## create the link by hacking up the html
                        ski_date_links <- lapply(ski_date_links, function(x) {y<-regexpr("tyt", x)
                                                                              z<- regexpr("\\\">", x)
                                                                              substring(x,y,z-1) })
                        ski_data_links <- lapply(ski_date_links, function(x) paste0("http://track.mtbachelor.com/", x))
                        
                        ## convert to data frame
                        ski_data_links <-as.data.frame(as.matrix(ski_data_links, ncols=1))
                        ## bind to date
                        
                        
                        ski_day_data<-cbind(ski_day_data, ski_data_links)
                        ## name columns
                        colnames(ski_day_data)<-c("date", "link", )
                        
                        skier_day_record<-rbind(skier_day_record, ski_day_data)
                }
