

## PASS NUMBER
        Pass <- "MBA6360970"

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
        
        
        skier_day_record<-NULL
 
for (j in 06:15) { 
        
        ##MAKE THE HYPERLINK
        link <- paste0("http://track.mtbachelor.com/tyt.asp?passmediacode=", Pass,"&season=", Season[j], "&currentday=null" )

        print(Season[j])
        print(link)

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
        ## bind to date
        
        
        ski_day_data<-cbind(ski_day_data, ski_data_links)
        ## name columns
        colnames(ski_day_data)<-c("date", "link")

        skier_day_record<-rbind(skier_day_record, ski_day_data)
}

print(skier_day_record)
        
## GET THE SKI RUN DATA FROM INDIVIDUAL SKI DAYS
        
number_of_ski_days<-1:dim(skier_day_record)[1]
        
every_ski_run<-NULL
        
for (k in number_of_ski_days){
        
        print(k)
        
        day_runs_data<-readLines(as.character(skier_day_record$link[k]))
        
        ## Keep just the lines of interest
        aa<- grepl("\t\t\t<td",day_runs_data)
        ## Get rid of some nuisance lines
        bb<- grepl("\t\t\t\t<td",day_runs_data)
        
        day_runs_data <- day_runs_data[aa&!bb]
        day_runs_data <- day_runs_data[5:(4*floor(length(day_runs_data)/4-1))] 
        
        runs_data <- lapply(day_runs_data, function(x) {y<- regexpr(">", x)[1]
                                                        z<- regexpr("</td>", x)[1]
                                                        substring(x,y+1,z-1) })
        
        runs_data<-matrix(runs_data, ncol=4, byrow=TRUE, 
                          dimnames=list(1:(length(runs_data)/4),c("time", "chair", "vertical_feet", "vertical_meters")))
        
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
        
        every_ski_run<-rbind(every_ski_run, runs_data)
        }
        
        ## convert to factor
        
        print(dim(every_ski_run))
        
        head(every_ski_run)
        
        library(ggplot2)
        
        p<-ggplot(every_ski_run, aes(x=time, y=vertical_feet))+geom_point()
        print(p)
        
        ## Get file ready to save
        every_ski_run$time<-as.character(every_ski_run$time)
        
        Directory <- "/Users/winstonsaunders/Documents/Ski_Data2/"
        
        write.csv(every_ski_run, paste0(Directory,Pass,".csv"), row.names=F)
        
#for (i in 1:length(ski_data_linksZ)) {
#        ski_day_data <- readLines(ski_data_linksZ[i])
#}






## ANTOHER ATTEMPT AT USING XML

# a<- readHTMLTable("http://track.mtbachelor.com/tyt.asp?passmediacode=MBA6360970&season=14-15&currentday=02/14/2015")
# names(a)
# length(a)
# class(a[2])
# 
# b<-as.data.frame(a[2])
# names(b)
# 
# class(b[2])
# attributes(b[2])
# 
# c<-htmlParse(a)
# 
# sapply(b, class)
# length(b)
# 
# 
# ## another method for getting a table
# c<-htmlParse(a)  ## generates warnings
# tableNodes <- getNodeSet(c, "//table")


# ## THIS IS A CODE CHUNK EXPLORING THE {rvest} PACKAGE
# 
#         library(rvest)
# 
#         tyt_data<-html("http://track.mtbachelor.com/tyt.asp?passmediacode=MBA6360970&season=13-14&currentday=null")
# 
#         object tyt_data is an html/xml object
#
#         html_nodes(tyt_data, xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "sample", " " ))]//table[(((count(preceding-sibling::*) + 1) = 2) and parent::*)] | //*[contains(concat( " ", @class, " " ), concat( " ", "sample", " " ))]//table//table//table//b//select') %>% html_attrs()
# 
#         strong_spanX1 <- '//center | //input//select//*[contains(concat( " ", @class, " " ), concat( " ", "sample", " " ))]//table[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "sample", " " ))]//table//table//table//b'
#  
#         strong_span <- '//form | //*[contains(concat( " ", @class, " " ), concat( " ", "sample", " " ))]//table[(((count(preceding-sibling::*) + 1) = 2) and parent::*)]'
# 
#         java_widget <- '//input | //select'
# 
#         html_nodes(tyt_data, xpath= strong_span)%>% html_attrs()
# 
#         html_nodes(tyt_data, xpath= java_widget)%>% html_attrs()