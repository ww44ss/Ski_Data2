## PASS HUNTER
## Find Valid Passes by query
## This program does a simple hunt to looks for valid ski pass numbers
## writing those valid pass numbers to a .csv file
## looking over 5000 numbers it found ~ 850 valid passes
##
## FUNCTIONS USED
##
## PASS NUMBER
## function to create seasons tags e.g. 09-10
## the challenge is to add the text 0 in front
    seasonfunction<- function(x) {
        if (x<11) year1 <- paste0("0",as.character(x-1)) else year1<-as.character(x-1)
        if (x<10) year2 <- paste0("0",as.character(x)) else year2<-as.character(x)
        return(paste0(year1, "-", year2))                         
    
    }
    
## SEASONS
    ##create a list of season tags
    Season<-NULL
    ## rip them off
    for (i in 1:15) Season[i] <- seasonfunction(i)

## VARIABLE SET UP 
    valid_pass<-NULL
    valid_pass_count=0
    set.seed(8675309)
    
    ## Test 5000 serial pass numbers
    
    for (jj in 1:5000){
        ## pick a starting point
        passcount = 6360970-200+jj    
        Pass <- paste0("MBA", as.character(passcount))
        ## display the number
        cat("Checking ",Pass, "\n")
        ## wait time of 0.5 to 1.5 secs between queries to not overwhelm server
        Sys.sleep(1.0*runif(1)+.5)

        ## create function to define ski season

        skier_day_record<-NULL

        ## for sake of simplicity hit Season 13-14
        j=14
    
        ## form hyper link
        link <- paste0("http://track.mtbachelor.com/tyt.asp?passmediacode=", Pass,"&season=", Season[j], "&currentday=null" )
    
        ## Read page
        thepage = readLines(link)
    
        ## get just the ski dates
        ski_date_info<-grepl("\t\t\t<td><a href=", thepage)
    
        ski_date_links <- thepage[ski_date_info]
    
        ## if more than just header, store the pass number
        if (length(ski_date_links) > 1){
            ## increment counter and story pass number
            valid_pass_count<-valid_pass_count+1
            valid_pass[valid_pass_count] = Pass
            ## print a screen message
            cat("valid pass found ", valid_pass_count, "  ",Pass, "\n")
        }
    }   

    ## print the result
    print(valid_pass_count)
    print(valid_pass)
    ## write the data to disk
    write.csv(valid_pass, file="valid_pass.csv")
    