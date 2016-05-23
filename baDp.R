#ABD : Pipeline, Approved, Ongoing
library(stringr)
#The comparison of the webcrapping using rvest and readLines
setStatusRedict <- function (strURL, strStatus) {
#Redirect to the good function to retrieve
if (strStatus == "pipeline"){ getProjectIDPipeline (strURL) }
else if (strStatus == "approved"){}
else if (strStatus == "ongoing"){}
else {}
}
#-----------

getProjectName <- function (String,pattern){
    #Pattern
    inHtmlTag <- c(pattern,"divider","class","<p>","</p>","<strong>","</strong>","h1","h2","h4","hr","br","div","[:punct:]",">","<","=")    
    rcString <- String #Assignment
    for (j in 1 : length(inHtmlTag)){        
        rcString <- str_replace_all(rcString,inHtmlTag[j],"") #replace pattern to null      
    }#End for    
    return (rcString)
    
}#End function getProjectName

getKeyContact <- function (String,pattern){
    #Pattern
    inHtmlTag <- c(pattern,"divider","class","<p>","</p>","<strong>","</strong>","h1","h2","h4","hr","br","div",">","<","=")    
    rcString <- String #Assignment
    for (j in 1 : length(inHtmlTag)){        
        rcString <- str_replace_all(rcString,inHtmlTag[j],"") #replace pattern to null        
    }#End for 
    return (rcString)    
}#End function getKeyContact

getOverview <- function (String){
    #Pattern
    inHtmlTag <- c("divider","class","div","<li>","<p>","</p>","</li>","<strong>","</strong>","</span>","hr","span","hide","<ul>","</ul>","<",">","=")
    rcString <- String #Assignment
    for (j in 1 : length(inHtmlTag)){        
        rcString <- str_replace_all(rcString,inHtmlTag[j],"")#replace pattern to null        
    }#End for
    return (rcString)
}#End function getOverview

getAmount <- function (String,pattern){
    #Pattern
    inHtmlTag <- c(pattern,"h1","h2","h4","divider","class","div","<li>","<p>","</p>","</li>","<strong>","</strong>","</span>","hr","span","hide","<ul>","</ul>","<",">","=","[:alpha:]","[:punct:]")
    rcString <- String #Assignment
    for (j in 1 : length(inHtmlTag)){        
        rcString <- str_replace_all(rcString,inHtmlTag[j],"") #replace pattern to null        
    }#End for
    return (rcString)
}#End function getAmount

getPageNumber <- function(String){
    #Pattern
	inString <- c("[:alpha:]","[:punct:]",">","<","=")
	rcString <- String #Assignment
	for (j in 1 : length(inString)){
  	    rcString <- str_replace_all(rcString,inString[j],"") #replace pattern to null
    }#End for
	rcString <- as.integer(str_trim(rcString)) #conversion to integer
	cmpLogic <- as.integer(rcString/20) #Diving by the max value on the screen
	
	#Getting number the sub pages to explore
	if (rcString %% 20 > 0) {  cmpLogic <- cmpLogic + 1
	}else{ cmpLogic <- cmpLogic}#if-condition
	return (cmpLogic)
}#End function getPageNumber


getProjectIDPipeline <- function (strURL){
   
    X <- readLines(strURL)
	K <- X[9]
	val <- unlist(gregexpr(pattern="projectSingle",K))
	V <- length(val)
	p <- 0
	for (i in  1 : V){
            if (val[i] > 1) { p <- val[i]}
    }#End for

st <- substr(K,p,str_length(K))
ptt <- "csc-header"
val <- unlist(gregexpr(pattern=ptt,st))

#val 1 to 2
pname <- getProjectName(substr(st,val[1],val[2]-1),ptt)
pname <- str_trim(pname)

#val 2 to 3
overview <- substr(st,val[2],val[3])
delim <- unlist(gregexpr(pattern="<li>",overview))

ref <- getOverview(substr(overview,delim[1],delim[2]))
apraisalDate <- getOverview(substr(overview,delim[2],delim[3]))
boardPres <- getOverview(substr(overview,delim[3],delim[4]))
status <- getOverview(substr(overview,delim[4],delim[5]))#if status is PIPE 
implementedAgency <- getOverview(substr(overview,delim[5],delim[6]))
location <- getOverview(substr(overview,delim[6],str_length(overview)))

#val 3 to 4
keyContact <- getKeyContact(substr(st,val[3],val[4]-1),ptt)

#val 4 to length
amount <- getAmount(substr(st,val[4],unlist(gregexpr(pattern="TYPO3SEARCH_end",st))),ptt)
amount <- str_trim(amount)

#Print
print(ref)
print(apraisalDate)
print(boardPres)
print(status)
print(implementedAgency)
print(location)
print(keyContact)
print(amount)

}

#--------------------------------


library(rvest)
mURL <- "http://www.afdb.org/en/projects-and-operations/project-portfolio/health/"

#List of project and creation of link + status
getSearchReference(mURL) #call the function for the main page

Q <- read_html(mURL)
Q <- iconv(Q,"UTF-8","latin1")
#Number of sub pages to explore - 1 
nbSubPage <- getPageNumber(substr(Q,unlist(gregexpr(pattern = "out of",Q))[1], unlist(gregexpr(pattern = "out of",Q))[1]+30))

#Browsing all the subpage related to the choice of the user from the interface
for (j in 1: nbSubPage - 1){
   #List of project and creation of link + status
   if (j > 0) {
       smURL <- paste(mURL,j,sep="")
	   print(smURL)
	   getSearchReference (smURL) #Call the function to collect all the project i.e link and status
   }#if-condition   
}#End for



getSearchReference <- function (strURL){

strURL <- strURL
stProject <- read_html(strURL)
#stProject <- iconv(stProject,"UTF-8","latin1")
stProject <-  stProject %>%
                html_nodes("tr td")%>%
			    html_text()
ui <- stProject
ui <- as.data.frame(ui)

nbRow <- nrow(ui)
for (j in 1 : nbRow){

  # ui [str_length(j)> 15, ] <- "NA"
  k <- 0
  if (str_length(ui[j,]) > 15) {
  
        k <- k +1 
        ui [j, ] <- "NA" #replace long string to NA		
		if (k == nbRow/3)break;
  }#if-condition
}#End function

uii <- na.omit(ui) #deleting NA
uii <- as.data.frame(uii)
nbRow <- nrow(uii)

setDefaultLink <- "http://www.afdb.org/en/projects-and-operations/project-portfolio/project/"

for (j in 1 : nbRow){

    if ( j %% 2 == 0 ){
	
		pStatus <- str_trim(tolower(substr(uii[j,],1,str_length(uii[j,])-4)))#Getting the status 
		pLink  <- str_trim(tolower(substr(uii[j-1,],1,str_length(uii[j-1,]))))#Getting the project id
		pLink  <- paste(setDefaultLink,pLink,sep="")#Creating the link
		print(pLink)
		print(pStatus)
		setStatusRedict(pLink,pStatus)		
	}#if-condition

}#End for

return("done")

}#End function



getProjectIDApproved <- function (strURL){
    X <- readLines(strURL)
	K <- X[9]
	val <- unlist(gregexpr(pattern="projectSingle",K))
	V <- length(val)
	p <- 0
	for (i in  1 : V){
            if (val[i] > 1) { p <- val[i]}
    }#End for

st <- substr(K,p,str_length(K))
ptt <- "csc-header"
val <- unlist(gregexpr(pattern=ptt,st))

#val 1 to 2
pname <- getProjectName(substr(st,val[1],val[2]-1),ptt)
pname <- str_trim(pname)

#val 2 to 3
overview <- substr(st,val[2],val[3])
delim <- unlist(gregexpr(pattern="<li>",overview))

ref 			<- getOverview(substr(overview,delim[1],delim[2]))
aprovalDate 	<- getOverview(substr(overview,delim[2],delim[3]))
startDate 		<- getOverview(substr(overview,delim[3],delim[4]))
apraisalDate	<- getOverview(substr(overview,delim[4],delim[5]))#if status is PIPE 
status 			<- getOverview(substr(overview,delim[5],delim[6]))
implementedAgency <- getOverview(substr(overview,delim[6],delim[7]))
location 		  <- getOverview(substr(overview,delim[7],str_length(overview)))

#val 3 to 4
keyContact <- getKeyContact(substr(st,val[3],val[4]-1),ptt)

#val 4 to length
amount <- getAmount(substr(st,val[4],unlist(gregexpr(pattern="TYPO3SEARCH_end",st))),ptt)
amount <- str_trim(amount)

#Print
print(ref)
print(aprovalDate)
print(startDate)
print(apraisalDate)
print(status)
#print(implementedAgency)
#print(location)
#print(keyContact)
#print(amount)
print(substr(st,val[4],unlist(gregexpr(pattern="TYPO3SEARCH_end",st))))#!!!! Here there are cost and key contact
}


getProjectOngoing <- function (strURL){

firstRow <- strURL %>%
            html_nodes("tr th") %>%
			html_text()
			
secondR <-  strURL %>%
            html_nodes("tbody") %>%
			html_text()
			
#Assembling firstRow and secondR
}



##getProjectIDApproved("http://www.afdb.org/en/projects-and-operations/project-portfolio/project/p-z1-aag-004/")