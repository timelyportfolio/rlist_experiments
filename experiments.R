#devtools::install_github("pipeR","renkun-ken")
#devtools::install_github("rlist","renkun-ken")

require(rlist)
require(pipeR)
require(jsonlite)

#from rlist documentation
x <- list(p1 = list(type="A",score=list(c1=10,c2=8)),
          p2 = list(type="B",score=list(c1=9,c2=9)),
          p3 = list(type="B",score=list(c1=9,c2=7)))
list.filter(x,type=="B")


#try my own sample
dt <- #dataframe(
  fromJSON(
    "https://gist.githubusercontent.com/timelyportfolio/3227939/raw/78dbd11f68ad49c917d3ea9d9374da08c3f21fe4/unemployment.json"
  )
#)

dt <- list.load(
  "https://gist.githubusercontent.com/timelyportfolio/3227939/raw/78dbd11f68ad49c917d3ea9d9374da08c3f21fe4/unemployment.json"
)
# I would think this would work but it doesn't
list.filter(dt, year == 2000)

# instead need an idea or container for each, so something like this
dt.nested <- dt %>>%
  {
    lapply(1:length(.[[1]]),FUN=function(row){
      l <- names(dt) %>>%
        lapply(FUN=function(col){
          dt[[col]][[row]]
        }) 
      names(l) <- names(dt)
      return(l)
    })
  }

# or using rlist functions instead
dt.nested <- dt[[1]] %>>%
  list.map(
    f(item,index) -> list.map(
      dt
      , .[index]
    )
  )
  

list.filter(dt.nested, year == 2000)
list.filter(dt.nested, rate > 4)
list.filter(dt.nested, rate > 4) %>>%
  list.group(year)
list.filter(dt.nested, rate > 4) %>>%
  list.select("date"=as.Date(paste0(year,"-",month,"-01")),rate)









devtools::install_github("renkun-ken/rlist@0.3")
devtools::install_github("renkun-ken/pipeR@0.3")
library(rlist)
library(pipeR)
library(rCharts)

x <- list(
  p1 = list(name=c("Ken", "Ren"),age=24),
  p2 = list(name=c("Kent", "Potter"),age=26),
  p3 = list(name=c("Sam", "Lee"),age=24),
  p4 = list(name=c("Keynes", "Bond"),age=30),
  p5 = list(name=c("Kwen", "Hu"),age=31))

list.search(x, any(like("Ken",1)), "character")
list.search(x, all(!like("Ken",2)), "character")
list.search(x, all(like(c("Ken","Hu"),2)), "character")

library(pipeR)
x %>>%
  list.filter(any(like("Ken",1,name))) %>>%
  list.mapv(paste(name,collapse = " "))

# do an example with an rCharts object treated as a list
mtcars %>>%
  ( data.frame( "car" = rownames(.), ., stringsAsFactors = F ) )  %>>%
{
  dPlot(
    data = .
    , x = "cyl"
    , y = "mpg"
    , groups = c("cyl","mpg","car")
    , type = "bubble"
  )
} %>>%  #probably an unlikely use case for rlist but do it anyways
{ 
  list.filter (
    .$params$data %>>% list.parse()
    , any( grepl( pattern = "^C", x = . ) )
  )
} # same as { list.match(.$params$data %>>% list.parse(), "^C") }







library(dplyr)
library(hflights)
library(microbenchmark)
library(ggplot2)

hflights <- as.data.frame(hflights)

hflights_df <- tbl_df(hflights)

hflights_list <- list.parse( hflights )

mb <- microbenchmark(
  dplyr_test = filter(hflights_df, Month == 1, DayofMonth == 1)
  ,df_test = hflights[which(hflights$Month == 1 & hflights$DayofMonth == 1),]
  ,list_vfilter = hflights_list[ which(hflights$Month == 1 & hflights$DayofMonth == 1) ]
  ,rlist_test = list.filter(hflights_list, Month == 1 && DayofMonth == 1 )
  ,List_test = List( hflights_list )$filter(Month == 1 && DayofMonth == 1 )
  ,lapply_test = list.clean(
    lapply(hflights_list,function(f){
      ifelse(f$Month==1 && f$DayofMonth== 1, return(f), return(NULL))
    })
  )
  ,filter_test = Filter(function(f){return( f$Month==1 && f$DayofMonth== 1) },hflights_list)
  ,times = 10L
)

autoplot(mb)mb






# try some rlist on igraphdata  which would be more typical of data
# on which rlist works wonders
require(igraph)
require(igraphdata)
# data(package="igraphdata")
# choose foodwebs for no real reason
data(foodwebs)

# ugly example
# nest it the way necessary for rlist to read
fw <- foodwebs$ChesLower[[9]][[3]][[1]] %>>%
  list.map(
    f(item,index) -> list.map(
      foodwebs$ChesLower[[9]][[3]]
      , .[index]
    )
  )

foodwebs$ChesLower[[9]] %>>%
  list.search( "^C", unlist=T, classes="ANY" )

# an ugly join which should approximate
# get.edgelist(foodwebs$ChesLower)
foodwebs$ChesLower %>>%
  get.adjlist %>>%
  {
    a <- .
    list.map(
      .,
      sapply(.,function(x){return(names(a)[x])})
    )
  }

fw_l <- foodwebs %>>% unclass

fw_l %>>% 
  list.search(
    f(a)->{ grepl( x=a, pattern = "^H" )  }
    ,unlist=T
  )

# which is not the same as
fw_l %>>%
  rapply(
    function(a){
      Filter(
        function(b){
          grepl(
            x = b
            ,pattern="^H"
          )
        }
        ,a
      )
    }
    ,how="list"
  )

# now with commit 
fw_l %>>% list.search(.[equal("^H",pattern=TRUE)], "character")




#### play with xml/xpath/css compared to rlist
#list to XML
#only found this stackoverflow
#http://stackoverflow.com/questions/6256064/how-to-create-xml-from-r-objects-e-g-is-there-a-listtoxml-function
library(XML)
#hadley has rvest that uses simon potter's selectr
#devtools::install_github("hadley/rvest")
library(rvest)

listToXML <- function(node, sublist){
  for(i in 1:length(sublist)){
    child <- newXMLNode(
      ifelse(
        is.null(names(sublist)[i]) || names(sublist)[i] == ""
        ,paste0("unnamed",i)
        #css does not like leading number
        ,ifelse(
          !(is.na(as.numeric(names(sublist)[i])))
          ,paste0("c",names(sublist)[i])
          ,names(sublist)[i]
        )
      )
      , parent=node
      , attrs= list(class = class(sublist[[i]]) )
    )
    
    if (typeof(sublist[[i]]) == "list"){
      listToXML(child, sublist[[i]])
    }
    
    else{
      sapply(
        sublist[[i]],
        function(d){
          child2 <- newXMLNode( class(d), parent = child )
          xmlValue(child2) <- d
        }
      )
    }
  } 
}

#use the nested y list from test-list.search.R
y <- list(
  n = 1:10
  , list(
    df = data.frame(
      id = 1:10
      , letter = letters[1:10]
      , stringsAsFactors = F
    ) %>>% list.parse
  )
  , list( "aa", "bb" )
)
root <- newXMLNode("list")
listToXML(root, y)
doc <- xmlDoc( root )

querySelectorAll(doc,"character")
xmlToList(root)%>>%list.search(.[equal("^a",pattern=T)])


root <- newXMLNode("list_top")
listToXML(root, fw_l[[9]])
doc <- xmlDoc( root )


#more with xml
doc <- xmlParse(system.file("exampleData", "mtcars.xml", package="XML"))

r <- xmlRoot(doc)

xmlSApply(r, xmlValue) %>>% data.frame

xmlApply(r, xmlAttrs)

xmlSApply(r, xmlSize)

doc %>>% querySelectorAll("record") %>>%
  xmlApply(xmlValue) %>>%
  list.map(strsplit(.,"\\s{1,}") %>>% unlist %>>% t %>>% data.frame) %>>%
  list.stack





#play with d3Network
#devtools::install_github('christophergandrud/d3Network')
#see http://edbaskerville.com/research/serengeti-food-web/#
library(d3Network)
#unfortunately doesn't work because d3network does not transform data
d3Tree(fw_l, file = "d3tree_foodweb.html")


foodwebs$ChesLower %>>%
  get.edgelist %>>%
  {structure( data.frame(.), names  = c("Target","Source") )} %>>%
  d3SimpleNetwork( file = "simplenetwork.html" )