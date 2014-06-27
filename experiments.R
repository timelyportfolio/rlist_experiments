devtools::install_github("pipeR","renkun-ken")
devtools::install_github("rlist","renkun-ken")

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
dt.nested <- dt %:>%
  lapply(1:length(.[[1]]),FUN=function(row){
    l <- names(dt) %>>%
      lapply(FUN=function(col){
        dt[[col]][[row]]
      }) 
    names(l) <- names(dt)
    return(l)
  })

list.filter(dt.nested, year == 2000)
list.filter(dt.nested, rate > 4)
list.filter(dt.nested, rate > 4) %>>%
  list.group(year)
list.filter(dt.nested, rate > 4) %>>%
  list.select("date"=as.Date(paste0(year,"-",month,"-01")),rate)
