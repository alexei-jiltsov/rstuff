library(XML)
library(RCurl)

url <- http://www.ebay.co.uk/sch/Single-Use-Batteries-/50602/i.html?_pgn=PAGE&_skc=200&rt=nc

for(i in 1:99) {
	url2 <- gsub('PAGE',i,url)
	html <- htmlTreeParse(url)
	
}