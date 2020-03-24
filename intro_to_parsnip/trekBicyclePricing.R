# trek bike web scraping with weight

# road bikes


top_url <- "https://www.trekbikes.com/us/en_US/bikes"
pl_url<- "/road-bikes/c/B200/"

# build a product list
fun<-function(x)(paste0(top_url, pl_url,"?q=%3Arelevance&page=",x,"&pageSize=72"))
trekproductlist_urls<- map(0:1, fun)

trekproductlist<-map(trekproductlist_urls, read_html) 

trekproducts<-trekproductlist %>%
  map(~html_nodes(., ".product-tile__title")) %>%
  map(html_text) %>%
  unlist()

pagelist<-productlist %>%
  map(~html_nodes(., ".product-tile__link")) %>%
  map(~html_attr(., "href")) %>%
  map(~paste0(top_url, .)) %>%
  unlist()




trekpricelist<-trekproductlist %>%
  map(~html_nodes(., "data-price")) %>%
  map(html_text) %>%
  map(~str_sub(., start = 2L)) %>%
  map(~parse_number(.)) %>%
  unlist()
