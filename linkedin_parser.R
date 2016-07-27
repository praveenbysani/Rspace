library(rvest)
library(httr)

lnkdn_resp <- GET("https://www.linkedin.com/pub/dir/?first=ben&last=&trk=uno-reg-guest-home-name-search&search=Search",
                  user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36"))

#use css selector in browser to get the corresponding paths
top_10_people <- content(lnkdn_resp) %>% html_nodes("h3") %>% html_text()
top_10_headline <- content(lnkdn_resp) %>% html_nodes(".headline") %>% html_text()
top_10_industry <- content(lnkdn_resp) %>% html_nodes("dd~ dd") %>% html_text()
