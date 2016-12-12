# install.packages('rsconnect')
library(rsconnect)

rsconnect::setAccountInfo(name='cimentadaj',
                          token='B599AA7AE9508743A906E74E7D03893F',
                          secret='VIP6Yxhbczog4B53jkQve2hpTqsTHFFkPDqa/LP/')

rsconnect::deployApp('/Users/cimentadaj/Downloads/inequality/shiny')
