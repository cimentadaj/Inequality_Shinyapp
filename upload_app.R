# install.packages('rsconnect')
library(rsconnect)

rsconnect::setAccountInfo(name='cimentadaj',
                          token='B599AA7AE9508743A906E74E7D03893F',
                          secret='VIP6Yxhbczog4B53jkQve2hpTqsTHFFkPDqa/LP/')

rsconnect::deployApp('/Users/cimentadaj/Downloads/inequality/Inequality_Shinyapp/shiny/')

cd = 'cd /Users/cimentadaj/Downloads/inequality/Inequality_Shinyapp/'
add = 'git add .'
commit = 'git commit'
push = 'git push'
