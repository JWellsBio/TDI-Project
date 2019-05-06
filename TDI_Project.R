## import facebook data ----
facebook_data <- read.csv('temp_datalab_records_social_facebook.csv', header = TRUE, stringsAsFactors = FALSE)
# clean up
facebook <- facebook_data[, -which(names(facebook_data) %in% c('has_added_app', 'dataset_id', 'facebook_id', 'entity_id', 'cusip', 'isin'))]
facebook$time <- gsub(" .*$", "", facebook$time) #only dates, no time needed
facebook$time <- as.Date(facebook$time, format = '%Y-%m-%d')
facebook$username <- tolower(facebook$username)

## scrape wikipedia for companies with SB commercials----
library(rvest)
library(magrittr)
url <- 'https://en.wikipedia.org/wiki/List_of_Super_Bowl_commercials'
sb2016 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[50]') %>%
  html_table()
sb2016 <- sb2016[[1]]
sb2016 <- sb2016[, c(1,2)]
colnames(sb2016) <- c('industry', 'company')
sb2016$company <- tolower(sb2016$company)
sb2016$company[5] <- 'honda'
sb2016$company[24] <- 'mountaindew'
sb2016$company[c(8,9)] <- 'doritosusa'

sb2017 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[51]') %>%
  html_table()
sb2017 <- sb2017[[1]]
sb2017 <- sb2017[, c(1,2)]
colnames(sb2017) <- c('industry', 'company')
sb2017$company <- tolower(sb2017$company)
sb2017$company[c(5,6,7)] <- 'alfa.romeo.cars'
sb2017$company[12] <- 'mrclean'
sb2017$company[31] <- 'wix'

sb2018 <- url %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[52]') %>%
  html_table()
sb2018 <- sb2018[[1]]
sb2018 <- sb2018[, c(1,2)]
colnames(sb2018) <- c('industry', 'company')
sb2018$company[c(12,13,14)] <- 'jeep'
sb2018$company[c(17,18,19)] <- 'toyotamotorcorporation'
sb2018$company[29] <- 'kraftfoods'
sb2018$company[32] <- 'mountaindew'
sb2018$company[34] <- 'etrade'
sb2018$company[c(43,44)] <- 'thecocacolaco'
sb2018$company[54] <- 'amazon'
sb2018$company[70] <- 'wix'

## subset to companies needed ----
facebook_2016 <- facebook[facebook$username %in% sb2016$company, ]
facebook_2016_colgate       <- facebook_2016[facebook_2016$username == 'colgate', ]
facebook_2016_doritosusa    <- facebook_2016[facebook_2016$username == 'doritosusa', ]
facebook_2016_honda         <- facebook_2016[facebook_2016$username == 'honda', ]
facebook_2016_jeep          <- facebook_2016[facebook_2016$username == 'jeep', ]
facebook_2016_mountaindew   <- facebook_2016[facebook_2016$username == 'mountaindew', ]

facebook_2017 <- facebook[facebook$username %in% sb2017$company, ]
facebook_2017_alfaromeo     <- facebook_2017[facebook_2017$username == 'alfa.romeo.cars', ]
facebook_2017_buick         <- facebook_2017[facebook_2017$username == 'buick', ]
facebook_2017_intel         <- facebook_2017[facebook_2017$username == 'intel', ]
facebook_2017_mrclean       <- facebook_2017[facebook_2017$username == 'mrclean', ]
facebook_2017_sprint        <- facebook_2017[facebook_2017$username == 'sprint', ]
facebook_2017_turbotax      <- facebook_2017[facebook_2017$username == 'turbotax', ]
facebook_2017_wix           <- facebook_2017[facebook_2017$username == 'wix', ]

facebook_2018 <- facebook[facebook$username %in% sb2018$company, ]
facebook_2018_amazon        <- facebook_2018[facebook_2018$username == 'amazon', ]
facebook_2018_etrade <- facebook_2018[facebook_2018$username == 'etrade', ]
facebook_2018_jeep <- facebook_2018[facebook_2018$username == 'jeep', ]
facebook_2018_kraftfoods <- facebook_2018[facebook_2018$username == 'kraftfoods', ]
facebook_2018_mountaindew <- facebook_2018[facebook_2018$username == 'mountaindew', ]
facebook_2018_coke <- facebook_2018[facebook_2018$username == 'thecocacolaco', ]
facebook_2018_toyota <- facebook_2018[facebook_2018$username == 'toyotamotorcorporation', ]
facebook_2018_wix <- facebook_2018[facebook_2018$username == 'wix', ]

## create plots of talked about trends----
library(ggplot2)
#2016 companies
colgate_2016 <- ggplot(data = facebook_2016_colgate, aes(x = facebook_2016_colgate$time, y = facebook_2016_colgate$talking_about_count)) + geom_line(size = 1) + geom_vline(xintercept = as.numeric(as.Date('2016-02-07')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time') + theme_bw() + geom_text(x=as.numeric(as.Date('2017-01-01')), y=75000, label="Colgate\n(2016)")
honda_2016 <- ggplot(data = facebook_2016_honda, aes(x = facebook_2016_honda$time, y = facebook_2016_honda$talking_about_count)) + geom_line(size = 1) + geom_vline(xintercept = as.numeric(as.Date('2016-02-07')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time') + theme_bw() + geom_text(x = as.numeric(as.Date('2017-06-01')), y = 165000, label = 'Honda')


#2017 companies
ggplot(data = facebook_2017_alfaromeo, aes(x = facebook_2017_alfaromeo$time, y = facebook_2017_alfaromeo$talking_about_count)) + geom_line() + geom_vline(xintercept = as.numeric(as.Date('2017-02-05')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time')
ggplot(data = facebook_2017_intel, aes(x = facebook_2017_intel$time, y = facebook_2017_intel$talking_about_count)) + geom_line() + geom_vline(xintercept = as.numeric(as.Date('2017-02-05')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time') + theme_bw()
mr_clean_2017 <- ggplot(data = facebook_2017_mrclean, aes(x = facebook_2017_mrclean$time, y = facebook_2017_mrclean$talking_about_count)) + geom_line(size = 1) + geom_vline(xintercept = as.numeric(as.Date('2017-02-05')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time') + theme_bw() + geom_text(x = as.numeric(as.Date('2017-06-01')), y = 4e5, label = 'Mr. Clean\n(2017)')
sprint_2017 <- ggplot(data = facebook_2017_sprint, aes(x = facebook_2017_sprint$time, y = facebook_2017_sprint$talking_about_count)) + geom_line(size = 1) + geom_vline(xintercept = as.numeric(as.Date('2017-02-05')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time') + theme_bw() + geom_text(x = as.numeric(as.Date('2016-05-01')), y = 78000, label = 'Sprint\n(2017)')
turbotax_2017 <- ggplot(data = facebook_2017_turbotax, aes(x = facebook_2017_turbotax$time, y = facebook_2017_turbotax$talking_about_count)) + geom_line(size = 1) + geom_vline(xintercept = as.numeric(as.Date('2017-02-05')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time') + theme_bw() + geom_text(x = as.numeric(as.Date('2017-07-01')), y = 1e5, label = 'Turbo Tax\n(2017)')

#2018 companies
amazon_2018 <- ggplot(data = facebook_2018_amazon, aes(x = facebook_2018_amazon$time, y = facebook_2018_amazon$talking_about_count)) + geom_line(size = 1) + geom_vline(xintercept = as.numeric(as.Date('2018-02-04')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time') + theme_bw() + geom_text(x = as.numeric(as.Date('2016-01-01')), y = 4e5, label = 'Amazon\n(2018)')
coke_2018 <- ggplot(data = facebook_2018_coke, aes(x = facebook_2018_coke$time, y = facebook_2018_coke$talking_about_count)) + geom_line(size = 1) + geom_vline(xintercept = as.numeric(as.Date('2018-02-04')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time') + theme_bw() + geom_text(x = as.numeric(as.Date('2017-02-01')), y = 80000, label = 'Coke\n(2018)')
etrade_2018 <- ggplot(data = facebook_2018_etrade, aes(x = facebook_2018_etrade$time, y = facebook_2018_etrade$talking_about_count)) + geom_line(size = 1) + geom_vline(xintercept = as.numeric(as.Date('2018-02-04')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time') + theme_bw() + geom_text(x = as.numeric(as.Date('2017-10-15')), y = 4000, label = 'E-trade\n(2018)')
kraft_2018 <- ggplot(data = facebook_2018_kraftfoods, aes(x = facebook_2018_kraftfoods$time, y = facebook_2018_kraftfoods$talking_about_count)) + geom_line(size = 1) + geom_vline(xintercept = as.numeric(as.Date('2018-02-04')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time') + theme_bw() + geom_text(x = as.numeric(as.Date('2017-04-01')), y = 60000, label = 'Kraft Foods\n(2018)')

#multiple-year companies
mtdew_both <- ggplot(data = facebook_2018_mountaindew, aes(x = facebook_2018_mountaindew$time, y = facebook_2018_mountaindew$talking_about_count)) + geom_line(size = 1) + geom_vline(xintercept = as.numeric(as.Date('2018-02-04')), linetype = 3, colour = 'red', size = 1.1) + geom_vline(xintercept = as.numeric(as.Date('2016-02-07')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time') + theme_bw() + geom_text(x = as.numeric(as.Date('2017-08-15')), y = 175000, label = 'Mountain Dew\n(2018)')
jeep_both <- ggplot(data = facebook_2018_jeep, aes(x = facebook_2018_jeep$time, y = facebook_2018_jeep$talking_about_count)) + geom_line() + geom_vline(xintercept = as.numeric(as.Date('2018-02-04')), linetype = 3, colour = 'red', size = 1.1) + geom_vline(xintercept = as.numeric(as.Date('2016-02-07')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time')
wix_both <- ggplot(data = facebook_2017_wix, aes(x = facebook_2017_wix$time, y = facebook_2017_wix$talking_about_count)) + geom_line(size = 1) + geom_vline(xintercept = as.numeric(as.Date('2017-02-05')), linetype = 3, colour = 'red', size = 1.1) + geom_vline(xintercept = as.numeric(as.Date('2018-02-04')), linetype = 3, colour = 'red', size = 1.1) + ylab('Talking About Count') + xlab('Time') + theme_bw() + geom_text(x = as.numeric(as.Date('2016-01-01')), y = 175000, label = 'Wix.com\n(2016 and 2018)')

##plot 6 with some evidence of benefit----
library(ggpubr)
ggarrange(colgate_2016,mr_clean_2017, turbotax_2017, etrade_2018, mtdew_both, wix_both, nrow = 2, ncol = 3)
ggarrange(sprint_2017, amazon_2018, coke_2018, kraft_2018, nrow = 2, ncol = 2)
