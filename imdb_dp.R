library(ggplot2)
library(GGally)
library(assertr)
library(splitstackshape)
library(lubridate)

#setwd("C://Users//rpothams//Downloads//BD//DVA//p1")
setwd('/Volumes/Transcend/Dropbox/Rajesh_DB/IMP_Dropbox/OMSCS/DVA/Project1')
load('movies_merged')
# https://github.com/Ebeid/CSE6242/blob/master/pr1.pdf
mm_sample <- movies_merged[1:10,]

columns()
colnames(movies_merged)
unique(movies_merged$Type)
movie_db <- movies_merged[which(movies_merged$Type == 'movie'),]
#movie_db1 = movie_db[!is.na(movie_db$Gross),]
# 'Runtime' - function for reading 4 types 1 h, 1 h 1 min, 90 min, 'N/A'
#language filter ??
#feature selection
# 1. budget log(budget) Vs log(revenue)
# 2. Genre
# 3. sum of all award winners(actors/directors e.t.c) in a movie
# 4. movie is a sequel or prequel
# 5. 
# 6. Runtime

#is.numeric(movie_db$Budget) -- TRUE
suppressWarnings(print(ggplot(movie_db, aes(y = Gross, x = Budget)) + geom_point() + 
  scale_x_continuous(trans = 'log10') +
  scale_y_continuous(trans = 'log10') + geom_abline(color = "red")))
cor(movie_db$Budget, movie_db$Gross) # 0.74 correlation

############################ production house ################
ggplot(movie_db, aes(y = Gross, x = Production)) + geom_boxplot()
length(unique(movie_db$Production))
prod_db <- movie_db[,c('Production', 'Budget')]
prod_db <- prod_db[complete.cases(prod_db),]
prod_cluster <- kmeans(movie_db[,c('Production', 'Budget')], 10, nstart = 20)
############################################################################
############# Runtime parsing, converts all formats to minutes####
parse_runtime <- function(runtime){
  runtime <- strsplit(runtime, ' ')[[1]]
  if((length(runtime) == 2) && (runtime[2] == 'h')){
    return(as.integer(runtime[1])*60)
  }else if((length(runtime) == 2) && (runtime[2] == 'min')){
    return(as.integer(runtime[1]))
  }else if(length(runtime) == 4){
    hr_to_min = as.integer(runtime[1])*60
    return(hr_to_min + as.integer(runtime[3]))
  }else return(NA)
}
movie_db$Runtime <- sapply(movie_db$Runtime, parse_runtime)

suppressWarnings(print(ggpairs(movie_db, aes(y = log10(Gross), x = Runtime)) + geom_density() + 
  geom_abline(color = "red") + xlim(80, 120)))

unique(movie_db$Year)
#############################################################################
floor_year <- function(year){
  year <- as.integer(year)
  decade <- year - (year %% 10)
  return(decade)
}
movie_db$decade <- sapply(movie_db$Year, floor_year)
### plots for Runtime Vs Decade
suppressWarnings(print(ggplot(data = movie_db, aes(Runtime, fill = as.character(decade))) + 
  geom_density() +
  facet_wrap(~decade, scales = 'free') +
  ggtitle("Runtime Dist. in each decade") + 
  theme_bw()))
suppressWarnings(print(ggplot(movie_db, aes(x = as.character(decade), Runtime, fill = as.character(decade))) + 
  geom_boxplot(alpha=0.3, notch=TRUE, notchwidth = 0.8, outlier.colour="red", outlier.fill="red", outlier.size=1) +
  theme(legend.position="right") +
  ggtitle("Runtime Dist. in each decade") +
  xlab("Decade") + coord_flip()))

########## Question - 3 ##################
############# all movie Genre's
genre_vector <- c()
for(i in 1:nrow(movie_db)){
  genres <- strsplit(movie_db$Genre[i], ",")[[1]]
  genre_vector <- append(genre_vector, genres)
}
genre_vector <- unique(genre_vector) #for a list of unique Genres


genre_df <- movie_db[,c('Title', 'Genre', 'Gross', 'Runtime')]
genre_df <- cSplit_e(genre_df, "Genre", ",", type = "character", fill = 0) #split Genre column in to multiple cols of binary vals
#genre_df <- col_concat(genre_df[,3:ncol(x)], sep = "") #concatenates columns
#movie_db <- movie_db[ , !(names(movie_db) %in% "Genre")]
#movie_db$Genre <- genre_df

########## 10 most common genres
most_cmon_10 <-  as.data.frame(sort(colSums(genre_df[,5:ncol(genre_df)]), decreasing = TRUE)[1:10], row.names = NULL)
most_cmon_10$Genre <- rownames(most_cmon_10)
colnames(most_cmon_10) <- c("Count", "Genre")
most_cmon_10$Genre <- gsub("Genre_", "", most_cmon_10$Genre)
colnames(genre_df) <- gsub("Genre_", "", colnames(genre_df))

# Select movies from top 10 most common genres and plot their relative proportions
most_cmon_10$Proportion <- most_cmon_10$Count/sum(most_cmon_10$Count)
suppressWarnings(print(ggplot(most_cmon_10, aes(x = Genre, y = Proportion)) + geom_bar(stat = "identity") + 
                         coord_flip()+
                    ggtitle("Top 10 Movie Genre's Proportion")))

# Plot Runtime distribution for top 10 most common genres
top_genre_runtime = melt(genre_df, id.vars = c("Title","Runtime"), measure.vars = most_cmon_10$Genre)
top_genre_runtime = top_genre_runtime[!is.na(top_genre_runtime$Runtime),]
top_genre_runtime = top_genre_runtime[top_genre_runtime$value!=0,]

#ggplot(top_genre_runtime, aes(variable, y = Runtime)) + geom_boxplot() + 
#  ggtitle("Top 10 Movie Genre's Runtime Dist.")

ylim1 = boxplot.stats(top_genre_runtime$Runtime)$stats[c(1, 5)] # compute lower and upper whiskers

suppressWarnings(print(ggplot(top_genre_runtime, aes(reorder(variable, Runtime, median), Runtime, fill = variable)) + 
  geom_boxplot(alpha=0.3, notch=TRUE, notchwidth = 0.8, outlier.colour="red", outlier.fill="red", outlier.size=0.5) +
  theme(legend.position="right") +
  ggtitle("Top 10 Movie Genre's Runtime Dist.") +
  xlab("Genre") + 
    ylim(ylim1*1.9) +
  coord_flip()))

#########  Question - 4 ##########

temp <- movie_db[,c('Title', 'Year', 'Released', 'Date')]
temp <- movie_db[!is.na(movie_db$Gross),] #4558
total_rows <- nrow(movie_db)
print("Percent of rows remaining after removing NA from 'Year'")
((total_rows - nrow(movie_db[is.na(movie_db$Year),]))/total_rows) * 100
#100
print("Percent of rows remaining after removing NA from 'Released'")
((total_rows - nrow(movie_db[is.na(movie_db$Released),]))/total_rows) * 100
#87.6275
print("Percent of rows remaining after removing NA from 'Released' with value in 'Gross'")
((nrow(temp) - nrow(temp[is.na(temp$Released),]))/nrow(temp)) * 100
# 99.0
print("Percent of rows remaining after removing NA from 'Date'")
((total_rows - nrow(movie_db[is.na(movie_db$Date),]))/total_rows) * 100
#11.395
print("Percent of rows remaining after removing NA from 'Date' with value in 'Gross'")
((nrow(temp) - nrow(temp[is.na(temp$Date),]))/nrow(temp)) * 100
#100 -> Date ==Gross== NA is Zero count
# removing all NA Released implies loss of more than 10% data

temp_df <- movie_db
temp_df$Released_year <- year(ymd(temp_df$Released))

temp_df$Released_year <- ifelse(is.na(temp_df$Released_year), temp_df$Year, temp_df$Released_year)
# | (is.na(temp_df$Released_year) & !is.na(temp_df$Year))
sub_temp_df <- temp_df[(temp_df$Year != temp_df$Released_year)  &
                         (temp_df$Date != temp_df$Released_year) &
                         (temp_df$Date != temp_df$Year),]

temp <- sub_temp_df[,c('Title', 'Year', 'Released_year', 'Date')]
print("Number of rows removed with value in 'Gross is 48, i.e less than 5%")
nrow(sub_temp_df[!is.na(sub_temp_df$Gross),]) #815 to 770 to 459

sd <- sub_temp_df[!is.na(sub_temp_df$Title),]
#Removing 5005 observations of which 48 rows have 'Gross' value i.e 

######### Question - 5 ###########

suppressWarnings(print(ggplot(data = movie_db, aes(log10(Gross) , fill = budget_quant)) + 
  geom_density() +
  facet_wrap(~budget_quant, scales = 'free') +
  ggtitle("Runtime Dist. in each Budget category") + 
  theme_bw()))

suppressWarnings(print(ggplot(data = movie_db, aes(log10(Budget), log10(Gross) , fill = budget_quant)) + 
  geom_boxplot() +
  #facet_wrap(~budget_quant, scales = 'free') +
  ggtitle("Runtime Dist. in each Budget category") + 
  theme_bw()))

# Plot Runtime distribution for top 10 most common genres
top_gross_runtime = melt(genre_df, id.vars = c("Title","Gross"), measure.vars = most_cmon_10$Genre)
top_gross_runtime = top_gross_runtime[!is.na(top_gross_runtime$Gross),]
top_gross_runtime = top_gross_runtime[top_gross_runtime$value!=0,]

#ggplot(top_genre_runtime, aes(variable, y = Runtime)) + geom_boxplot() + 
#  ggtitle("Top 10 Movie Genre's Runtime Dist.")

ylim1 = boxplot.stats(top_genre_runtime$Gross)$stats[c(1, 5)] # compute lower and upper whiskers

suppressWarnings(print(ggplot(top_gross_runtime, aes(reorder(variable, Gross, median), Gross, fill = variable)) + 
  geom_boxplot(alpha=0.3, notch=TRUE, notchwidth = 0.8, outlier.colour="red", outlier.fill="red", outlier.size=0.5) +
  theme(legend.position="right") +
  ggtitle("Top 10 Movie Genre's Gross Dist.") +
  xlab("Genre") + 
  coord_flip() +
  coord_cartesian(ylim = ylim1*1.0)))

####### parse Runtime to categories ####
runtime_quantiles <- quantile(movie_db$Runtime, probs = seq(0, 1, 0.33), na.rm = TRUE)

# 0% 33% 66% 99% 
# 1  81  96 172 

runtimeQuantiles <- function(runtime) {
  cut(runtime, breaks = runtime_quantiles, labels = c( "Short", "Medium", "Long"))
}
movie_db$runtime_quant <- sapply(movie_db$Runtime, runtimeQuantiles)
### plots for Runtime Vs Budget Category
suppressWarnings(print(ggplot(data = movie_db, aes(log10(Gross), fill = runtime_quant)) + 
  geom_density() +
  facet_wrap(~runtime_quant, scales = 'free') +
  ggtitle("Gross Dist. in each Runtime category") + 
  theme_bw()))
# short to long; Gross has less tail distribution i.e most long movies have higher Gross
suppressWarnings(print(ggplot(data = movie_db, aes(runtime_quant, log10(Gross), fill = runtime_quant)) + 
  geom_boxplot() +
  #facet_wrap(~runtime_quant, scales = 'free') +
  ggtitle("Gross Dist. in each Runtime category") + 
  theme_bw()))
# Gross median is increasing from Short to Long movies.

### Gross Revenue w.r.t release month
month_df <- movie_db[,c('Released', 'Gross')]
month_df$month <- month(ymd(month_df$Released))
#month_df <- month_df[!is.na(month_df$month),]

suppressWarnings(print(ggplot(data = movie_db, aes(reorder(as.factor(month_df$month), Gross, median), log10(Gross), fill = month_df$month)) + 
  geom_boxplot() +
  ggtitle("Gross Dist. Vs Month") + 
  xlab("Month") +
  theme_bw()))

########## Question - 6 #############
awards_df <- movie_db
awards_df <- awards_df[which(awards_df$Awards != 'N/A'), ]
awards_df <- df[sample(nrow(awards_df), 5000), ] #sampling 5000 to reduce runtime for submission
parse_awards <- function(movie_db){
  movie_db$numeric_awards <- sapply(movie_db$Awards, 
                                 function(awards) as.numeric( unlist(
                                   regmatches(awards, gregexpr("+[0-9]+", awards))[[1]]
                                 ) ) )
  movie_db$Wins = 0
  movie_db$Nominations = 0
  Count_InvalidAwards <- 0 #
  cv <- nrow(movie_db)
  for(i in 1:cv){
    print(i)
    if( (length(movie_db[i,]$numeric_awards[[1]])!=3) & (length(movie_db[i,]$numeric_awards[[1]])!=2) & (length(movie_db[i,]$numeric_awards[[1]])!=0)){
      if(length(grep("win",movie_db[i,]$Awards))>0){
        movie_db[i,]$Wins = as.numeric(movie_db[i,]$numeric_awards[[1]][1])          
      }
      if(length(grep("nomination",movie_db[i,]$Awards))>0){
        movie_db[i,]$Nominations = as.numeric(movie_db[i,]$numeric_awards[[1]][1])   
      }
    }
    if(length(movie_db[i,]$numeric_awards[[1]])==2){
      if((length(grep("win",movie_db[i,]$Awards))>0) || (length(grep("won",movie_db[i,]$Awards))>0)|| (length(grep("Won",movie_db[i,]$Awards))>0)){
        movie_db[i,]$Wins = as.numeric(movie_db[i,]$numeric_awards[[1]][1])          
      }
      if(length(grep("nomination",movie_db[i,]$Awards))>0){
        movie_db[i,]$Nominations = as.numeric(movie_db[i,]$numeric_awards[[1]][2])   
      }
    }
    if(length(movie_db[i,]$numeric_awards[[1]])==3){
      if((length(grep("win",movie_db[i,]$Awards))>0) || (length(grep("won",movie_db[i,]$Awards))>0)|| (length(grep("Won",movie_db[i,]$Awards))>0)){
        movie_db[i,]$Wins = as.numeric(movie_db[i,]$numeric_awards[[1]][2])          
      }
      if(length(grep("nomination",movie_db[i,]$Awards))>0){
        movie_db[i,]$Nominations = as.numeric(movie_db[i,]$numeric_awards[[1]][3])   
      }
    }
    if(length(movie_db[i,]$numeric_awards[[1]])==0){
      movie_db[i,]$Wins = 0
      movie_db[i,]$Nominations = 0
      Count_InvalidAwards = Count_InvalidAwards + 1
    }
    
    if(length(unlist(movie_db[i,]$Wins))==1)
      movie_db[i,]$Wins = as.numeric(unlist(movie_db[i,]$Wins))
    else
      print(paste(movie_db[i,]$Awards, movie_db[i,]$Wins, sep = " ==> " ))
    if(length(unlist(movie_db[i,]$Nominations))==1)
      movie_db[i,]$Nominations = as.numeric(unlist(movie_db[i,]$Nominations)[[1]])
    else
      print(paste(movie_db[i,]$Nominations, movie_db[i,]$Nominations, sep = " ==> " ))
  }
  print(Count_InvalidAwards)
  return(movie_db)
}

regex_awards <- function(awards) {
  temp_x <- regmatches(awards, gregexpr("+[0-9]+", awards))[[1]]
  temp_x <- as.numeric(unlist(temp_x))
} 
#awards_df$award_count <- sapply(awards_df$Awards, regex_awards)

awards_df <- parse_awards(awards_df)

awards_df$total_awards <- awards_df$Wins + awards_df$Nominations
awards_quantiles <- quantile(awards_df$total_awards, probs = seq(0.5, 1, 0.20), na.rm = TRUE)

awardQuantiles <- function(runtime) {
  cut(runtime, breaks = awards_quantiles, labels = c( "few awards", "more", "alot"))
}
awards_df$award_quant <- sapply(awards_df$Runtime, awardQuantiles)
### plots for Runtime Vs Budget Category
suppressWarnings(print(ggplot(data = awards_df, aes(log10(Gross), fill = award_quant)) + 
                         geom_density() +
                         facet_wrap(~runtime_quant, scales = 'free') +
                         ggtitle("Gross Dist. in each No. of Awards Category") + 
                         theme_bw()))

suppressWarnings(print(ggplot(awards_df, aes(x = Gross, y = Wins)) + geom_point()))

suppressWarnings(print(ggplot(awards_df, aes(y = Gross, x = Nominations)) + geom_point()))

########### Question - 7 ############
my_bin <- function(data, mapping, ..., low = "#132B43", high = "#56B1F7") {
  ggplot(data = data, mapping = mapping) +
    geom_bin2d(...) +
    scale_fill_gradient(low = low, high = high)
}

# including "tomatoUserReviews" increased the Runtime
pm <- ggpairs(
  movie_db, columns = c("tomatoMeter","tomatoRating", "tomatoUserRating", "imdbVotes","imdbRating"),
  mapping=aes(color = "grey"),
  lower = list(
    combo = wrap("facethist", binwidth = 1),
    continuous = wrap(my_bin, binwidth = c(5, 0.5), high = "red")
  )
)
suppressWarnings(print(pm + theme_bw()))

########### Question - 8 ############

pm <- ggpairs(
  awards_df, columns = c("total_awards","tomatoRating","imdbRating", "imdbVotes", "tomatoUserReviews"),
  mapping=aes(color = "grey"),
  lower = list(
    combo = wrap("facethist", binwidth = 1),
    continuous = wrap(my_bin, binwidth = c(5, 0.5), high = "red")
  )
)
suppressWarnings(print(pm + theme_bw()))

########### Question - 9 #############
#### Expected Insights
# Insight - 1; Runtime Dist. Vs budget classification
####### parse budget to categories ####
budget_quantiles <- quantile(movie_db$Budget, probs = seq(0, 1, 0.20), na.rm = TRUE)
budgetQuantiles <- function(budget) {
  cut(budget, breaks = budget_quantiles, labels = c("Ultra-Low-Budget", "Low-Budget", "Medium-Budget", "Large-Budget", "Ultra-Large-Budget"))
}
movie_db$budget_quant <- sapply(movie_db$Budget, budgetQuantiles)
### plots for Runtime Vs Budget Category
suppressWarnings(print(ggplot(data = movie_db, aes(Runtime, fill = budget_quant)) + 
  geom_density() +
  facet_wrap(~budget_quant, scales = 'free') +
  ggtitle("Runtime Dist. in each Budget category") + 
  theme_bw()))

suppressWarnings(print(ggplot(data = movie_db, aes(Runtime, log10(Gross), fill = budget_quant)) + 
  geom_boxplot() +
  facet_wrap(~budget_quant, scales = 'free') +
  ggtitle("Gross Dist. in each Runtime Duration Category") + 
  theme_bw()))

# Insight - 2; Top 10 Genres with Gross
genre_df <- movie_db[,c('Title', 'Genre', 'Budget', 'Runtime')]
genre_df <- cSplit_e(genre_df, "Genre", ",", type = "character", fill = 0) #split Genre column in to multiple cols of binary vals
########## 10 most common genres
most_cmon_10 <-  as.data.frame(sort(colSums(genre_df[,5:ncol(genre_df)]), decreasing = TRUE)[1:10], row.names = NULL)
most_cmon_10$Genre <- rownames(most_cmon_10)
colnames(most_cmon_10) <- c("Count", "Genre")
most_cmon_10$Genre <- gsub("Genre_", "", most_cmon_10$Genre)
colnames(genre_df) <- gsub("Genre_", "", colnames(genre_df))

# Plot Runtime distribution for top 10 most common genres
top_genre_runtime = melt(genre_df, id.vars = c("Title","Budget"), measure.vars = most_cmon_10$Genre)
top_genre_runtime = top_genre_runtime[!is.na(top_genre_runtime$Budget),]
top_genre_runtime = top_genre_runtime[top_genre_runtime$value!=0,]

ggplot(top_genre_runtime, aes(reorder(variable, Budget, median), Budget, fill = variable)) + 
  geom_boxplot(alpha=0.3, notch=TRUE, notchwidth = 0.8, outlier.colour="red", outlier.fill="red", outlier.size=0.5) +
  theme(legend.position="right") +
  ggtitle("Top 10 Movie Genre's Budget Dist.") +
  xlab("Genre") + 
  coord_flip()

### Insight - 3
suppressWarnings(print(ggplot(movie_db, aes(y = Gross, x = Domestic_Gross)) + geom_point() + 
                         scale_x_continuous(trans = 'log10') +
                         scale_y_continuous(trans = 'log10') + geom_abline(color = "red")))

##### Unexpected Insights ##############
language_df <- movie_db[,c('Title', 'Language', "tomatoRating","imdbRating")]
language_df <- cSplit_e(language_df, "Language", ",", type = "character", fill = 0) #split Genre column in to multiple cols of binary vals
language_df$lang_count <- rowSums(language_df[,5:ncol(language_df)])
language_df <- language_df[,c('Title', 'Language', "tomatoRating","imdbRating", "lang_count")]

suppressWarnings(print(ggplot(language_df, aes(reorder(lang_count, lang_count, median), tomatoRating, fill = lang_count)) + 
  geom_boxplot(alpha=0.3, notch=FALSE, notchwidth = 0.8, outlier.colour="red", outlier.fill="red", outlier.size=0.5) +
  theme(legend.position="right") +
  ggtitle("No. of languages Vs tomatoRating") +
  xlab("No. of Languages")))

suppressWarnings(print(ggplot(language_df, aes(reorder(lang_count, lang_count, median), imdbRating, fill = lang_count)) + 
                         geom_boxplot(alpha=0.3, notch=FALSE, notchwidth = 0.8, outlier.colour="red", outlier.fill="red", outlier.size=0.5) +
                         theme(legend.position="right") +
                         ggtitle("No. of languages Vs imdbRating") +
                         xlab("No. of Languages")))

suppressWarnings(print(ggplot(movie_db, aes(x = imdbVotes, y = imdbRating)) + geom_bar(stat = "identity") + 
                         ggtitle("Top 10 Movie Genre's Proportion")))

## Relation between indbVotes Vs imdbReviews
suppressWarnings(print(ggplot(movie_db, aes(y = log10(imdbVotes), x = imdbRating)) + geom_point() + 
                         geom_abline(color = "red")))
#runtime_quant,
suppressWarnings(print(ggplot(data = movie_db, aes(log10(Budget), fill = runtime_quant)) + 
                         geom_density() +
                         facet_wrap(~runtime_quant, scales = 'free') +
                         ggtitle("Budget Dist. in each Runtime category") + 
                         theme_bw()))



