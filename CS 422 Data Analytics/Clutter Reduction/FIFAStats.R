library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(lattice)
library(base)
library(stringr)
library(corrplot)

#this function is used to build exploratory data plots and analysis
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

dataset <- read_csv("Data/fifa19.csv")
#purge all but the first 500 records
dataset <- dataset[1:500, ]
df <- data.frame(dataset)

#removing columns that aren't helpful to our data exploration
df$Photo <- df$Flag <- df$Club.Logo <- df$Jersey.Number <- df$Loaned.From <- df$Joined <- df$Real.Face <- NULL
df$Contract.Valid.Until <- df$Release.Clause <- df$ID <- df$Age <- df$Weak.Foot <- df$StandingTackle <- NULL
df$GKDiving <- df$GKHandling <- df$GKKicking <- df$GKPositioning <- df$GKReflexes <- df$SlidingTackle <- NULL
df$LAM <- df$LB <- df$LCB <- df$LCM <- df$LDM <- df$LF <- df$LM <- df$LS <- df$LW <- df$LWB <- NULL
df$ST <- df$RS <- df$CF <- df$RF <- df$RW <- df$CAM <- df$RAM <- df$CM <- df$RCM <- df$RM <- NULL
df$CDM <- df$RDM <- df$RWB <- df$CB <- df$RCB <- df$RB <- df$Special <- df$Skill.Moves <- df$Potential <- NULL
df$Height <- df$Weight <- df$Vision <- df$Nationality <- df$Club <- NULL

#removing any observations with NA values
df <- df %>% drop_na()

#factoring attributes
df$Position <- factor(df$Position)
df$Body.Type <- factor(df$Body.Type)
df$Work.Rate <- factor(df$Work.Rate)
df$Preferred.Foot <- factor(df$Preferred.Foot)

#Exploring what abilities are preferred in a player
#creating a new dataframe with just what we want on the parallel coordinates plot
positionStats <- df
positionStats$X1 <- positionStats$Name <- positionStats$Preferred.Foot <- NULL
positionStats$Value <- positionStats$Wage <- positionStats$Work.Rate <- positionStats$Body.Type <- NULL
positionStats$International.Reputation <- NULL

#creating an initial parallel coordinate visualization
ggparcoord(positionStats, columns = 2:27, groupColumn = "Position")

#here is where I realized that goalkeepers are an outlier to our data, so I decided to remove them
positionStats <- positionStats[!positionStats$Position == "GK",]

#after analyzing our initial parallel coordinates, we reorder the dimensions
ggparcoord(positionStats, columns = c(2, 5, 3, 4, 6:19, 23, 21, 22, 24, 25, 27,26), groupColumn = "Position")

#creating a correlation matrix
M <- cor(positionStats[,c(1,3:28)])
#creating a correlogram
corrplot(M, method="circle")
#creating a correlogram that shows the significance of different correlations
p.mat <- cor.mtest(positionStats[,c(1,3:28)])
corrplot(M, order="hclust", 
         p.mat = p.mat, sig.level = 0.01)




#Exploring how overall rating affects value and wage
#creating a new dataframe with just what we want for a scatterplot matrix
wageStats <- df
wageStats <- wageStats[, c(3:5, 7, 8, 10)]

#the value and wage columns are strings instead of numerics
#using regex, we can parse the strings and get their number value
wageStats$Value <- as.numeric(str_extract(wageStats$Value, "[0-9]+"))
wageStats$Wage <- as.numeric(str_extract(wageStats$Wage, "[0-9]+"))

#renaming columns
wageStats <- wageStats %>% rename(Value.in.M = Value, Wage.in.K = Wage)

#initial scatterplot matrix
pairs(wageStats)

#reordering dimensions
wageStats <- wageStats[, c(2, 3, 1, 4, 6, 5)]

#revised scatterplot matrix
pairs(wageStats)

#creating a correlation matrix
M2 <- cor(wageStats[,1:4])
#creating a correlogram
corrplot(M2, method="number")

#scatterplot
pf <- plot_ly(state, x = wageStats$Overall, y = wageStats$Value.in.M, z = wageStats$Wage.in.K, color = wageStats$Position) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Overall Rating'),
                      yaxis = list(title = 'Value in Millions of Euros'),
                      zaxis = list(title = 'Wage in Thousands of Euros'),
                      title = 'Player Value and Wage by Overall Rating and Position'))
