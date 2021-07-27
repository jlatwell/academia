library(readr)
library(dplyr)
library(ggplot2)
library(lattice)
library(tidyr)
library(GGally)
library(plotly)

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

medicare15 <- read_csv("Data/medicare15.csv")
dfm15 <- data.frame(medicare15)

dfm15$Provider.ID <- dfm15$Street.Address <- dfm15$Facility.Name <- dfm15$Zip.Code <- NULL
dfm15$Nondual.Beneficiaries <- dfm15$Dual.Beneficiaries <- NULL

dfm15$City <- factor(dfm15$City)
dfm15$State <- factor(dfm15$State)

dfm15 <- dfm15 %>% drop_na(Male.Beneficiaries, Female.Beneficiaries)

#First question:  does the state affect preexisting conditions?
#creating new dataset for analysis
state <- dfm15
#removing attributes that are not useful for analysis
state$Total.SNF.Charge.Amount <- state$Total.SNF.Medicare.Allowed.Amount <- state$Total.SNF.Medicare.Payment.Amount <- NULL
state$Total.SNF.Medicare.Standard.Payment.Amount <- state$Average.HCC.Score <- state$White.Beneficiaries <- NULL
state$Black.Beneficiaries <- state$American.Indian.or.Alaska.Native.Beneficiaries <- state$Asian.Pacific.Islander.Beneficiaries <- NULL
state$Hispanic.Beneficiaries <- state$Other..Unknown.Beneficiaries <- NULL
state$City <- state$Average.Length.of.Stay..Days. <- state$Total.Stays <- NULL
#state$Male.Beneficiaries<- state$Female.Beneficiaries <- NULL
#removing observations that have NA values
state <- state %>% drop_na()

#converting these values from percentages to 
state$Percent.of.Beneficiaries.with.Atrial.Fibrillation <- state$Percent.of.Beneficiaries.with.Atrial.Fibrillation/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.Alzheimer.s <- state$Percent.of.Beneficiaries.with.Alzheimer.s/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.Asthma <- state$Percent.of.Beneficiaries.with.Asthma/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.Cancer <- state$Percent.of.Beneficiaries.with.Cancer/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.CHF <- state$Percent.of.Beneficiaries.with.CHF/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.Chronic.Kidney.Disease <- state$Percent.of.Beneficiaries.with.Chronic.Kidney.Disease/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.COPD <- state$Percent.of.Beneficiaries.with.COPD/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.Depression <- state$Percent.of.Beneficiaries.with.Depression/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.Diabetes <- state$Percent.of.Beneficiaries.with.Diabetes/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.Hyperlipidemia <- state$Percent.of.Beneficiaries.with.Hyperlipidemia/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.Hypertension <- state$Percent.of.Beneficiaries.with.Hypertension/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.IHD <- state$Percent.of.Beneficiaries.with.IHD/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.Osteoporosis <- state$Percent.of.Beneficiaries.with.Osteoporosis/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.RA.OA <- state$Percent.of.Beneficiaries.with.RA.OA/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.Schizophrenia <- state$Percent.of.Beneficiaries.with.Schizophrenia/100 * state$Distinct.Beneficiaries.Per.Provider
state$Percent.of.Beneficiaries.with.Stroke <- state$Percent.of.Beneficiaries.with.Stroke/100 * state$Distinct.Beneficiaries.Per.Provider

#renaming columns
colnames(state)[2] <- "Beneficiaries"
state <- state %>% rename(Afib = Percent.of.Beneficiaries.with.Atrial.Fibrillation, Alzheimers = Percent.of.Beneficiaries.with.Alzheimer.s,
                          Asthma = Percent.of.Beneficiaries.with.Asthma, Cancer = Percent.of.Beneficiaries.with.Cancer, CHF = Percent.of.Beneficiaries.with.CHF,
                          CKD = Percent.of.Beneficiaries.with.Chronic.Kidney.Disease, COPD = Percent.of.Beneficiaries.with.COPD,
                          Depression = Percent.of.Beneficiaries.with.Depression, Diabetes = Percent.of.Beneficiaries.with.Diabetes, Hyperlipidemia = Percent.of.Beneficiaries.with.Hyperlipidemia,
                          Hypertension = Percent.of.Beneficiaries.with.Hypertension, IHD = Percent.of.Beneficiaries.with.IHD, Osteoperosis = Percent.of.Beneficiaries.with.Osteoporosis,
                          RA.OA = Percent.of.Beneficiaries.with.RA.OA, Schizophrenia = Percent.of.Beneficiaries.with.Schizophrenia, Stroke = Percent.of.Beneficiaries.with.Stroke)

#creating an initial parallel coordinate vizualization of the data
ggparcoord(state, columns = c(3,6:21), groupColumn = "State")

#revised parallel coordinates after dimensional reordering
ggparcoord(state, columns = c(3,20,18,7,16,15,21,13,9,11,12,14,8,19,17), groupColumn = "State")

#scatterplot examining Hypertension by Age and gender
p <- plot_ly(state, x = state$Female.Beneficiaries, y = state$Male.Beneficiaries, z = state$Hypertension, color = state$Average.Age) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Female Beneficieries'),
                      yaxis = list(title = 'Male Beneficieries'),
                      zaxis = list(title = 'Hypertension'),
                      title = 'Beneficiaries with Hypertension by Age and Gender'))

#scatterplot exploring the relationship between schizophrenia and depression
p2 <- plot_ly(state, x = state$Schizophrenia, y = state$Depression, z = state$Beneficiaries, color = state$State) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Beneficiaries with Schizophrenia'),
                      yaxis = list(title = 'Beneficiaries with Depression'),
                      zaxis = list(title = 'Total Beneficiaries'),
                      title = 'Beneficiaries with Alzheimers by Age and Gender'))


#correlation visualizations
M3 <- cor(state[,2:21])
corrplot(M3, method = "color")
#correlation significance visualization
p.mat <- cor.mtest(state[,c2:21])
corrplot(M3, order="hclust", 
         p.mat = p.mat, sig.level = 0.01)