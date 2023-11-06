# Install packages
install.packages("data.table")

# Load required packages
library(data.table)
library(ggplot2)

# Load data
filePath <-"/home/limoo/Documents/Projects/Retail Analytics/"
data <- fread(paste0(filePath,"QVI_data.csv"))

# Set themes
theme_set(theme_bw())
theme_update(plot.title = element_text(hjust = 0.5))

# Calculate measures for each store - create month ID
data[, YEARMONTH := year(DATE) * 100 + month(DATE)]

# Define measure calculations
measureOverTIme <- data[, .(totSales = sum(TOT_SALES),
                 nCustomers = uniqueN(LYLTY_CARD_NBR),
                 nTxnPerCust = uniqueN(TXN_ID)/uniqueN(LYLTY_CARD_NBR),
                 nChipsPerTxn = sum(TOT_SALES)/sum(PROD_QTY)),
             by = c("STORE_NBR", "YEARMONTH")][order(STORE_NBR, YEARMONTH)]

# Filter to pre trial and stores with full observation
storeWithFullObs <- unique(measureOverTIme[, .N, STORE_NBR][N==12,STORE_NBR])
preTrialMeasures <- measureOverTIme[YEARMONTH < 201902 & STORE_NBR %in% storeWithFullObs,]

# Function to calculate correlation for a measure
calculateCorrelation <- function(inputTable, metricCol, storeComparison){
  calcCorrTable = data.table(Store1 = numeric(), Store2 = numeric(), corr_measure = numeric())
  storeNumbers <- unique(inputTable[, STORE_NBR])
  for (i in storeNumbers){
    calculatedMeasure = data.table("Store1" = storeComparison, "Store2" = i,
                                   "corr_measure" = cor(inputTable[STORE_NBR == storeComparison,
                                                                   eval(metricCol)], inputTable[STORE_NBR == i, eval(metricCol)]))
    calcCorrTable <- rbind(calcCorrTable, calculatedMeasure)
  }
  return(calcCorrTable)
}


# Function to calculate magnitude distance for a measure
calculateMagnitudeDistance <- function(inputTable, metricCol, storeComparison){
  calcDistTable = data.table(Store1 = numeric(), Store2 = numeric(), YEAR_MONTH = numeric(), measure = numeric())
  storeNumbers <- unique(inputTable[, STORE_NBR])
  for (i in storeNumbers){
    calculatedMeasure = data.table("Store1" = storeComparison, "Store2" = i, 
                                   "YEARMONTH" = inputTable[STORE_NBR == storeComparison, YEARMONTH],
                                   "measure" = abs(inputTable[STORE_NBR == storeComparison, eval(metricCol)] - inputTable[STORE_NBR == i, eval(metricCol)]))
    calcDistTable <- rbind(calcDistTable, calculatedMeasure, fill=TRUE)
  }
  # standardize the magnitude
  minMaxDist <- calcDistTable[, .(minDist = min(measure), maxDist = max(measure)), by= c("Store1", "YEARMONTH")]
  distTable <- merge(calcDistTable, minMaxDist, by = c("Store1", "YEARMONTH"))
  distTable[, magnitudeMeasure := 1 - (measure - minDist)/(maxDist - minDist)]
  finalDistTable <- distTable[, .(mag_measure = mean(magnitudeMeasure)), by = .(Store1, Store2)]
  return(finalDistTable)
}

# Calculate correlations
trial_store <- 77
corr_nSales <- calculateCorrelation(preTrialMeasures, quote(totSales),trial_store)
corr_nCustomers <- calculateCorrelation(preTrialMeasures, quote(nCustomers), trial_store)

# Calculate magnitudes
magnitude_nSales <- calculateMagnitudeDistance(preTrialMeasures, quote(totSales), trial_store)
magnitude_nCustomers <- calculateMagnitudeDistance(preTrialMeasures, quote(nCustomers), trial_store)


# Combine score of correlation and magnitude
corr_weight <- 0.5
score_nSales <- merge(corr_nSales, magnitude_nSales, by = c("Store1",
                "Store2"))[,scoreNSales := corr_measure * corr_weight + mag_measure * (1- corr_weight)]
score_nCustomers <- merge(corr_nCustomers, magnitude_nCustomers, by = c("Store1",
                    "Store2"))[, scoreNCust := corr_measure * corr_weight + mag_measure * (1 - corr_weight)]

# Combine scores across the drivers
score_Control <- merge(score_nSales, score_nCustomers, by= c("Store1", "Store2"))
score_Control[, finalControlScore := scoreNSales * 0.5 + scoreNCust * 0.5]

# Store with highest score is the control store (most similar to the trial store)
## control store for trial store 77
control_store <- score_Control[Store1 == trial_store, ][order(-finalControlScore)][2, Store2]
control_store


# Plot visualizations
## Total sales
measureOverTImeSales <- measureOverTIme
pastSales <- measureOverTImeSales[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                         ifelse(STORE_NBR == control_store, "Control", "Other stores"))
                                  ][, totSales := mean(totSales), by = c("YEARMONTH", "Store_type")
                                    ][,TransactionMonth := as.Date(paste(YEARMONTH %/%100, YEARMONTH %% 100, 1, sep="-"), "%Y-%m-%d")
                                      ][YEARMONTH < 201903, ]
ggplot(pastSales, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_line() + 
  labs(x = "Operation month", y = "Total sales", title = "Total Sales by Month")

## Number of customers
measureOverTImeCusts <- measureOverTIme
pastCustomers <- measureOverTImeCusts[, Store_type := ifelse(STORE_NBR == trial_store, "Trial",
                                                             ifelse(STORE_NBR == control_store, "Control", "Other stores"))
                                      ][, numberCustomers := mean(nCustomers), by = c("YEARMONTH", "Store_type")
                                        ][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
                                          ][YEARMONTH < 201903, ]
ggplot(pastCustomers, aes(TransactionMonth, numberCustomers, color = Store_type)) +
  geom_line() +
  labs(x = "Operation month", y = " Total customers", title = "Total Customers by Month")


# Trial Assessments

# scale pre trial control stores
scalingFactorControlSales <- preTrialMeasures[STORE_NBR == trial_store & YEARMONTH < 201902, sum(totSales)]/
  preTrialMeasures[STORE_NBR == control_store & YEARMONTH < 201902, sum(totSales)]

# Apply scaling factor
measureOverTImeSales <- measureOverTIme
scaledControlSales <- measureOverTImeSales[STORE_NBR == control_store, ][, controlSales := totSales * scalingFactorControlSales]

# Percentage difference btwn scaled control sales and trial sales
percDiff <- merge(scaledControlSales[, c("YEARMONTH", "controlSales")],
                  measureOverTIme[STORE_NBR == trial_store, c("totSales", "YEARMONTH")],
                  by = "YEARMONTH")[, percDiff := abs(controlSales - totSales)/controlSales]

stdDev <- sd(percDiff[YEARMONTH < 201902, percDiff])

# 8 months in pretrial period (8 - 1 = 7 degrees of freedom)
degreesOfFreedom <- 7

# Test with a null hypothesis (0 diff btwn trial and control stores)
percDiff[, tValue := (percDiff - 0)/stdDev][, TransactionMonth := as.Date(paste(YEARMONTH %/% 100, YEARMONTH %% 100, 1, sep = "-"), "%Y-%m-%d")
                                            ][YEARMONTH < 201905 & YEARMONTH > 201901, .(TransactionMonth, tValue)]

# Find 95th percentile
qt(0.95, df=degreesOfFreedom)

## >>>> the increase in sales in the trial store in March and April is statistically greater than in the control store

# Visual check
## Control store 95th percentile
pastSales_control95 <- pastSales[Store_type == "Control", 
                                 ][, totSales := totSales * (1 + stdDev *2)
                                   ][, Store_type := "Control 95th % confidence interval"]

## Control store 5th percentile
pastSales_control5 <- pastSales[Store_type == "Control",
                               ][, totSales := totSales * (1 - stdDev * 2)
                                 ][, Store_type := "Control 5th % confidence interval"]

trialAssessment <- rbind(pastSales, pastSales_control95, pastSales_control5)

ggplot(trialAssessment, aes(TransactionMonth, totSales, color = Store_type)) +
  geom_rect(data = trialAssessment[YEARMONTH < 201905 & YEARMONTH > 201901, ],
            aes(xmin = min(TransactionMonth), xmax = max(TransactionMonth), ymin = 0, 
                ymax = Inf, color = NULL), show.legend = FALSE) +
  geom_line() +
  labs(x = "Operation month", y = "Total sales", title = "Total Sales by Month")

