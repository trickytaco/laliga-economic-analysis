#Library to read Excel files
library(xlsx)
#library(ggplot2)

#Set the working directory
setwd("D:\\Statistics\\LaLiga\\laliga-economic-analysis")

#Read in the standings for each season from 2005-06 to 2014-15
standingsList <- list()
sheetNames <- c("2005-06", "2006-07", "2007-08", "2008-09", "2009-10",
                "2010-11", "2011-12", "2012-13", "2013-14", "2014-15")

for (i in 1:10) {
    standingsList[[i]] <- read.xlsx("La Liga Standings.xlsx",
                                    sheetName = sheetNames[i])
}

#Read in the Points sheet and set the column names
PointsDF <- read.xlsx("La Liga economics.xlsx", sheetName = "Points")
names(PointsDF) <- c("Team_ID", "Team", "2005-06", "2006-07", "2007-08",
                     "2008-09", "2009-10", "2010-11", "2011-12", "2012-13",
                     "2013-14", "2014-15")
#Remove some non-data rows
PointsDF <- PointsDF[-1:-3,-1]

#Calculate the maximum value of the entire data table for plotting purposes
allMax <- 0
for (i in 2:11) {
    if (max(PointsDF[,i][complete.cases(PointsDF[,i])]) > allMax) {
        allMax <- max(PointsDF[,i][complete.cases(PointsDF[,i])])
    }
}

#Plot the first row's data as a line
plot(factor(names(PointsDF)[2:11]), PointsDF[2,2:11], type="l",
     ylim = c(0,allMax), xaxt = "n", xlab = "Season", ylab = "Points",
     main = "Points by Season")
#Add some information to the bottom axis
axis(1, 1:10, names(PointsDF)[2:11])

#Now add a line for every other team to the plot
for (i in 2:36) {
    lines(1:10, PointsDF[i,2:11], col = i)
}

#Copy the device to a png device
dev.copy(png, file = ".\\plots\\points.png", width = 1280, height = 720, units = "px")

#Close the device to save the file
dev.off()

#Read in the transfer expense sheet and set the column names
TransExpDF <- read.xlsx("La Liga economics.xlsx", sheetName = "Transfer expense")
names(TransExpDF) <- c("Team_ID", "Team", "2005-06", "2006-07", "2007-08",
                     "2008-09", "2009-10", "2010-11", "2011-12", "2012-13",
                     "2013-14", "2014-15")
#Remove some non-data rows
TransExpDF <- TransExpDF[-37:-39,-1]

#Calculate the maximum value of the entire data table for plotting purposes
allMax <- 0
for (i in 2:11) {
    if (max(TransExpDF[,i][complete.cases(TransExpDF[,i])]) > allMax) {
        allMax <- max(TransExpDF[,i][complete.cases(TransExpDF[,i])])
    }
}

#Plot the first row's data as a line
plot(factor(names(TransExpDF)[2:11]), TransExpDF[2,2:11], type="l",
     ylim = c(0,allMax), xaxt = "n", xlab = "Season",
     ylab = "Transfer Expenditure", main = "Transfer Expenditure by Season")
#Add some information to the bottom axis
axis(1, 1:10, names(TransExpDF)[2:11])

#Now add a line for every other team to the plot
for (i in 1:36) {
    lines(1:10, TransExpDF[i,2:11], col = i)
}

#Copy the device to a png device
dev.copy(png, file = ".\\plots\\transfer_expense.png", width = 1280, height = 720, units = "px")

#Close the device to save the file
dev.off()

#Read in the transfer average expense sheet and set the column names
TransExpAvgDF <- read.xlsx("La Liga economics.xlsx", sheetName = "Transfer expense - Rolling avg")
names(TransExpAvgDF) <- c("Team_ID", "Team", "2005-06", "2006-07", "2007-08",
                       "2008-09", "2009-10", "2010-11", "2011-12", "2012-13",
                       "2013-14", "2014-15")
#Remove some non-data rows
TransExpAvgDF <- TransExpAvgDF[-37:-39,-1]

#Calculate the maximum value of the entire data table for plotting purposes
allMax <- 0
for (i in 2:11) {
    if (max(TransExpAvgDF[,i][complete.cases(TransExpAvgDF[,i])]) > allMax) {
        allMax <- max(TransExpAvgDF[,i][complete.cases(TransExpAvgDF[,i])])
    }
}

#Plot the first row's data as a line
plot(factor(names(TransExpAvgDF)[2:11]), TransExpAvgDF[2,2:11], type="l",
     ylim = c(0,allMax), xaxt = "n", xlab = "Season",
     ylab = "Transfer Expenditure Average",
     main = "Transfer Expenditure Average by Season")
#Add some information to the bottom axis
axis(1, 1:10, names(TransExpAvgDF)[2:11])

#Now add a line for every other team to the plot
for (i in 1:36) {
    lines(1:10, TransExpAvgDF[i,2:11], col = i)
}

#Copy the device to a png device
dev.copy(png, file = ".\\plots\\transfer_expense_average.png", width = 1280, height = 720, units = "px")

#Close the device to save the file
dev.off()

#Read in the net transfer expense sheet and set the column names
NetTransDF <- read.xlsx("La Liga economics.xlsx", sheetName = "Net transfer spend")
names(NetTransDF) <- c("Team_ID", "Team", "2005-06", "2006-07", "2007-08",
                       "2008-09", "2009-10", "2010-11", "2011-12", "2012-13",
                       "2013-14", "2014-15")
#Remove some non-data rows
NetTransDF <- NetTransDF[-37:-39,-1]

#Calculate the maximum and minimum value of the entire data table for plotting
#purposes
allMax <- 0
allMin <- 0
for (i in 2:11) {
    if (max(NetTransDF[,i][complete.cases(NetTransDF[,i])]) > allMax) {
        allMax <- max(NetTransDF[,i][complete.cases(NetTransDF[,i])])
    }
    if (min(NetTransDF[,i][complete.cases(NetTransDF[,i])]) < allMin) {
        allMin <- min(NetTransDF[,i][complete.cases(NetTransDF[,i])])
    }
}

#Plot the first row's data as a line
plot(factor(names(NetTransDF)[2:11]), NetTransDF[2,2:11], type="l",
     ylim = c(allMin,allMax), xaxt = "n", xlab = "Season",
     ylab = "Net Transfer Expenditure",
     main = "Net Transfer Expenditure by Season")
#Add some information to the bottom axis
axis(1, 1:10, names(NetTransDF)[2:11])

#Now add a line for every other team to the plot
for (i in 1:36) {
    lines(1:10, NetTransDF[i,2:11], col = i)
}

#Copy the device to a png device
dev.copy(png, file = ".\\plots\\net_transfer_spend.png", width = 1280, height = 720, units = "px")

#Close the device to save the file
dev.off()

#Read in the net transfer expense average sheet and set the column names
NetTransAvgDF <- read.xlsx("La Liga economics.xlsx", sheetName = "Net transfer spend - Rolling av")
names(NetTransAvgDF) <- c("Team_ID", "Team", "2005-06", "2006-07", "2007-08",
                       "2008-09", "2009-10", "2010-11", "2011-12", "2012-13",
                       "2013-14", "2014-15")
#Remove some non-data rows
NetTransAvgDF <- NetTransAvgDF[-37:-39,-1]

#Calculate the maximum and minimum value of the entire data table for plotting
#purposes
allMax <- 0
allMin <- 0
for (i in 2:11) {
    if (max(NetTransAvgDF[,i][complete.cases(NetTransAvgDF[,i])]) > allMax) {
        allMax <- max(NetTransAvgDF[,i][complete.cases(NetTransAvgDF[,i])])
    }
    if (min(NetTransAvgDF[,i][complete.cases(NetTransAvgDF[,i])]) < allMin) {
        allMin <- min(NetTransAvgDF[,i][complete.cases(NetTransAvgDF[,i])])
    }
}

#Plot the first row's data as a line
plot(factor(names(NetTransAvgDF)[2:11]), NetTransAvgDF[2,2:11], type="l",
     ylim = c(allMin,allMax), xaxt = "n", xlab = "Season",
     ylab = "Net Transfer Expenditure Average",
     main = "Net Transfer Expenditure Average by Season")
#Add some information to the bottom axis
axis(1, 1:10, names(NetTransAvgDF)[2:11])

#Now add a line for every other team to the plot
for (i in 1:36) {
    lines(1:10, NetTransAvgDF[i,2:11], col = i)
}

#Copy the device to a png device
dev.copy(png, file = ".\\plots\\net_transfer_spend_average.png", width = 1280, height = 720, units = "px")

#Close the device to save the file
dev.off()

#Read in the market value sheet and set the column names
MarketValueDF <- read.xlsx("La Liga economics.xlsx", sheetName = "Market Value")
names(MarketValueDF) <- c("Team_ID", "Team", "2005-06", "2006-07", "2007-08",
                       "2008-09", "2009-10", "2010-11", "2011-12", "2012-13",
                       "2013-14", "2014-15")
#Remove some non-data rows
MarketValueDF <- MarketValueDF[-37:-39,-1]

#Calculate the maximum value of the entire data table for plotting purposes
allMax <- 0
for (i in 2:11) {
    if (max(MarketValueDF[,i][complete.cases(MarketValueDF[,i])]) > allMax) {
        allMax <- max(MarketValueDF[,i][complete.cases(MarketValueDF[,i])])
    }
}

#Plot the first row's data as a line
plot(factor(names(MarketValueDF)[2:11]), MarketValueDF[2,2:11], type="l",
     ylim = c(0,allMax), xaxt = "n", xlab = "Season", ylab = "Market Value",
     main = "Market Value by Season")
#Add some information to the bottom axis
axis(1, 1:10, names(MarketValueDF)[2:11])

#Now add a line for every other team to the plot
for (i in 1:36) {
    lines(1:10, MarketValueDF[i,2:11], col = i)
}

#Copy the device to a png device
dev.copy(png, file = ".\\plots\\market_value.png", width = 1280, height = 720, units = "px")

#Close the device to save the file
dev.off()

#Correlate TransExpAvgDF to PointsDF year-by-year
for (i in 2:11) {
    #Convert PointsDF and TransExpAvgDF into one-dimensional vectors so that
    #the correlation can be calculated.  The complete.cases part is so that
    #the operation excludes the NA values.
    PointsDFVector <- c(as.matrix(PointsDF[,i]))[complete.cases(c(as.matrix(PointsDF[,i])))]
    TransExpAvgDFVector <- c(as.matrix(TransExpAvgDF[,i]))[complete.cases(c(as.matrix(TransExpAvgDF[,i])))]
    
    #Now calculate the correlation between the two vectors
    corVal <- cor(PointsDFVector, TransExpAvgDFVector)

    #Print the results along with the year
    print(paste(names(PointsDF)[i], as.character(corVal)))
}

#Correlate TransExpAvgDF to PointsDF year-by-year (no Madrid or Barcelona)
for (i in 2:11) {
    #Convert PointsDF and TransExpAvgDF into one-dimensional vectors so that
    #the correlation can be calculated.  The complete.cases part is so that
    #the operation excludes the NA values.  The -1:-2 bits cause the first two
    #rows to be excluded.  These are the RMD/BAR rows.
    PointsDFVector <- c(as.matrix(PointsDF[-1:-2,i]))[complete.cases(c(as.matrix(PointsDF[-1:-2,i])))]
    TransExpAvgDFVector <- c(as.matrix(TransExpAvgDF[-1:-2,i]))[complete.cases(c(as.matrix(TransExpAvgDF[-1:-2,i])))]
    
    #Now calculate the correlation between the two vectors
    corVal <- cor(PointsDFVector, TransExpAvgDFVector)
    
    #Print the results along with the year
    print(paste(names(PointsDF)[i], as.character(corVal)))
}

#Correlate MarketValueDF to PointsDF year-by-year
for (i in 2:11) {
    PointsDFVector <- c(as.matrix(PointsDF[,i]))[complete.cases(c(as.matrix(PointsDF[,i])))]
    MarketValueDFVector <- c(as.matrix(MarketValueDF[,i]))[complete.cases(c(as.matrix(MarketValueDF[,i])))]
    corVal <- cor(PointsDFVector, MarketValueDFVector)
    print(paste(names(PointsDF)[i], as.character(corVal)))
}

#Correlate MarketValueDF to PointsDF year-by-year (no Madrid or Barcelona)
for (i in 2:11) {
    #Convert PointsDF and TransExpAvgDF into one-dimensional vectors so that
    #the correlation can be calculated.  The complete.cases part is so that
    #the operation excludes the NA values.  The -1:-2 bits cause the first two
    #rows to be excluded.  These are the RMD/BAR rows.
    PointsDFVector <- c(as.matrix(PointsDF[-1:-2,i]))[complete.cases(c(as.matrix(PointsDF[-1:-2,i])))]
    MarketValueDFVector <- c(as.matrix(MarketValueDF[-1:-2,i]))[complete.cases(c(as.matrix(MarketValueDF[-1:-2,i])))]
    
    
    #Now calculate the correlation between the two vectors
    corVal <- cor(PointsDFVector, MarketValueDFVector)
    
    #Print the results along with the year
    print(paste(names(PointsDF)[i], as.character(corVal)))
}

#Calculate the maximum Market value for plotting purposes
marketMax <- 0
for (i in 2:11) {
    if (max(MarketValueDF[,i][complete.cases(MarketValueDF[,i])]) > marketMax) {
        marketMax <- max(MarketValueDF[,i][complete.cases(MarketValueDF[,i])])
    }
}

#Create a linear regression model of points earned vs. market value (all
#clubs.)
for (i in 2:11) {
    #Convert PointsDF and MarketValueDF into one-dimensional vectors so that
    #the regression can be calculated.  The complete.cases part is so that
    #the operation excludes the NA values.
    PointsDFVector <- c(as.matrix(PointsDF[,i]))[complete.cases(c(as.matrix(PointsDF[,i])))]
    MarketValueDFVector <- c(as.matrix(MarketValueDF[,i]))[complete.cases(c(as.matrix(MarketValueDF[,i])))]
    
    #Create the linear model and print the details
    pts_MktValFit <- lm(PointsDFVector ~ MarketValueDFVector)
    print(names(PointsDF)[i])
    print(summary(pts_MktValFit))
    
    #Plot the data points and trend line
    if (summary(pts_MktValFit)$coefficients[2] >= 0) {
        signStr <- "+"
    } else {
        signStr <- "-"
    }
    regFormula <- paste(round(summary(pts_MktValFit)$coefficients[1], 4), " ",
                     signStr, " ",
                     round(summary(pts_MktValFit)$coefficients[2], 4),
                     "x", sep="")
    plot(MarketValueDFVector, PointsDFVector, type="p",
         xlim=c(0, marketMax), ylim=c(0, 114),
         main=paste("Market Value vs. Points, ",
                    names(PointsDF)[i], "\n", regFormula, 
                    ", R-Squared = ",
                    round(summary(pts_MktValFit)$adj.r.squared, 5), sep=""),
         xlab = "Market Value", ylab = "Points", pch=19)
    abline(pts_MktValFit)
    
    #Copy the device to a png device
    dev.copy(png, file = paste(".\\plots\\points_vs_market_value_",
                               names(PointsDF)[i], ".png", sep=""),
             width = 1280, height = 720, units = "px")
    
    #Close the device to save the file
    dev.off()
}


#Calculate the maximum Market value for plotting purposes (no RMD or BAR)
marketMaxNORMDBAR <- 0
for (i in 2:11) {
    if (max(MarketValueDF[-1:-2,i][complete.cases(MarketValueDF[-1:-2,i])]) > marketMaxNORMDBAR) {
        marketMaxNORMDBAR <- max(MarketValueDF[-1:-2,i][complete.cases(MarketValueDF[-1:-2,i])])
    }
}

#Create a linear regression model of points earned vs. market value (no
#RMD or BAR.)
for (i in 2:11) {
    #Convert PointsDF and MarketValueDF into one-dimensional vectors so that
    #the regression can be calculated.  The complete.cases part is so that
    #the operation excludes the NA values.
    PointsDFVector <- c(as.matrix(PointsDF[-1:-2,i]))[complete.cases(c(as.matrix(PointsDF[-1:-2,i])))]
    MarketValueDFVector <- c(as.matrix(MarketValueDF[-1:-2,i]))[complete.cases(c(as.matrix(MarketValueDF[-1:-2,i])))]
    
    #Create the linear model and print the details
    pts_MktValFit <- lm(PointsDFVector ~ MarketValueDFVector)
    print(names(PointsDF)[i])
    print(summary(pts_MktValFit))
    
    #Plot the data points and trend line
    if (summary(pts_MktValFit)$coefficients[2] >= 0) {
        signStr <- "+"
    } else {
        signStr <- "-"
    }
    regFormula <- paste(round(summary(pts_MktValFit)$coefficients[1], 4), " ",
                        signStr, " ",
                        round(summary(pts_MktValFit)$coefficients[2], 4),
                        "x", sep="")
    plot(MarketValueDFVector, PointsDFVector, type="p",
         xlim=c(0, marketMaxNORMDBAR), ylim=c(0, 114),
         main=paste("Market Value vs. Points (No RMD/BAR), ",
                    names(PointsDF)[i], "\n", regFormula, 
                    ", R-Squared = ",
                    round(summary(pts_MktValFit)$adj.r.squared, 5), sep=""),
         xlab = "Market Value", ylab = "Points", pch=19)
    abline(pts_MktValFit)
    
    #Copy the device to a png device
    dev.copy(png, file = paste(".\\plots\\points_vs_market_value_",
                               names(PointsDF)[i], "_noRMDBAR.png", sep=""),
             width = 1280, height = 720, units = "px")
    
    #Close the device to save the file
    dev.off()
}

#Calculate the maximum transfer expense average value for plotting purposes
transExpAvgMax <- 0
for (i in 2:11) {
    if (max(TransExpAvgDF[,i][complete.cases(TransExpAvgDF[,i])]) > transExpAvgMax) {
        transExpAvgMax <- max(TransExpAvgDF[,i][complete.cases(TransExpAvgDF[,i])])
    }
}

#Create a linear regression model of points earned vs. transfer expense average
#(all clubs.)
for (i in 2:11) {
    #Convert PointsDF and TransExpAvgDF into one-dimensional vectors so that
    #the regression can be calculated.  The complete.cases part is so that
    #the operation excludes the NA values.
    PointsDFVector <- c(as.matrix(PointsDF[,i]))[complete.cases(c(as.matrix(PointsDF[,i])))]
    TransExpAvgDFVector <- c(as.matrix(TransExpAvgDF[,i]))[complete.cases(c(as.matrix(TransExpAvgDF[,i])))]
    
    #Create the linear model and print the details
    pts_TExpAvgFit <- lm(PointsDFVector ~ TransExpAvgDFVector)
    print(names(PointsDF)[i])
    print(summary(pts_TExpAvgFit))
    
    #Plot the data points and trend line
    if (summary(pts_TExpAvgFit)$coefficients[2] >= 0) {
        signStr <- "+"
    } else {
        signStr <- "-"
    }
    regFormula <- paste(round(summary(pts_TExpAvgFit)$coefficients[1], 4), " ",
                        signStr, " ",
                        round(summary(pts_TExpAvgFit)$coefficients[2], 4),
                        "x", sep="")
    plot(TransExpAvgDFVector, PointsDFVector, type="p",
         xlim=c(0, transExpAvgMax), ylim=c(0, 114),
         main=paste("Transfer Expense Average vs. Points, ",
                    names(PointsDF)[i], "\n", regFormula, 
                    ", R-Squared = ",
                    round(summary(pts_TExpAvgFit)$adj.r.squared, 5), sep=""),
         xlab = "Transfer Expense Average", ylab = "Points", pch=19)
    abline(pts_TExpAvgFit)
    
    #Copy the device to a png device
    dev.copy(png, file = paste(".\\plots\\points_vs_transfer_expense_average_",
                               names(PointsDF)[i], ".png", sep=""),
             width = 1280, height = 720, units = "px")
    
    #Close the device to save the file
    dev.off()
}


#Calculate the maximum transfer expense average value for plotting purposes (no RMD or BAR)
transExpAvgMaxNORMDBAR <- 0
for (i in 2:11) {
    if (max(TransExpAvgDF[-1:-2,i][complete.cases(TransExpAvgDF[-1:-2,i])]) > transExpAvgMaxNORMDBAR) {
        transExpAvgMaxNORMDBAR <- max(TransExpAvgDF[-1:-2,i][complete.cases(TransExpAvgDF[-1:-2,i])])
    }
}

#Create a linear regression model of points earned vs. transfer expense average (no
#RMD or BAR.)
for (i in 2:11) {
    #Convert PointsDF and MarketValueDF into one-dimensional vectors so that
    #the regression can be calculated.  The complete.cases part is so that
    #the operation excludes the NA values.
    PointsDFVector <- c(as.matrix(PointsDF[-1:-2,i]))[complete.cases(c(as.matrix(PointsDF[-1:-2,i])))]
    TransExpAvgDFVector <- c(as.matrix(TransExpAvgDF[-1:-2,i]))[complete.cases(c(as.matrix(TransExpAvgDF[-1:-2,i])))]
    
    #Create the linear model and print the details
    pts_TExpAvgFit <- lm(PointsDFVector ~ TransExpAvgDFVector)
    print(names(PointsDF)[i])
    print(summary(pts_TExpAvgFit))
    
    #Plot the data points and trend line
    if (summary(pts_TExpAvgFit)$coefficients[2] >= 0) {
        signStr <- "+"
    } else {
        signStr <- "-"
    }
    regFormula <- paste(round(summary(pts_TExpAvgFit)$coefficients[1], 4), " ",
                        signStr, " ",
                        round(summary(pts_TExpAvgFit)$coefficients[2], 4),
                        "x", sep="")
    plot(TransExpAvgDFVector, PointsDFVector, type="p",
         xlim=c(0, transExpAvgMaxNORMDBAR), ylim=c(0, 114),
         main=paste("Transfer Expense Average vs. Points (No RMD/BAR), ",
                    names(PointsDF)[i], "\n", regFormula, 
                    ", R-Squared = ",
                    round(summary(pts_TExpAvgFit)$adj.r.squared, 5), sep=""),
         xlab = "Transfer Expense Average", ylab = "Points", pch=19)
    abline(pts_TExpAvgFit)
    
    #Copy the device to a png device
    dev.copy(png, file = paste(".\\plots\\points_vs_transfer_expense_average_",
                               names(PointsDF)[i], "_noRMDBAR.png", sep=""),
             width = 1280, height = 720, units = "px")
    
    #Close the device to save the file
    dev.off()
}

#COMMENTED OUT BECAUSE THIS ISN'T REALLY A VALID CALCULATION

# #Calculate the maximum goal differential value for plotting purposes
# goalDiffMin <- 0
# goalDiffMax <- 0
# for (i in 1:10) {
#     if (max(standingsList[[i]]$GD) > goalDiffMax) {
#         goalDiffMax <- max(standingsList[[i]]$GD)
#     }
#     if (min(standingsList[[i]]$GD) < goalDiffMin) {
#         goalDiffMin <- min(standingsList[[i]]$GD)
#     }
# }

# #Create a linear regression model of goal differential vs. market value
# #(all clubs.)
# for (i in 1:10) {
#     #Extract each season in standingsList and perform the calculations
#     goalDiffVector <- standingsList[[i]]$GD
#     TransExpAvgDFVector <- c(as.matrix(TransExpAvgDF[,i+1]))[match(standingsList[[i]]$Team, PointsDF$Team)]

#     #Create the linear model and print the details
#     GD_TExpAvgFit <- lm(goalDiffVector ~ TransExpAvgDFVector)
#     print(names(PointsDF)[i+1])
#     print(summary(GD_TExpAvgFit))
#     
#     #Plot the data points and trend line
#     if (summary(GD_TExpAvgFit)$coefficients[2] >= 0) {
#         signStr <- "+"
#     } else {
#         signStr <- "-"
#     }
#     regFormula <- paste(round(summary(GD_TExpAvgFit)$coefficients[1], 4), " ",
#                         signStr, " ",
#                         round(summary(GD_TExpAvgFit)$coefficients[2], 4),
#                         "x", sep="")
#     plot(TransExpAvgDFVector, goalDiffVector, type="p",
#          xlim=c(0, transExpAvgMax), ylim=c(goalDiffMin, goalDiffMax),
#          main=paste("Transfer Expense Average vs. Goal Differential, ",
#                     names(PointsDF)[i+1], "\n", regFormula, 
#                     ", R-Squared = ",
#                     round(summary(GD_TExpAvgFit)$adj.r.squared, 5), sep=""),
#          xlab = "Transfer Expense Average", ylab = "Goal Differential", pch=19)
#     abline(GD_TExpAvgFit)
#     
#     #Copy the device to a png device
#     dev.copy(png, file = paste(".\\plots\\goal_differential_vs_transfer_expense_average_",
#                                names(PointsDF)[i+1], ".png", sep=""),
#              width = 1280, height = 720, units = "px")
#     
#     #Close the device to save the file
#     dev.off()
# }

#Calculate the maximum goals for value for plotting purposes
goalsForMax <- 0
for (i in 1:10) {
    if (max(standingsList[[i]]$GD) > goalsForMax) {
        goalsForMax <- max(standingsList[[i]]$GD)
    }
}

#Create a linear regression model of goals for vs. market value
#(all clubs.)
for (i in 1:10) {
    #Extract each season in standingsList and perform the calculations
    goalsForVector <- standingsList[[i]]$OF
    TransExpAvgDFVector <- c(as.matrix(TransExpAvgDF[,i+1]))[match(standingsList[[i]]$Team, PointsDF$Team)]
    
    #Create the linear model and print the details
    GF_TExpAvgFit <- lm(goalsForVector ~ TransExpAvgDFVector)
    print(names(PointsDF)[i+1])
    print(summary(GF_TExpAvgFit))
    
    #Plot the data points and trend line
    if (summary(GF_TExpAvgFit)$coefficients[2] >= 0) {
        signStr <- "+"
    } else {
        signStr <- "-"
    }
    regFormula <- paste(round(summary(GF_TExpAvgFit)$coefficients[1], 4), " ",
                        signStr, " ",
                        round(summary(GF_TExpAvgFit)$coefficients[2], 4),
                        "x", sep="")
    plot(TransExpAvgDFVector, goalsForVector, type="p",
         xlim=c(0, transExpAvgMax), ylim=c(0, goalsForMax),
         main=paste("Transfer Expense Average vs. Goals For, ",
                    names(PointsDF)[i+1], "\n", regFormula, 
                    ", R-Squared = ",
                    round(summary(GF_TExpAvgFit)$adj.r.squared, 5), sep=""),
         xlab = "Transfer Expense Average", ylab = "Goals For", pch=19)
    abline(GF_TExpAvgFit)
    
    #Copy the device to a png device
    dev.copy(png, file = paste(".\\plots\\goals_for_vs_transfer_expense_average_",
                               names(PointsDF)[i+1], ".png", sep=""),
             width = 1280, height = 720, units = "px")
    
    #Close the device to save the file
    dev.off()
}

#Calculate the maximum goals for value for plotting purposes (no RMD or BAR)
goalsForMaxNORMDBAR <- 0
for (i in 1:10) {
    if (max(standingsList[[i]]$OF[!(standingsList[[i]]$Team %in% c("Real Madrid", "Barcelona"))]) > goalsForMaxNORMDBAR) {
        goalsForMaxNORMDBAR <- max(standingsList[[i]]$OF[!(standingsList[[i]]$Team %in% c("Real Madrid", "Barcelona"))])
    }
}

#Create a linear regression model of goals for vs. market value
#(no RMD or BAR.)
for (i in 1:10) {
    #Extract each season in standingsList and perform the calculations
    goalsForVector <- standingsList[[i]]$OF[!(standingsList[[i]]$Team %in% c("Real Madrid", "Barcelona"))]
    TransExpAvgDFVector <- c(as.matrix(TransExpAvgDF[,i+1]))[match(standingsList[[i]]$Team[!(standingsList[[i]]$Team %in% c("Real Madrid", "Barcelona"))], PointsDF$Team)]
    
    #Create the linear model and print the details
    GF_TExpAvgFit <- lm(goalsForVector ~ TransExpAvgDFVector)
    print(names(PointsDF)[i+1])
    print(summary(GF_TExpAvgFit))
    
    #Plot the data points and trend line
    if (summary(GF_TExpAvgFit)$coefficients[2] >= 0) {
        signStr <- "+"
    } else {
        signStr <- "-"
    }
    regFormula <- paste(round(summary(GF_TExpAvgFit)$coefficients[1], 4), " ",
                        signStr, " ",
                        round(summary(GF_TExpAvgFit)$coefficients[2], 4),
                        "x", sep="")
    plot(TransExpAvgDFVector, goalsForVector, type="p",
         xlim=c(0, transExpAvgMaxNORMDBAR), ylim=c(0, goalsForMaxNORMDBAR),
         main=paste("Transfer Expense Average vs. Goals For (No RMD/BAR), ",
                    names(PointsDF)[i+1], "\n", regFormula, 
                    ", R-Squared = ",
                    round(summary(GF_TExpAvgFit)$adj.r.squared, 5), sep=""),
         xlab = "Transfer Expense Average", ylab = "Goals For", pch=19)
    abline(GF_TExpAvgFit)
    
    #Copy the device to a png device
    dev.copy(png, file = paste(".\\plots\\goals_for_vs_transfer_expense_average_",
                               names(PointsDF)[i+1], "_noRMDBAR.png", sep=""),
             width = 1280, height = 720, units = "px")
    
    #Close the device to save the file
    dev.off()
}