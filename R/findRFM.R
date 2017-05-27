#' Compute RFM for Transaction Data
#'
#' The function calculates the RFM value of a given customer data.The function consumes customer
#' data in a fixed format and returns RFM values and scores for each customer.
#' \href{https://github.com/didroo55/didrooPackages/blob/master/didrooRFM.pdf}{Click here for an overview document}
#' \href{https://youtu.be/SojqDzHpKRA}{Click here for a VIDEO TUTORIAL}
#' @param customerdata - A data frame of the follwing coloumns - TransactionID, Customer ID, Date of Transaction (in date format),Amount of purchase
#' @param recencyWeight - Weight the model should assign to the recency factor
#' @param frequencyWeight - Weight the model should assign to the frequency factor
#' @param  monetoryWeight - Weight the model should assign to the monetory factor
#' @return A data frame summarized ar customer ID level with the folloiwng data :
#' @return Individual Recency, Frequency and Monetary Scores for the data set
#' @return Weighted individual Recency, Frequency and Monetary scores for the data set
#' @return Final RFM and Weighted RFM scores for each customer
#' @return Customer class on a 5 point scale
#' @examples
#' TransNo <- c('0','1')
#' CustomerID <- c('Cust1','Cust2')
#' DateofPurch <- as.Date(c('2010-11-1','2008-3-25'))
#' Amount <- c(1000,500)
#' customerData <- data.frame(TransNo,CustomerID,DateofPurch,Amount)
#' findRFM(customerData)
#' @import dplyr
#' @import graphics
#' @export
findRFM <- function(customerdata,
                    recencyWeight = 4,
                    frequencyWeight = 4,
                    monetoryWeight = 4)
{

  TransNo <- NULL
  CustomerID <- NULL
  Amount<- NULL
  DateofPurch<- NULL
  MeanValue<- NULL
  NoTransaction<- NULL
  LastTransaction<- NULL
  MonetoryPercentile<- NULL
  FrequencyPercentile<- NULL
  RecencyPercentile<- NULL
  MonetoryScore<- NULL
  FrequencyScore<- NULL
  RecencyScore<- NULL
  FinalScore<- NULL
  FinalWeightedScore<- NULL
  MonetoryWeightedScore<- NULL
  FrequencyWeightedScore<- NULL
  RecencyWeightedScore<- NULL
  colnames(customerdata) <- c("TransNo","CustomerID","DateofPurch","Amount")

  if (class(customerdata$DateofPurch) != "Date")
  {
    stop("The date of purchase should be in date format")
  }


  customerdata <- mutate(customerdata,TransNo = as.character(TransNo),
                         CustomerID = as.character(CustomerID))

  rfmData <- customerdata %>% group_by(CustomerID) %>%summarize(MeanValue = mean(Amount),
                                                                LastTransaction = max(DateofPurch),
                                                                NoTransaction = n())

  rfmData <- mutate(rfmData,
                    MonetoryPercentile = percent_rank(MeanValue),
                    FrequencyPercentile = percent_rank(NoTransaction),
                    RecencyPercentile = percent_rank(LastTransaction))
  rfmData <- rfmData %>%
    rowwise() %>%
    mutate(MonetoryScore = individualScore(MonetoryPercentile),
           FrequencyScore = individualScore(FrequencyPercentile),
           RecencyScore = individualScore(RecencyPercentile))

  rfmData <- mutate(rfmData,MonetoryWeightedScore = MonetoryScore*monetoryWeight,
                    FrequencyWeightedScore = FrequencyScore*frequencyWeight,
                    RecencyWeightedScore = RecencyScore*recencyWeight )
  rfmData <- mutate(rfmData,
                    FinalScore = RecencyWeightedScore+
                      FrequencyWeightedScore+
                      MonetoryWeightedScore)

  rfmData <- mutate(rfmData,
                    FinalWeightedScore = FinalScore/(monetoryWeight+frequencyWeight+recencyWeight))

  rfmData <- mutate(rfmData,
                    FinalCustomerClass = paste("Class",floor(FinalWeightedScore),sep = "-"))

  hist(rfmData$FinalWeightedScore)
  rfmData

}

individualScore <- function (percentile)
{
  if(percentile>0.8)
  {
    5
  }
  else if(percentile>0.6)
  {
    4
  }
  else if(percentile>0.4)
  {
    3
  }
  else if(percentile>0.2)
  {
    2
  }
  else
  {
    1
  }
}


