library(data.table)
library(roxygen2)

#' calculateRFM
#'
# Description
#' Calculates the RFM score for a given dataset
#'
#' @details
#' \code{data} contains the transaction data. The data set must contain
#'             a column labelled 'Customer' that allows unique customer
#'             identification, a column labelled "TransDate", indicating
#'             the purchase date and a column labelled "PurchAmount"
#'             specifying the total spending per purchase.
#'
# Arguments
#' @param data the data set
#' @param weight_recency a number specifying the weight of the recency
#' @param weight_frequency a number specifying the weight of the frequency
#' @param weight_monetary a number specifying the weight of the monetary
#'
# Returned values
#' @return a table with the following columns:
#'         \code{Customer} the identification of the customer
#'         \code{recency} the recency score for that customer
#'         \code{frequency} the frequency score for that customer
#'         \code{monetary} the monetary score for that customer
#'         \code{finalscore} the final score with weights applied for that customer
#'         \code{group} the group the user belongs to
#'
#  Import packages that are required for using your package
#' @import data.table
#'
# Examples
#' @examples
#' calculateRFM(transactions, 20, 20, 60)
#' calculateRFM(transactions, 30, 40, 30)
#'
#' @export

calculateRFM <- function(data, weight_recency=1, weight_frequency=1, weight_monetary=1){

  # ensure it's a data.table
  data <- as.data.table(data)

  # adjusting values to ensure that the weights add up to one
  weight_recency2 <- weight_recency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_frequency2 <- weight_frequency/sum(weight_recency, weight_frequency, weight_monetary)
  weight_monetary2 <- weight_monetary/sum(weight_recency, weight_frequency, weight_monetary)

  print("weights are calculated")

  # RFM measures
  max.Date <- max(data[,TransDate])

  temp <- data[,list(
    recency = as.numeric(max.Date - max(TransDate)),
    frequency = .N,
    monetary = mean(PurchAmount)),
    by=Customer
  ]

  print("RFM Measure done")

  # RFM scores
  temp <- temp[,list(Customer,
                     recency = as.numeric(cut2(-recency, g=3)),
                     frequency = as.numeric(cut2(frequency, g=3)),
                     monetary = as.numeric(cut2(monetary, g=3)))]

  # Overall RFM score
  temp[,finalscore:=weight_recency2*recency+weight_frequency2*frequency+weight_monetary2*monetary]

  print("Overall RFM Measure done")

  # RFM group
  temp[,group:=round(finalscore)]

  # Return final table
  return(temp)
}
