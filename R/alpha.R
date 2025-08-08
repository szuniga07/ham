#' Calculates Cronbach's alpha on scale items
#'
#' Performs Cronbach's alpha of specified items from a data frame.
#' Cronbach's Alpha is a formula for estimating the internal consistency
#' reliability of a measurement instrument such as survey items  (see Allen & Yang, 1979; Kline, 1999).
#' Survey items can have 2 or more categories such as 5-point scales and contain 2 or more items.
#'
#' @param items Vector of items names that form a scale (e.g., 5-point Likert scales)
#'
#' @param data  Data frame object.
#'
#' @return A character vector.
#'
#' @references
#' Allen, M. J., & Yen, W. M. (1979). Introduction to Measurement Theory.
#' Brooks/Cole. ISBN: 0-8185-0283-5.
#' Kline, Paul (1999). Handbook of Psychological Testing (2nd ed).
#' Routledge, New York. ISBN: 9780415211581.
#'
#' @examples
#' alpha(items=c("i1","i2","i3","i4","i5"), data=cas)
#'
#' # remove i1 as suggested in the previous example, returns higher alpha
#' alpha(items=c("i2","i3","i4","i5"), data=cas)
#' @export
#'
alpha <- function(items, data) {
  if (sum( length(quote(items)), length(quote(data)) ) != 2) stop("Error: You did not type both names for 'items' and 'data' arguments.")  #If statement expression prints this.
  if (is.character(items) != TRUE) stop("Error: Expecting a character vector.")
  if (!class(data)[1] %in% c("data.frame","matrix")) stop("Error: Expecting a data frame.")
  if( length(items) ==1) stop("Error: You have only 1 item in your scale.")
  if( length(items) ==2) warning("Consider adding more items to your scale. Scale statistics if item deleted are not calculated.")

  talpha <- function(items=NULL, data=NULL) {
    #Select correct column numbers
    scale.columns <- which(colnames(data) %in% items)
    #Match order of how items are specified and how they are found in the data
    match.columns <- match(items, colnames(data[, which(colnames(data) %in% items)]))

    #Calculate the item variances
    item.var <- apply(data[stats::complete.cases(data), scale.columns], 2, FUN="var")
    item.var <- item.var[match.columns]
    #Calculate the item SDs
    item.SD <- sqrt(item.var)
    #Calculate the item mean
    item.means <- colMeans(data[stats::complete.cases(data), scale.columns])
    item.means <- item.means[match.columns]

    #Calculate the sum of the item variances
    total.item.var <- sum(item.var)

    #Sum the row scores per case
    row.sum <- rowSums(data[stats::complete.cases(data), scale.columns])

    #Variance of the row sums per case
    total.var <- stats::var(row.sum)

    ## Calculate Cronbach's alpha ##
    #Number of items
    item.n <- length(items)
    alpha <- (item.n / (item.n - 1)) * ( (total.var - total.item.var) / total.var )

    ## Overall scale scores ##
    #Calculate the means
    Overall.Mean <- mean(rowMeans(data[stats::complete.cases(data), scale.columns]))
    #Variance
    Overall.Var <- stats::var(rowMeans(data[stats::complete.cases(data), scale.columns]))
    #Standard deviation
    Overall.SD <- stats::sd(rowMeans(data[stats::complete.cases(data), scale.columns]))
    #Make list of overall scale score statistics
    Scale.Statistics <- list("alpha"=alpha, "Overall.Mean"=Overall.Mean,
                             "Overall.Var"=Overall.Var, "Overall.SD"=Overall.SD,
                             "Items"=item.n)
    z <- list("alpha"=alpha, "total.var"=total.var,
              "item.means"=item.means, "item.var"=item.var, "item.SD"=item.SD,
              "Scale.Statistics"=Scale.Statistics)
    return(z)
  }

  Scale.Statistics <- talpha(items= items, data=data[stats::complete.cases(data), ])[["Scale.Statistics"]]
  #Item means, variances, SDs when items are deleted
  item.var <- talpha(items= items, data=data[stats::complete.cases(data), ])["item.var"]
  item.SD <- talpha(items= items, data=data[stats::complete.cases(data), ])["item.SD"]
  item.means <- talpha(items= items, data=data[stats::complete.cases(data), ])["item.means"]
  #Single object
  Item.Statistics <- list("item.means"=item.means, "item.var"=item.var, "item.SD"=item.SD)

  ## Alpha, mean, variance if each item is deleted ##
  alpha.item.deleted <- vector(mode = "list", length = length(items) )
  scale.var.item.deleted <- vector(mode = "list", length = length(items) )
  scale.SD.item.deleted <- vector(mode = "list", length = length(items) )
  scale.mean.item.deleted <- vector(mode = "list", length = length(items) )
  if (length(items) >= 3) {
    for (i in 1:length(items) ) {
      alpha.item.deleted[i] <- talpha(items= items[-i], data=data[stats::complete.cases(data), ])[["Scale.Statistics"]]["alpha"]
      scale.var.item.deleted[i] <- talpha(items= items[-i], data=data[stats::complete.cases(data), ])[["Scale.Statistics"]]["Overall.Var"]
      scale.SD.item.deleted[i] <- lapply((talpha(items= items[-i], data=data[stats::complete.cases(data), ])[["Scale.Statistics"]]["Overall.Var"]), FUN="sqrt")
      scale.mean.item.deleted[i] <- talpha(items= items[-i], data=data[stats::complete.cases(data), ])[["Scale.Statistics"]]["Overall.Mean"]
    }
    #Give names to list elements
    item_columns <- which(colnames(data) %in% items)
    for (i in 1:length(items) ) {
      names(alpha.item.deleted)[i] <- items[i]
      names(scale.var.item.deleted)[i] <- items[i]
      names(scale.SD.item.deleted)[i] <- items[i]
      names(scale.mean.item.deleted)[i] <- items[i]
    }
    #Item means, variances, SDs when items are deleted
    Item.Deleted <- list("alpha.item.deleted"=alpha.item.deleted,
                         "scale.mean.item.deleted"=scale.mean.item.deleted,
                         "scale.var.item.deleted"=scale.var.item.deleted,
                         "scale.SD.item.deleted" = scale.SD.item.deleted)
  } else {
    Item.Deleted <- NULL
  }

  #Descriptve info about the data
  Total.Rows <- nrow(data)
  Valid.Rows <- nrow(data[stats::complete.cases(data), ])
  Excluded.Rows <- Total.Rows - Valid.Rows
  #List of data info
  Sample.N <- list("Total" = Total.Rows, "Valid"=Valid.Rows, "Excluded"=Excluded.Rows)

  #Make list of all objects
  z <- list("Scale.Statistics"= Scale.Statistics,
            "Item.Statistics"= Item.Statistics,
            "Item.Deleted"= Item.Deleted,
            "Sample.N"= Sample.N,
            "Items"= items)
  class(z) <- c("alpha","ham", "list")
  return(z)
}
