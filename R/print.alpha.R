#' Print alpha results
#'
#' @param x alpha object from Cronbach's alpha calculation.
#'
#' @param ... Additional arguments.
#'
#' @return
#'
#' @examples
#' print(alpha(items=c("i1","i2","i3","i4","i5"), data=cas))
#'
#' @export
print.alpha <- function(x, ...) {
  Alpha <- x
  #Combine item statistics
  Item_stats <- data.frame(Alpha$Item.Statistics$item.means,
                           Alpha$Item.Statistics$item.var,
                           Alpha$Item.Statistics$item.SD)
  colnames(Item_stats) <- c("Mean", "Variance","Std. Dev." )
  row.names(Item_stats) <- Alpha$Items
  #Combine item statistics if item deleted
  if (length(Alpha$Items) >= 3) {
    Item_deleted <- data.frame(unlist(Alpha$Item.Deleted$alpha.item.deleted),
                               unlist(Alpha$Item.Deleted$scale.mean.item.deleted),
                               unlist(Alpha$Item.Deleted$scale.var.item.deleted),
                               unlist(Alpha$Item.Deleted$scale.SD.item.deleted))
    colnames(Item_deleted) <- c("Alpha","Mean", "Variance","Std. Dev." )
    row.names(Item_deleted) <- Alpha$Items
  }  else {
    Item_deleted <- "not calculated for 2 item scales."
  }
  cat("Scale statistics", "\n")
  cat("Cronbach\'s alpha   =", round(Alpha$Scale.Statistics$alpha, 3), "\n")
  cat("Mean               =", round(Alpha$Scale.Statistics$Overall.Mean, 3), "\n")
  cat("Variance           =", round(Alpha$Scale.Statistics$Overall.Var, 3), "\n")
  cat("Standard Deviation =", round(Alpha$Scale.Statistics$Overall.SD, 3), "\n")
  cat("Items              =", Alpha$Scale.Statistics$Items,"\n","\n")
  cat("Item statistics", "\n")
  show(round(Item_stats, 3))
  cat("\n")
  if (length(Alpha$Items) >= 3) {
    cat("Scale statistics if item deleted", "\n")
    show(round(Item_deleted, 3))
  } else {
    cat(paste("Scale statistics if item deleted", Item_deleted), "\n")
  }
  cat("\n")
  cat("Sample", "\n")
  cat("Total    =", Alpha$Sample.N$Total, "\n")
  cat("Valid    =", Alpha$Sample.N$Valid, "\n")
  cat("Excluded =", Alpha$Sample.N$Excluded, "\n")
}
