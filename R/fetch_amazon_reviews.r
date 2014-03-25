#' Fetch the amazon reviews by product id.
#'
#' @param id integer. The product ID as in the URL, for example,
#'   \code{http://www.amazon.com/R-Graphics-Cookbook-Winston-Chang/product-reviews/1449316956}
#'   would give an ID of 1449316956.
#' @return data.frame containing the reviews with names
#'   helpful_count, helpful_total, stars, title, date, description
#' @export
#' @updates
#'   Current URL work for any product and don't show errors when there is no reviews for the product
fetch_amazon_reviews <- function (id, page = 1) 
{
  library(RCurl)
  library(XML)
  curl <- getCurlHandle(useragent = "R", followlocation = TRUE)
  URL <- paste0("http://www.amazon.com/product-reviews/", id)
  
  if (page > 1) 
    URL <- paste0(URL, "/cm_cr_pr_top_link?pageNumber=",page)
  raw.html <- getURL(URL, curl = curl)
  parsed.html <- htmlParse(raw.html)
  
  if ((page == 1) && (length(xpathSApply(parsed.html, "//div[@class='CMpaginate']"))>1)) {
    pages <- as.vector(xpathSApply(parsed.html, "//div[@class='CMpaginate']"))[[1]]
    page_count <- max(unique(na.omit(as.numeric(unlist(strsplit(gsub("[^/ 0-9]", '', xmlValue(pages[[2]])),split=" "))))))
    more_reviews <- lapply(seq_len(page_count) , function(page_num) fetch_amazon_reviews(id,page = page_num))
  }
  else more_reviews <- NULL
  
  reviews <- as.vector(xpathSApply(parsed.html, "//table[@id=\"productReviews\"]/tr/td/div"))
  reviews_df <- do.call(rbind, lapply(reviews, review_to_df))
  do.call(rbind, append(more_reviews, list(reviews_df)))
}

