#' @title ResIN_to_gephi
#'
#' @description Saves a ResIN graph as a series of csv files readable by Gephi. Source code taken from RMHogervorst / gephi
#'
#' @param ResIN_object the output of the ResIN function (a list with class ResIN).
#' @param file the name with .csv extension for the Gephi readable file to be output at. Defaults to "ResIN_gephi.csv".
#'
#' @return A series of csv files readable by Gephi
#'
#' @examples
#'
#' \dontrun{
#' ## Load the 12-item simulated Likert-type ResIN toy dataset
#' data(lik_data)
#'
#' ## Run the function:
#' ResIN_to_gephi(ResIN(lik_data), file = "ResIN_gephi.csv")
#' }
#'
#' @export
#' @importFrom readr "write_csv"
#' @references Source code was taken from: https://github.com/RMHogervorst/gephi?tab=MIT-1-ov-file#readme
#'

ResIN_to_gephi <- function(ResIN_object, file = "ResIN_gephi.csv") {
  ## Test for ResIN object
  if(class(ResIN_object)[2] !=  "ResIN"){
    stop("Please supply a ResIN type list object.")
  }
    dataframe <- ResIN_object$ResIN_edgelist
    df_names <- names(dataframe)

    from_ind <- grep("from", df_names)[[1]]
    df_names[from_ind] <- "Source"

    to_ind <- grep("to", df_names)[[1]]
    df_names[to_ind] <- "Target"

    names(dataframe) <- df_names

    dataframe <- dataframe[, 1:3]
    readr::write_csv(dataframe, file = file, na = "")
    return(invisible(dataframe))
  }

# MIT License
#
# Copyright (c) 2018 Roel M. Hogervorst
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
