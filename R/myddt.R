#' @name myddt
#' @title A function to retrieve information about DDT dataset
#'
#' @description Returns a plot with length vs weight for species and saves the data frame used in the function to the working directory as LvsWforSPECIES.csv where the "SPECIES" string is replaces with the species used. Also prints to the command line a named list containing the DDT data before subsetting, the subsetted data frame, and a relative frequency table of the river before subsetting.
#'
#' @details A function for Project 1 in MATH 4753
#'
#' @param df the data frame to be subsetted, which is ddt for this functions purposes
#' @param SPECIES the string of species to be to be plotted and subsetted
#'
#' @importFrom utils data read.table
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_smooth
#' @importFrom ggplot2 ggtitle
#' @importFrom ggplot2 ggplot
#' @importFrom utils write.csv
#'
#' @return plot with length vs weight for species and saves the data frame used in the function to the working directory as LvsWforSPECIES.csv where the "SPECIES" string is replaces with the species used. Also prints to the command line a named list containing the DDT data before subsetting, the subsetted data frame, and a relative frequency table of the river before subsetting.
#'
#' @examples
#' \dontrun{myddt(df=ddt,SPECIES = "CCATFISH")
#' }
library(dplyr)
library(ggplot2)

myddt <- function(df, SPECIES){
  RIVER <-  WEIGHT <- LENGTH <- NULL
  # dataframe before subsetting:
  # list = as.list(ddt)
  # print(list)
  print(df)
  # subsetting the dataframe based on species
  df1 <- df %>% filter(SPECIES == {{SPECIES}})
  # plotting using ggplot2
  g <- ggplot(df1, aes_string(x="LENGTH",y="WEIGHT")) + # Note the use of aes_string
    geom_point(aes_string(color = "RIVER" )) +
    geom_smooth(formula = y~x +I(x^2), method = "lm") +
    ggtitle("Mark Cunningham")
  print(g)
  print(df1)
  # dataframe after subsetting:
  write.csv(df1, paste("LvsWfor",SPECIES,".CSV",sep=""))
  # Table for relative frequency of Rivers:
  rf_river=with(df,round((table(RIVER)/length(RIVER)),3))
  rf_river
}
