% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_problem.R
\name{create_problem}
\alias{create_problem}
\title{Function to create a large dataset for the Knapsack problem}
\usage{
create_problem(nmax)
}
\arguments{
\item{nmax}{size of the dataset}
}
\value{
a data.frame of size nmax
}
\description{
Function to create a large dataset for the Knapsack problem
}
\examples{
x <- create_problem(2000)
print(head(x))
}
