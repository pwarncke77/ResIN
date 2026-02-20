# Convert ResIN networks to a Gephi-readable csv tables

Saves a ResIN graph as a series of csv files readable by Gephi. Source
code taken from RMHogervorst / gephi

## Usage

``` r
ResIN_to_gephi(ResIN_object, file = "ResIN_gephi.csv")
```

## Arguments

- ResIN_object:

  the output of the ResIN function (a list with class ResIN).

- file:

  the name with .csv extension for the Gephi readable file to be output
  at. Defaults to "ResIN_gephi.csv".

## Value

A series of csv files readable by Gephi

## References

Source code was taken from:
https://github.com/RMHogervorst/gephi?tab=MIT-1-ov-file#readme

## Examples

``` r
if (FALSE) { # \dontrun{
## Load the 12-item simulated Likert-type ResIN toy dataset
data(lik_data)

## Run the function:
ResIN_to_gephi(ResIN(lik_data), file = "ResIN_gephi.csv")
} # }
```
