# MISTR
Implementation of Ontario's Modelling and Inventory Support Tool (MIST). 
A set of models used for growth and yield analysis.


## Installation 
```
install.packages("devtools")
devtools::install_github("ptompalski/MISTR")
library(MISTR)
```

## Example usage
```
StandAttributes(SI = 20, age = 30, SppId = "AW")
StandAttributes(SI = 20, age = 10:50, SppId = "PR")

```
