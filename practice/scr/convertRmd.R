# install.packages("knitr")
library(knitr)

# purl Rmd
## documentation=0
purl(input="input/script.Rmd", output="output/script0.R", documentation=0)
## documentation=1(default)
purl(input="input/script.Rmd", output="output/script1.R", documentation=1)
## documentation=2
purl(input="input/script.Rmd", output="output/script2.R", documentation=2)
