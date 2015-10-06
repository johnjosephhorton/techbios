# techbios

## Installation

This should work: 
    library(devtools)
    install_github("johnjosephhorton/techbios")
    
Certain SQL-related files need to be installed. 
On Arch Linux, 

    pacman -S postgresql-libs 

## How it works

In the '/data-raw folder, there is an SQL database of parsed LinkedIn resumes. The file 'main.R in that folder analyzes this data and writes R objects to '/data which are then used by the R functions described in R. 