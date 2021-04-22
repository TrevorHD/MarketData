# Overview

An exploration of using machine learning for predictive modelling on stocks and ETFs, using the S&P 500 (GSPC) as an example. Includes methods such as support vector machines, discriminant analysis, decision trees, logistic regression, and more. Data for the NASDAQ Composite (IXIC) and Dow Jones Industrial Average (DJIA) are also included, though while not used in the analyses, can easily be accommodated using the existing code.

# Files

## Data


**SPWeekly** *(.csv)* - Weekly percent change for the S&P 500, calculated from the Yahoo Finance data [here](https://finance.yahoo.com/quote/%5EGSPC/history?p=%5EGSPC).

## Scripts

**YFanalysis** *(.Rmd)* Markdown for generating figures and rendering output to PDF.

**YFanalysis** *(.R)* Script for conducting analyses.

**YFData** *(.py)* - Script that extracts index data from Yahoo Finance, adds lag variables, and saves it as a CSV file.

**YFData** *(.R)* - Script that cleans the index data prior to analysis.

## Extras

*Note: these files are not used in the analysis, but the code can be easily extended to accommodate them.*

**DJDaily** *(.csv)* - Daily percent change for the Dow Jones Industrial Average, calculated from the Yahoo Finance data [here](https://finance.yahoo.com/quote/%5EDJI/history?p=%5EDJI).

**DJWeekly** *(.csv)* - Weekly percent change for the Dow Jones Industrial Average, calculated from the Yahoo Finance data [here](https://finance.yahoo.com/quote/%5EDJI/history?p=%5EDJI).

**NDDaily** *(.csv)* - Daily percent change for the NASDAQ Composite, calculated from the Yahoo Finance data [here](https://finance.yahoo.com/quote/%5EIXIC/history?p=%5EIXIC).

**NDWeekly** *(.csv)* - Weekly percent change for the NASDAQ Composite, calculated from the Yahoo Finance data [here](https://finance.yahoo.com/quote/%5EIXIC/history?p=%5EIXIC).

**SPDaily** *(.csv)* - Daily percent change for the S&P 500, calculated from the Yahoo Finance data [here](https://finance.yahoo.com/quote/%5EGSPC/history?p=%5EGSPC).
