# Overview

An exploration of using machine learning for predictive modelling on stocks and ETFs, using the S&P 500 (GSPC) as an example. Includes methods such as support vector machines, discriminant analysis, decision trees, logistic regression, and more. Data for the NASDAQ Composite (IXIC) and Dow Jones Industrial Average (DJIA) are also included, though while not used in the analyses, can easily be accommodated using the existing code.

***Note: due to shifting project priorities and a large number of other commitments, I have temporarily suspended work on this project and will likely resume work sometime in 2022.***

<br/>

# Disclaimer

Trading stocks, ETFs, options, futures, or any other financial assets or derivatives has inherent risks that should be understood before making any substantial investments; such risks can include significant loss of principal, and it is the investor's responsibility to determine their appropriate level of exposure to risk. The contents of this work are not, in any form, meant to serve as investment advice, but rather as exploratory analysis of stock market data with the goal being able to predict changes in future conditions based solely off of conditions at a prior date. As such, the author bears no liability for any damages or losses, realised or unrealised, incurred by the reader should they choose to use the models in these analyses for any sort of investment guidance.

<br/>

# Files

## Data

**SPWeekly** *(.csv)* - Weekly percent change and volume for the S&P 500, calculated after using Python to scrape the Yahoo Finance data [here](https://finance.yahoo.com/quote/%5EGSPC/history?p=%5EGSPC).

## Scripts

**YFAnalysis** *(.R)* Script for conducting analyses.

**YFData** *(.py)* - Script that extracts index data from Yahoo Finance, adds lag variables, and saves it as a CSV file.

**YFData** *(.R)* - Script that cleans the index data prior to analysis.

**YFAnalysis** *(.Rmd)* Markdown for generating figures and rendering output to PDF.

## Other

**YFAnalysis** *(.pdf)* PDF report of the analyses.

**Header** *(.tex)* A TeX file with header specifications.

## Extras

*Note: these files are not used in the analysis, but the code can be easily extended to accommodate them.*

**DJDaily** *(.csv)* - Daily percent change and volume for the Dow Jones Industrial Average, calculated after using Python to scrape the Yahoo Finance data [here](https://finance.yahoo.com/quote/%5EDJI/history?p=%5EDJI).

**DJWeekly** *(.csv)* - Weekly percent change and volume for the Dow Jones Industrial Average, calculated after using Python to scrape the Yahoo Finance data [here](https://finance.yahoo.com/quote/%5EDJI/history?p=%5EDJI).

**NDDaily** *(.csv)* - Daily percent change and volume for the NASDAQ Composite, calculated after using Python to scrape the Yahoo Finance data [here](https://finance.yahoo.com/quote/%5EIXIC/history?p=%5EIXIC).

**NDWeekly** *(.csv)* - Weekly percent change and volume for the NASDAQ Composite, calculated after using Python to scrape the Yahoo Finance data [here](https://finance.yahoo.com/quote/%5EIXIC/history?p=%5EIXIC).

**SPDaily** *(.csv)* - Daily percent change and volume for the S&P 500, calculated after using Python to scrape the Yahoo Finance data [here](https://finance.yahoo.com/quote/%5EGSPC/history?p=%5EGSPC).
