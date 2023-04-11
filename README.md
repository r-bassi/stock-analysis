
# Stock-Analysis

# Introduction

This is a simple application that provides a user with a buy/sell recommendation for any given ticker symbol (**This program is NOT financial advice! We take no responsibility for your usage of this program to make financial decisions.**).

# Algorithm

We're using the Alpha Vantage API to get market data. Using this data, we've identified certain key metrics and generate a score for each ticker symbol. A positive symbol results in a buy rating and vice versa.

# Usage

This assumes that SWI-Prolog is properly installed on your device, and contains the working XPCE library. (Note: XPCE may not work on Apple Silicon).

First, clone the repository:

    $ git clone https://github.com/r-bassi/stock-analysis.git

Then, navigate to the project directory:

    $ cd stock-analysis

Run SWI-Prolog:
    
    $ swipl
    
Build and run the project:
    
    $ [analyzestock].

    $ [gui].


