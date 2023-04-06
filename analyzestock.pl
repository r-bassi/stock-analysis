:- consult(config).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).

:- dynamic points/0.

% set the value of the global variable
set_points(Value) :-
    retractall(points(_)),
    assertz(points(Value)).

% get the value of the global variable
get_points(Value) :-
    points(Exp),
    Value is Exp.

% Make an API request and get stock data
get_stock_data(Ticker, Data) :-
    api_key(APIKey),
    format(atom(StockDataURL), 'https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=~w&apikey=~w', [Ticker, APIKey]),
    setup_call_cleanup(
        http_open(StockDataURL, In, []),
        json_read_dict(In, Data),
        close(In)
    ).

% Parse stock data and return the relevant stats
parse_stock_data(Data, Ticker, Stats) :-
    get_dict('Global Quote', Data, Quote),
    get_dict('05. price', Quote, PriceStr),
    get_dict('08. previous close', Quote, PrevCloseStr),
    get_dict('10. change percent', Quote, ChangePercentStr),
    atom_number(PriceStr, Price),
    atom_number(PrevCloseStr, PrevClose),
    sub_atom(ChangePercentStr, 0, _, 1, ChangePercentNoPercent),
    atom_number(ChangePercentNoPercent, ChangePercent),
    set_points(0),
    Stats = stock_statistics{
        ticker: Ticker,
        price: Price,
        prev_close: PrevClose,
        change_percent: ChangePercent
    }.

get_stock_sma(Ticker, SmaData) :-
    api_key(APIKey),
    format(atom(StockDataURL), 'https://www.alphavantage.co/query?function=SMA&symbol=~w&interval=weekly&time_period=10&series_type=open&apikey=~w', [Ticker, APIKey]),
    setup_call_cleanup(
        http_open(StockDataURL, In, []),
        json_read_dict(In, SmaData),
        close(In)
    ).

parse_stock_sma(SmaData, Ticker, GlobalQuoteStats, Stats) :- 
    get_dict('Technical Analysis: SMA', SmaData, Smas), 
    get_dict('2023-03-31', Smas, SmaDict),
    get_dict('SMA', SmaDict, SmaStr),
    atom_number(SmaStr, Sma),
    get_points(Value),
    (Sma < GlobalQuoteStats.price -> set_points(Value + 1); set_points(Value - 1)),
    Stats = stock_statistics{
        sma: Sma
    }.

get_stock_overview(Ticker, Data) :-
    format(atom(StockDataURL), 'https://www.alphavantage.co/query?function=OVERVIEW&symbol=~w&apikey=~w', [Ticker, APIKey]),
    setup_call_cleanup(
        http_open(StockDataURL, In, []),
        json_read_dict(In, Data),
        close(In)
    ).

% Calculate the PE ratio
parse_pe_ratio(Ticker, Stats) :-
    get_stock_overview(Ticker, Data),
    get_dict('PERatio', Data, PERatioStr),
    atom_number(PERatioStr, PE),
    get_points(Value),
    (PE < 23 -> set_points(Value + 1); set_points(Value - 1)),
    Stats = stock_statistics{
        peRatio: PE
    }.

% Calculate the 52 week change
parse_week52_data(Ticker, Stats) :-
    get_stock_overview(Ticker, Data),
    get_dict('52WeekHigh', Data, Week52High),
    get_dict('52WeekLow', Data, Week52Low),
    atom_number(Week52High, Week52HighInt),
    atom_number(Week52Low, Week52LowInt),
    get_points(Value),
    (Week52HighInt - Week52LowInt > 0 -> set_points(Value + 1); set_points(Value - 1)),
    Stats = stock_statistics{
        week52High: Week52HighInt,
        week52Low: Week52LowInt
    }.


% Output the entire stock overview information
output_overview_information(Data) :-
    get_dict('Symbol', Data, Symbol),
    get_dict('AssetType', Data, AssetType),
    get_dict('Name', Data, Name),
    get_dict('Description', Data, Description),
    get_dict('CIK', Data, CIK),
    get_dict('Exchange', Data, Exchange),
    get_dict('Currency', Data, Currency),
    get_dict('Country', Data, Country),
    get_dict('Sector', Data, Sector),
    get_dict('Industry', Data, Industry),
    get_dict('Address', Data, Address),
    get_dict('FiscalYearEnd', Data, FiscalYearEnd),
    get_dict('LatestQuarter', Data, LatestQuarter),
    get_dict('MarketCapitalization', Data, MarketCapitalization),
    get_dict('EBITDA', Data, EBITDA),
    get_dict('PERatio', Data, PERatio),
    get_dict('PEGRatio', Data, PEGRatio),
    get_dict('BookValue', Data, BookValue),
    get_dict('DividendPerShare', Data, DividendPerShare),
    get_dict('DividendYield', Data, DividendYield),
    get_dict('EPS', Data, EPS),
    get_dict('RevenuePerShareTTM', Data, RevenuePerShareTTM),
    get_dict('ProfitMargin', Data, ProfitMargin),
    get_dict('OperatingMarginTTM', Data, OperatingMarginTTM),
    get_dict('ReturnOnAssetsTTM', Data, ReturnOnAssetsTTM),
    get_dict('ReturnOnEquityTTM', Data, ReturnOnEquityTTM),
    get_dict('RevenueTTM', Data, RevenueTTM),
    get_dict('GrossProfitTTM', Data, GrossProfitTTM),
    get_dict('DilutedEPSTTM', Data, DilutedEPSTTM),
    get_dict('QuarterlyEarningsGrowthYOY', Data, QuarterlyEarningsGrowthYOY),
    get_dict('QuarterlyRevenueGrowthYOY', Data, QuarterlyRevenueGrowthYOY),
    get_dict('AnalystTargetPrice', Data, AnalystTargetPrice),
    get_dict('TrailingPE', Data, TrailingPE),
    get_dict('ForwardPE', Data, ForwardPE),
    get_dict('PriceToSalesRatioTTM', Data, PriceToSalesRatioTTM),
    get_dict('PriceToBookRatio', Data, PriceToBookRatio),
    get_dict('EVToRevenue', Data, EVToRevenue),
    get_dict('EVToEBITDA', Data, EVToEBITDA),
    get_dict('Beta', Data, Beta),
    get_dict('52WeekHigh', Data, Week52High),
    get_dict('52WeekLow', Data, Week52Low),
    get_dict('50DayMovingAverage', Data, Day50MovingAverage),
    get_dict('200DayMovingAverage', Data, Day200MovingAverage),
    get_dict('SharesOutstanding', Data, SharesOutstanding),
    get_dict('DividendDate', Data, DividendDate),
    get_dict('ExDividendDate', Data, ExDividendDate),
    format('Currency: ~w~nCountry: ~w~nSector: ~w~nIndustry: ~w~nAddress: ~w~nFiscalYearEnd: ~w~nLatestQuarter: ~w~nMarketCapitalization: ~w~nEBITDA: ~w~nPERatio: ~w~nPEGRatio: ~w~nBookValue: ~w~nDividendPerShare: ~w~nDividendYield: ~w~nEPS: ~w~nRevenuePerShareTTM: ~w~nProfitMargin: ~w~nOperatingMarginTTM: ~w~nReturnOnAssetsTTM: ~w~nReturnOnEquityTTM: ~w~nRevenueTTM: ~w~nGrossProfitTTM: ~w~nDilutedEPSTTM: ~w~nQuarterlyEarningsGrowthYOY: ~w~nQuarterlyRevenueGrowthYOY: ~w~nAnalystTargetPrice: ~w~nTrailingPE: ~w~nForwardPE: ~w~nPriceToSalesRatioTTM: ~w~nPriceToBookRatio: ~w~nEVToRevenue: ~w~nEVToEBITDA: ~w~nBeta: ~w~n52WeekHigh: ~w~n52WeekLow: ~w~n50DayMovingAverage: ~w~n200DayMovingAverage: ~w~nSharesOutstanding: ~w~nDividendDate: ~w~nExDividendDate: ~w~n', 
    [Currency, Country, Sector, Industry, Address, FiscalYearEnd, LatestQuarter, 
    MarketCapitalization, EBITDA, PERatio, PEGRatio, BookValue, DividendPerShare, 
    DividendYield, EPS, RevenuePerShareTTM, ProfitMargin, OperatingMarginTTM, 
    ReturnOnAssetsTTM, ReturnOnEquityTTM, RevenueTTM, GrossProfitTTM, DilutedEPSTTM, 
    QuarterlyEarningsGrowthYOY, QuarterlyRevenueGrowthYOY, AnalystTargetPrice, TrailingPE, 
    ForwardPE, PriceToSalesRatioTTM, PriceToBookRatio, EVToRevenue, EVToEBITDA, Beta, Week52High, 
    Week52Low, Day50MovingAverage, Day200MovingAverage, SharesOutstanding, DividendDate, 
    ExDividendDate]).


% Analyze the stock to determine if it's a good buy
analyze_stock(Stats, Result) :-
    Stats.price > 0,
    Stats.change_percent > 0,
    !,
    Result = 'Good Buy'.

analyze_stock(_, 'Not a Good Buy').


stock_analysis(Ticker, Statistics, Result) :-
    get_stock_data(Ticker, Data),
    parse_stock_data(Data, Ticker, GlobalQuoteStats),
    get_stock_sma(Ticker, SmaData),
    parse_stock_sma(SmaData, Ticker, GlobalQuoteStats, SmaStats),
    get_stock_overview(Ticker, OverviewData),
    output_overview_information(OverviewData),
    parse_pe_ratio(Ticker, PERatioStats),
    parse_week52_data(Ticker, Week52Stats),
    Statistics = GlobalQuoteStats.put(SmaStats).put(PERatioStats).put(Week52Stats).
%analyze_stock(Statistics, Result).