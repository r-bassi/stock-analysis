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
    call(points,Value).

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
    parse_stock_sma(SmaData, Ticker, GlobalQuoteStats, Statistics).
%analyze_stock(Statistics, Result).
