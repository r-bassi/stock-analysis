:- consult(config).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).


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
    Stats = stock_statistics{
        ticker: Ticker,
        price: Price,
        prev_close: PrevClose,
        change_percent: ChangePercent
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
    parse_stock_data(Data, Ticker, Statistics),
    analyze_stock(Statistics, Result).
