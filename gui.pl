:- use_module(library(pce)).
:- use_module(analyzestock).

% initialize and open main dialog window
main :-
    new(D, dialog('Stock Analyzer')),
    send(D, append, new(TickerText, text_item('Enter Stock Ticker'))),
    send(D, append, button('Analyze', message(@prolog, analyze, TickerText))),
    send(D, append, button('Show Statistics', message(@prolog, show_statistics, TickerText))),
    send(D, append, button('Quit', message(D, destroy))),
    send(D, open).

% format string for analysis statistics
statistics_string(Statistics, StatsString) :-
    format(string(StatsString),
    'Ticker: ~w\nPrice: ~w\nPrevious Close: ~w\nChange Percent: ~w\nSMA: ~w\nPE Ratio: ~w\n52 Week High: ~w\n52 Week Low: ~w\n',
    [Statistics.ticker, Statistics.price, Statistics.prev_close, Statistics.change_percent, Statistics.sma, Statistics.peRatio, Statistics.week52High, Statistics.week52Low]).

% analyze the stock with the given TickerText, show analysis result
analyze(TickerText) :-
    get(TickerText, selection, Ticker),
    stock_analysis(Ticker, Statistics, Result),
    new(ResultDialog, dialog('Analysis Result')),
    format(string(Title), 'Analysis Result for ~w', [Ticker]),
    statistics_string(Statistics, StatsString),
    split_string(StatsString, "\n", "", StatsList),
    send(ResultDialog, append, label(title, Title)),
    forall(member(Line, StatsList), (
        % gensym to generate unique atom for each label
        gensym(label, LabelName),
        send(ResultDialog, append, label(LabelName, Line))
    )),
    send(ResultDialog, append, label(result, string('Result: %s', Result))),
    send(ResultDialog, append, button('OK', message(ResultDialog, destroy))),
    send(ResultDialog, open).

% display stock overview information for the given TickerText
show_statistics(TickerText) :-
    get(TickerText, selection, Ticker),
    new(StatsDialog, dialog('Stock Statistics')),
    get_stock_overview(Ticker, Data),
    output_overview_information(Data, FormattedOutput),
    split_string(FormattedOutput, "\n", "", StatsList),
    forall(member(Line, StatsList), (
        % gensym to generate unique atom for each label
        gensym(label, LabelName),
        send(StatsDialog, append, label(LabelName, Line))
    )),
    send(StatsDialog, append, button('OK', message(StatsDialog, destroy))),
    send(StatsDialog, open).


:- main.
