:- use_module(library(pce)).

main :-
    new(D, dialog('Stock Analyzer')),
    send(D, append, new(TickerText, text_item('Enter Stock Ticker'))),
    send(D, append, button('Analyze', message(@prolog, analyze, TickerText?selection))),
    send(D, append, button('Quit', message(D, destroy))),
    send(D, open).

analyze(Ticker) :-
    stock_analysis(Ticker, Statistics, _),
    get_points(Points),
    new(ResultDialog, dialog('Analysis Result')),
    format(string(Title), 'Analysis Result for ~w', [Ticker]),
    format(string(StatsLabel), 'Statistics: ~w', [Statistics]),
    send(ResultDialog, append, label(title, Title)),
    send(ResultDialog, append, label(stats, StatsLabel)),
    send(ResultDialog, append, label(points, string('Points: %d', Points))),
    send(ResultDialog, append, button('OK', message(ResultDialog, destroy))),
    send(ResultDialog, open).

:- main.
