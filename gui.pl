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

% analyze the stock with the given TickerText, show analysis result
analyze(TickerText) :-
    get(TickerText, selection, Ticker),
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
