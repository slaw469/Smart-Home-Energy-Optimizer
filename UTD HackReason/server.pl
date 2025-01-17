:- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

% Start the server
server :-
    http_server(http_dispatch, [port(8080)]).

% Define API endpoints
:- http_handler(root(.), root_handler, []).
:- http_handler(root(appliance_states), appliance_states_handler, []).
:- http_handler(root(optimize_hvac), optimize_hvac_handler, []).
:- http_handler(root(energy_saving), energy_saving_handler, []).

% Root handler to serve the HTML page
root_handler(_Request) :-
    open('index.html', read, Stream), % Open the HTML file
    read_string(Stream, _, HTML),    % Read the file as a string
    close(Stream),                   % Close the file stream
    format('Content-type: text/html~n~n'), % Set the correct HTTP header
    format('~s', [HTML]).            % Send the HTML content as response

% Handler to return appliance states
appliance_states_handler(_Request) :-
    findall(
        json{name: Name, state: State, room: Room},
        appliance(Name, State, _, _, Room),
        Appliances
    ),
    reply_json(Appliances).

% Optimize HVAC handler
optimize_hvac_handler(_Request) :-
    optimize_hvac(Action),
    reply_json(json{action: Action}).

% Energy-saving recommendation handler
energy_saving_handler(_Request) :-
    suggest_energy_saving(Saving),
    reply_json(json{saving: Saving}).

% Include your smart home code
:- consult('smart_home.pl').
