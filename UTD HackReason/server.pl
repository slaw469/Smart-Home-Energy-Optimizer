:- use_module(library(http/http_server)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

% Start the server
server :-
    http_server(http_dispatch, [port(8080)]).

:- http_handler(root(store_input), store_input_handler, []).

% Define API endpoints
:- http_handler(root(.), root_handler, []).
:- http_handler(root(appliance_states), appliance_states_handler, []).
:- http_handler(root(toggle_appliance), toggle_appliance_handler, []).
:- http_handler(root(energy_saving), energy_saving_handler, []).

% Root handler to serve the HTML page
root_handler(_Request) :-
    open('index.html', read, Stream),
    read_string(Stream, _, HTML),
    close(Stream),
    format('Content-type: text/html~n~n'),
    format('~s', [HTML]).


% Handler to return appliance states
appliance_states_handler(_Request) :-
    findall(
        json{name: Name, state: State, room: Room},
        appliance(Name, State, _, _, Room),
        Appliances
    ),
    reply_json(Appliances).


toggle_appliance_handler(Request) :-
    http_read_json_dict(Request, Dict),
    Name = Dict.get(name),
    NewState = Dict.get(state),
    atom_string(ApplianceName, Name),
    atom_string(State, NewState),
    (   retract(appliance(ApplianceName, _, Usage, Essential, Room)) ->
        assertz(appliance(ApplianceName, State, Usage, Essential, Room)),
        reply_json(json{status: "success", message: "State toggled successfully", new_state: State})
    ;   reply_json(json{status: "error", message: "Appliance not found"}, [status(404)])
    ).





% Energy-saving recommendation handler
energy_saving_handler(_Request) :-
    suggest_energy_saving(Saving),
    reply_json(json{saving: Saving}).

% Include your smart home code
:- consult('smart_home.pl').

store_input_handler(Request) :-
    % Read the JSON payload from the request
    http_read_json_dict(Request, Dict),
    % Extract the name and value from the payload
    Name = Dict.get(name),
    Value = Dict.get(value),
    atom_string(ApplianceName, Name),
    atom_string(State, Value),
    % Update the appliance state in the database
    (   retract(appliance(ApplianceName, _, Usage, Essential, Room)) ->
        assertz(appliance(ApplianceName, State, Usage, Essential, Room)),
        reply_json(json{status: "success", message: "Appliance state updated"})
    ;   reply_json(json{status: "error", message: "Appliance not found"}, [status(404)])
    ).

    
    % Check if the appliance exists and toggle its state
    (   retract(appliance(ApplianceName, CurrentState, Usage, Essential, Room)) ->
        % Toggle the state
        (   ToggleState = "on" ->
            UpdatedState = "off"
        ;   UpdatedState = "on"
        ),
        % Update the appliance with the new state
        assertz(appliance(ApplianceName, UpdatedState, Usage, Essential, Room)),
        % Send success response
        reply_json(json{status: "success", message: "State toggled successfully", new_state: UpdatedState})
    ;   % If appliance not found, return an error
        reply_json(json{status: "error", message: "Appliance not found"}, [status(404)])
    ).

