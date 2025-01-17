:- module(smart_home, [
    appliance/5,
    weather/3,
    prefers_temperature/1,
    prefers_humidity/1,
    home_occupied/1,
    time_of_day/1,
    energy_price/2,
    peak_hours/2,
    current_energy_price/1,
    optimize_hvac/1,
    should_turn_off/1,
    should_turn_off_during_peak/1,
    should_remain_on/1,
    schedule_appliance_use/2,
    total_energy_consumption/1,
    estimated_energy_cost/1,
    suggest_energy_saving/1,
    room_appliance_usage/2,
    suggest_turn_off_lights/1
]).

:- use_module(library(lists)).  % Ensure the lists library is included.

% Appliance states: appliance(Name, State, EnergyUsage, Essential, Room).
:- dynamic appliance/5.

appliance(tv, off, 150, no, living_room).
appliance(fridge, on, 200, yes, kitchen).
appliance(light, on, 50, no, bedroom).
appliance(oven, off, 1000, no, kitchen).
appliance(heater, off, 1500, no, bathroom).
appliance(washing_machine, off, 500, no, laundry_room).
appliance(computer, on, 300, no, office).

% Weather information: weather(Season, Temperature, Humidity).
:- dynamic weather/3.

weather(summer, 90, 70).

% User preferences
:- dynamic prefers_temperature/1, prefers_humidity/1.

prefers_temperature(72).
prefers_humidity(50).

% Home occupancy
:- dynamic home_occupied/1.

home_occupied(no).

% Time of day
:- dynamic time_of_day/1.

time_of_day(14).

% Energy pricing
:- dynamic energy_price/2.

energy_price(peak, 0.20).
energy_price(off_peak, 0.10).

% Peak hours
:- dynamic peak_hours/2.

peak_hours(17, 21).

% Determine current energy price
current_energy_price(Price) :-
    time_of_day(Hour),
    peak_hours(Start, End),
    (Hour >= Start, Hour < End ->
        energy_price(peak, Price)
    ;
        energy_price(off_peak, Price)).

% Optimize HVAC usage
optimize_hvac(Action) :-
    weather(summer, Temp, _),
    prefers_temperature(PrefTemp),
    (Temp > PrefTemp + 5 ->
        Action = 'Turn on air conditioning'
    ;
     Temp < PrefTemp - 5 ->
        Action = 'Turn on heating'
    ;
        Action = 'HVAC off').

% Recommend appliances to turn off when the home is unoccupied
should_turn_off(Appliance) :-
    appliance(Appliance, on, _, no, _),
    home_occupied(no).

% Total energy consumption
total_energy_consumption(Total) :-
    findall(Energy, appliance(_, on, Energy, _, _), Energies),
    sumlist(Energies, Total).  % Use `sumlist` instead of `sum_list`.

% Estimate energy cost
estimated_energy_cost(Cost) :-
    total_energy_consumption(TotalKWh),
    current_energy_price(PricePerKWh),
    Cost is TotalKWh * PricePerKWh.

% Suggest energy-saving actions
suggest_energy_saving(Action) :-
    total_energy_consumption(Total),
    (Total > 2000 ->
        Action = 'Consider turning off non-essential appliances to reduce consumption'
    ;
        Action = 'Energy consumption is within optimal range').

% Suggest turning off lights in unoccupied rooms
suggest_turn_off_lights(Room) :-
    appliance(light, on, _, _, Room),
    home_occupied(no).
% User input for electricity calculation
:- dynamic user_input/2.

% Reset user inputs (optional for repeated use)
reset_user_inputs :-
    retractall(user_input(_, _)).

% Store user input for a specific appliance or factor
store_user_input(Name, Value) :-
    retractall(user_input(Name, _)), % Remove existing input for this appliance
    assertz(user_input(Name, Value)).

% Calculate total electricity usage based on user inputs
calculate_usage(TotalKWh) :-
    findall(Value, user_input(_, Value), Values),
    sumlist(Values, TotalKWh).

% Suggest kWh savings based on user input
suggest_kwh_saving(SavingKWh) :-
    calculate_usage(TotalKWh),
    (TotalKWh > 200 ->
        SavingKWh is TotalKWh * 0.2; % Assume 20% saving potential
        SavingKWh is 0).

:- dynamic appliance/5.

% Appliance states: appliance(Name, State, EnergyUsage, Essential, Room).
appliance(tv, off, 150, no, living_room).
appliance(fridge, on, 200, yes, kitchen).
appliance(light, on, 50, no, bedroom).
appliance(oven, off, 1000, no, kitchen).
appliance(heater, off, 1500, no, bathroom).
appliance(washing_machine, off, 500, no, laundry_room).
appliance(computer, on, 300, no, office).
