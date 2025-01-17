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

:- use_module(library(lists)).

% ---------------------------------------------------------------------------
% APPLIANCE DEFINITIONS
% ---------------------------------------------------------------------------
% appliance(Name, State, EnergyUsage, Essential, Room).
:- dynamic appliance/5.

appliance(tv, off, 150, no, living_room).
appliance(fridge, on, 200, yes, kitchen).
appliance(light, on, 50, no, bedroom).
appliance(oven, off, 1000, no, kitchen).
appliance(heater, off, 1500, no, bathroom).
appliance(washing_machine, off, 500, no, laundry_room).
appliance(computer, on, 300, no, office).

% ---------------------------------------------------------------------------
% WEATHER AND USER PREFERENCES
% ---------------------------------------------------------------------------
:- dynamic weather/3.

weather(summer, 90, 70).

:- dynamic prefers_temperature/1, prefers_humidity/1.

prefers_temperature(72).
prefers_humidity(50).

% ---------------------------------------------------------------------------
% HOME OCCUPANCY AND TIME
% ---------------------------------------------------------------------------
:- dynamic home_occupied/1.

home_occupied(no).

:- dynamic time_of_day/1.

time_of_day(14).

% ---------------------------------------------------------------------------
% ENERGY PRICING
% ---------------------------------------------------------------------------
:- dynamic energy_price/2.

energy_price(peak, 0.20).
energy_price(off_peak, 0.10).

% Peak hours
:- dynamic peak_hours/2.

peak_hours(17, 21).

% ---------------------------------------------------------------------------
% DETERMINE CURRENT ENERGY PRICE
% ---------------------------------------------------------------------------
current_energy_price(Price) :-
    time_of_day(Hour),
    peak_hours(Start, End),
    ( Hour >= Start, Hour < End ->
        energy_price(peak, Price)
    ;   energy_price(off_peak, Price)
    ).

% ---------------------------------------------------------------------------
% HVAC OPTIMIZATION
% ---------------------------------------------------------------------------
optimize_hvac(Action) :-
    weather(summer, Temp, _Humidity),
    prefers_temperature(PrefTemp),
    ( Temp > PrefTemp + 5 ->
        Action = 'Turn on air conditioning'
    ; Temp < PrefTemp - 5 ->
        Action = 'Turn on heating'
    ;
        Action = 'HVAC off'
    ).

% ---------------------------------------------------------------------------
% RECOMMEND TURNING OFF APPLIANCES WHEN UNOCCUPIED
% ---------------------------------------------------------------------------
% should_turn_off(Appliance) is true if Appliance is ON, 
% non-essential, and home is unoccupied.
should_turn_off(Appliance) :-
    appliance(Appliance, on, _Energy, no, _Room),
    home_occupied(no).

% ---------------------------------------------------------------------------
% TOTAL ENERGY CONSUMPTION
% ---------------------------------------------------------------------------
total_energy_consumption(Total) :-
    findall(Energy, appliance(_Name, on, Energy, _Essential, _Room), Energies),
    sumlist(Energies, Total).

% ---------------------------------------------------------------------------
% ESTIMATE ENERGY COST
% ---------------------------------------------------------------------------
estimated_energy_cost(Cost) :-
    total_energy_consumption(TotalKWh),
    current_energy_price(PricePerKWh),
    Cost is TotalKWh * PricePerKWh.

% ---------------------------------------------------------------------------
% SUGGEST TURNING OFF LIGHTS IN UNOCCUPIED ROOMS
% (One simple example: if the house is unoccupied, 
%  we might suggest turning off lights, or you can refine this further.)
% ---------------------------------------------------------------------------
suggest_turn_off_lights(Room) :-
    appliance(light, on, _Energy, _Essential, Room),
    home_occupied(no).

% ---------------------------------------------------------------------------
% SCHEDULE HIGH-ENERGY APPLIANCES FOR OFF-PEAK USAGE
% ---------------------------------------------------------------------------
schedule_appliance_use(Appliance, 'off-peak hours') :-
    appliance(Appliance, off, Energy, _Essential, _Room),
    Energy > 200,  % Only schedule high-energy appliances
    current_energy_price(Price),
    % If we see the price is peak or high, wait for off-peak
    Price >= 0.20.

% ---------------------------------------------------------------------------
% RECOMMEND APPLIANCES TO TURN OFF DURING PEAK HOURS
% ---------------------------------------------------------------------------
should_turn_off_during_peak(Appliance) :-
    appliance(Appliance, on, _Energy, no, _Room),
    current_energy_price(Price),
    Price > 0.15.  % Example threshold

% ---------------------------------------------------------------------------
% RECOMMEND ESSENTIAL APPLIANCES REMAIN ON
% ---------------------------------------------------------------------------
should_remain_on(Appliance) :-
    appliance(Appliance, _State, _Energy, yes, _Room).

% ---------------------------------------------------------------------------
% SUGGEST ENERGY-SAVING ACTIONS (MAIN ENTRY POINT FOR /energy_saving)
% ---------------------------------------------------------------------------
suggest_energy_saving(FullRecommendation) :-
    total_energy_consumption(Total),
    findall(A, should_turn_off(A), UnoccupiedOff),
    findall(A, should_turn_off_during_peak(A), PeakOff),
    findall(Room, suggest_turn_off_lights(Room), LightsOffRooms),
    findall(Appliance, schedule_appliance_use(Appliance, 'off-peak hours'), Scheduled),
    
    % If total usage is above 2000, highlight that we’re consuming a lot
    (   Total > 2000
    ->  OverConsumptionMsg = 'Your total energy consumption is high. Consider turning off non-essential appliances.'
    ;   OverConsumptionMsg = 'Your energy consumption is within an acceptable range.'
    ),

    % Build partial advice strings
    build_unoccupied_msg(UnoccupiedOff, UnoccupiedMsg),
    build_peak_off_msg(PeakOff, PeakOffMsg),
    build_lights_off_msg(LightsOffRooms, LightsOffMsg),
    build_scheduling_msg(Scheduled, SchedulingMsg),

    % Combine them all into one single recommendation text
    atomic_list_concat(
      [
        OverConsumptionMsg,
        UnoccupiedMsg,
        PeakOffMsg,
        LightsOffMsg,
        SchedulingMsg
      ],
      ' ',
      FullRecommendation
    ).

% Helper to advise turning off non-essential appliances if unoccupied
build_unoccupied_msg([], '').
build_unoccupied_msg(List, Msg) :-
    List \= [],
    format(atom(Msg),
      'Since the home is unoccupied, you could turn off these non-essential appliances: ~w.',
      [List]).

% Helper to advise turning off during peak
build_peak_off_msg([], '').
build_peak_off_msg(List, Msg) :-
    List \= [],
    format(atom(Msg),
      'It is currently peak hours, consider turning off these appliances: ~w.',
      [List]).

% Helper to advise turning off lights when no one is home
build_lights_off_msg([], '').
build_lights_off_msg(List, Msg) :-
    List \= [],
    format(atom(Msg),
      'Turn off lights in these rooms: ~w.',
      [List]).

% Helper to advise scheduling high-energy appliances for off-peak
build_scheduling_msg([], '').
build_scheduling_msg(List, Msg) :-
    List \= [],
    format(atom(Msg),
      'Schedule these high-energy appliances to run during off-peak hours: ~w.',
      [List]).
% ---------------------------------------------------------------------------
% ROOM APPLIANCE USAGE
% ---------------------------------------------------------------------------
room_appliance_usage(Room, TotalUsage) :-
    findall(Energy, appliance(_, on, Energy, _, Room), Energies),
    sumlist(Energies, TotalUsage).
