% Smart Home Energy Optimization System
% This program uses commonsense reasoning to optimize energy consumption 
% in a smart home while maintaining user comfort.

% Appliance states: appliance(Name, State, EnergyUsage).
appliance(tv, on, 150).
appliance(fridge, on, 200).
appliance(light, on, 50).

% Weather information: weather(Season, Temperature).
weather(summer, 90).

% User preferences: prefers_temperature(Temp).
prefers_temperature(72).

% Occupancy status: home_occupied(Status).
home_occupied(no).

% Rule: Optimize HVAC usage
optimize_hvac(OffTime) :-
    weather(summer, Temp),
    Temp > 85,
    prefers_temperature(Pref),
    Temp - Pref > 10,
    OffTime = '10 PM'.

% Rule: Recommend appliances to turn off when the home is unoccupied
should_turn_off(Appliance) :-
    appliance(Appliance, on, _),
    home_occupied(no),
    \+ essential_appliance(Appliance).

% Essential appliances that must remain on
essential_appliance(fridge).
essential_appliance(smoke_detector).

% Rule: Identify appliances that should remain on
should_remain_on(Appliance) :-
    essential_appliance(Appliance).
