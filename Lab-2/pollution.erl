-module(pollution).
%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3]).

%% rekord przedstawiający informację o stacji
-record(station, {coordinates, stationName}).
%% rekord przedstawiający informację o pomiarze (bez wartości)
-record(measurement, {type, date}).

createMonitor() -> maps:new().

%% funkcja pomocnicza dla odtwarzania pełnego klucza na podstawie części
getStationKey([], _) -> null;
getStationKey([#station{stationName=Key, coordinates = Coordinates}|_], Key) ->
              #station{stationName=Key, coordinates = Coordinates};
getStationKey([#station{stationName=Name, coordinates =Key}|_], Key) ->
              #station{stationName=Name, coordinates =Key};
getStationKey([_|T], Key) -> getStationKey(T, Key).


addStation(Name, Coordinates, Monitor) ->
  NameExists = getStationKey(maps:keys(Monitor), Name),
  CoordinatesExists = getStationKey(maps:keys(Monitor), Coordinates),
  case {NameExists, CoordinatesExists} of
    {null, null} -> Monitor#{#station{stationName=Name, coordinates = Coordinates} => maps:new()};
    {null, _} -> {error, "Station with these coordinates already exists!"};
    {_, _} -> {error, "Station with such a name already exists!"}
  end.


addValue(Key, MeasurementDate, MeasurementType, Value, Monitor) ->
  Value = maps:find(getStationKey(maps:keys(Monitor), Key), Monitor),
  case Value of
    error -> {error, "Measurement cannot be added: such station does not exist!"};
    {ok, Measurement} ->
      Exists = maps:find(#measurement{type=MeasurementType, date=MeasurementDate}, Measurement),
      case Exists of
        error ->
          case maps:size(Measurement) of
            0 -> maps:put(getStationKey(maps:keys(Monitor), Key), maps:put(
                  #measurement{type= MeasurementType, date= MeasurementDate}, Value, Measurement), Monitor);
            _ -> maps:update(getStationKey(maps:keys(Monitor), Key), maps:put(
                  #measurement{type= MeasurementType, date= MeasurementDate}, Value, Measurement), Monitor)
          end;
        _ -> {error, "Measurement with that date and type already exists!"}
      end
  end.


removeValue(Key, MeasurementDate, MeasurementType, Monitor) ->
  Value = maps:find(getStationKey(maps:keys(Monitor), Key), Monitor),
  case Value of
    error -> {error, "Measurement cannot be removed: such station does not exist!"};
    {ok, Measurement} ->
      ToRemove = maps:find(#measurement{type=MeasurementType, date=MeasurementDate}, Measurement),
      case ToRemove of
        error -> {error, "Measurement with that date and type does not exist yet!"};
        _ -> NewMeasurement = maps:remove(#measurement{type=MeasurementType, date=MeasurementDate}, Measurement),
          maps:update(getStationKey(maps:keys(Monitor), Key), NewMeasurement, Monitor)
      end
  end.


getOneValue(Key, MeasurementDate, MeasurementType, Monitor) ->
  Value = maps:find(getStationKey(maps:keys(Monitor), Key), Monitor),
  case Value of
    error -> {error, "Cannot get the value: such station does not exist!"};
    {ok, Measurement} ->
      ToReturn = maps:find(#measurement{type= MeasurementType, date= MeasurementDate}, Measurement),
      case ToReturn of
        error -> {error, "Measurements with such parameters don't exist"};
        {ok, Val} -> Val
      end
  end.


getStationMean(Key, MeasurementType, Monitor) ->
  Value = maps:find(getStationKey(maps:keys(Monitor), Key), Monitor),
  case Value of
    error -> {error, "Cannot get the mean value: such station does not exist!"};
    {ok, Measurement} ->
      getStationMeanByType(maps:to_list(Measurement), MeasurementType, 0, 0)
  end.


getStationMeanByType([], _, _, 0) -> 0.0;     %% zabiezpieczenie przed a / 0
getStationMeanByType([], _, Sum, Size) -> Sum / Size;
getStationMeanByType([{#measurement{type= MeasurementType}, Val}|T], MeasurementType, Sum, Size) ->
  getStationMeanByType(T, MeasurementType, Sum+Val, Size+1);
getStationMeanByType([_|T], MeasurementType, Sum, Size) ->
  getStationMeanByType(T, MeasurementType, Sum, Size).


iterateStationsMean(_, [], _, _, _, _, 0) -> 0.0;   %% zabiezpieczenie przed a / 0
iterateStationsMean(_, [], _, _, _, Sum, Size) -> Sum / Size;
iterateStationsMean(Monitor, [H|T], Fun, MeasurementType, Arg, Sum, Size) ->
  {ok, Val} = maps:find(H, Monitor),
  {MSum, MSize} = Fun(maps:to_list(Val), MeasurementType, Arg, {0,0}),
  iterateStationsMean(Monitor, T, Fun, MeasurementType, Arg, Sum + MSum, Size + MSize).


getDailyMean(MeasurementType, Day, Monitor) ->
  iterateStationsMean(Monitor, maps:keys(Monitor), fun getDailyMeanByType/4, MeasurementType, Day, 0, 0).


getDailyMeanByType([], _, _, {Sum,Size}) -> {Sum,Size};
getDailyMeanByType([{#measurement{type= MeasurementType, date={Day, _}}, Val}|T], MeasurementType, Day, {Sum,Size}) ->
  getDailyMeanByType(T, MeasurementType, Day, {Sum+Val,Size+1});
getDailyMeanByType([_|T], MeasurementType, Day, {Sum,Size}) ->
  getDailyMeanByType(T, MeasurementType, Day, {Sum,Size}).
