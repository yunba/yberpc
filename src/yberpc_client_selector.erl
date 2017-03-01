-module(yberpc_client_selector).

-export([select_one/1]).
-compile({parse_transform, lager_transform}).

%% input with a list of clients, and return with a Client
%% if the list is empty, return undefined
-spec(select_one(Clients :: list()) -> Client :: tuple() | undefined).
select_one(Clients) ->
  TotalWeight = lists:foldl(
    fun({_, _, Weight, _}, CurWeight) ->
      CurWeight + Weight
    end, 0, Clients),
  RandomWeight =
    case TotalWeight of
      0 -> 0;
      _ -> random:uniform(TotalWeight)
    end,
  select_one(Clients, 0, RandomWeight).

select_one([], _, _) ->
  undefined;
select_one([Client], _, _) ->
  lager:debug("client is selected: ~p", [Client]),
  Client;
select_one([Client | Tail], CurWeight, RandomWeight) ->
  {_, _, Weight, _} = Client,
  case (Weight + CurWeight >= RandomWeight) of
    true ->
      lager:debug("client is selected: ~p", [Client]),
      Client;
    _ ->
      select_one(Tail, Weight + CurWeight, RandomWeight)
  end.
