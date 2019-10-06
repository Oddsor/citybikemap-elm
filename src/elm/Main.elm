module Main exposing (Model, Msg(..), init, listOfStations, main, oslo, stationMarker, subs, update, updatedMap, view)

import Dict
import Html as H exposing (..)
import Html.Attributes as A
import Html.Events as E
import Http
import Json.Decode as JsonDecode
import Json.Decode.Pipeline as JsonDecodeP
import Maps
import Maps.Geo
import Maps.Map as Map
import Maps.Marker as Marker exposing (Marker)
import Task


type ApiRequest data
    = Loading (Maybe data)
    | Loaded data
    | Error Http.Error
    | NotLoaded


type alias Station =
    { stationId : String
    , address : String
    , lat : Float
    , lng : Float
    , capacity : Int
    }


type ApiMsg
    = GetStations (ApiRequest Stations)
    | GetAvailability (ApiRequest Availabilities)


type alias ApiStore =
    { availabilities : ApiRequest Availabilities
    , stations : ApiRequest Stations
    }


initApiStore =
    { availabilities = NotLoaded
    , stations = NotLoaded
    }


updateStore : ApiMsg -> ApiStore -> ( ApiStore, Cmd msg )
updateStore apiResult apiStore =
    case apiResult of
        GetAvailability status ->
            ( { apiStore | availabilities = status }, Cmd.none )

        GetStations status ->
            ( { apiStore | stations = status }, Cmd.none )


type alias Stations =
    Dict.Dict String Station


type alias Availabilities =
    Dict.Dict String StationAvailability


type alias StationAvailability =
    { stationId : String
    , bikesAvailable : Int
    , docksAvailable : Int
    }


stationDecoder =
    JsonDecode.succeed Station
        |> JsonDecodeP.required "station_id" JsonDecode.string
        |> JsonDecodeP.required "address" JsonDecode.string
        |> JsonDecodeP.required "lat" JsonDecode.float
        |> JsonDecodeP.required "lon" JsonDecode.float
        |> JsonDecodeP.required "capacity" JsonDecode.int


availabilityDecoder =
    JsonDecode.succeed StationAvailability
        |> JsonDecodeP.required "station_id" JsonDecode.string
        |> JsonDecodeP.required "num_bikes_available" JsonDecode.int
        |> JsonDecodeP.required "num_docks_available" JsonDecode.int


availabilitiesDecoder =
    JsonDecode.field "data" (JsonDecode.field "stations" (JsonDecode.list availabilityDecoder))
        |> JsonDecode.map stationToDict


stationToDict : List { a | stationId : String } -> Dict.Dict String { a | stationId : String }
stationToDict =
    Dict.fromList << List.map (\y -> ( y.stationId, y ))


stationsDecoder =
    JsonDecode.field "data" (JsonDecode.field "stations" (JsonDecode.list stationDecoder))
        |> JsonDecode.map stationToDict


getAvailabilities oldValue =
    Cmd.batch
        [ Task.perform GetAvailability <| Task.succeed (Loading oldValue)
        , Cmd.map GetAvailability <|
            Http.send resultToStatus <|
                Http.get
                    "https://gbfs.urbansharing.com/oslobysykkel.no/station_status.json"
                    availabilitiesDecoder
        ]


getStations oldValue =
    Cmd.batch
        [ Task.perform GetStations <| Task.succeed (Loading oldValue)
        , Cmd.map GetStations <|
            Http.send resultToStatus <|
                Http.get
                    "https://gbfs.urbansharing.com/oslobysykkel.no/station_information.json"
                    stationsDecoder
        ]


resultToStatus :
    Result Http.Error data
    -> ApiRequest data
resultToStatus decoded =
    case decoded of
        Ok res ->
            Loaded res

        Result.Err err ->
            Error err


main : Program Never Model Msg
main =
    H.program { init = init, view = view, update = update, subscriptions = subs }


oslo =
    Maps.Geo.latLng 59.91902100453379 10.732437231445346


init : ( Model, Cmd Msg )
init =
    { selectedId = Nothing
    , map =
        Maps.defaultModel
            |> Maps.updateMap (Map.setZoom 12 >> Map.moveTo oslo)
    , apiData = initApiStore
    }
        ! [ Cmd.map ApiMsg (getStations Nothing)
          , Cmd.map ApiMsg (getAvailabilities Nothing)
          ]


subs : Model -> Sub Msg
subs model =
    Maps.subscriptions model.map
        |> Sub.map MapMsg


type Msg
    = SelectedStation String Bool
    | MapMsg (Maps.Msg Msg)
    | ApiMsg ApiMsg


type alias Model =
    { selectedId : Maybe String
    , map : Maps.Model Msg
    , apiData : ApiStore
    }


stationMarker : Maybe String -> Station -> Marker Msg
stationMarker selectedId station =
    Marker.createCustom
        (H.span [ E.onClick <| SelectedStation station.stationId False ]
            [ H.text
                (if Just station.stationId == selectedId then
                    "🚴"

                 else
                    "🚲"
                )
            ]
        )
        { lat = station.lat, lng = station.lng }


updatedMap zoom model =
    case model.apiData.stations of
        Loaded stations ->
            let
                stationList =
                    Dict.values stations

                moveAndZoom =
                    \m ->
                        let
                            maybeStation =
                                Maybe.andThen (\s -> Dict.get s stations) model.selectedId
                        in
                        case ( maybeStation, zoom ) of
                            ( Just station, True ) ->
                                m |> Map.moveTo { lat = station.lat, lng = station.lng } |> Map.setZoom 16

                            _ ->
                                m
            in
            { model
                | map =
                    model.map
                        |> Maps.updateMarkers (\oldMarkers -> List.map (stationMarker model.selectedId) stationList)
                        |> Maps.updateMap moveAndZoom
            }

        _ ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedStation stationId zoom ->
            updatedMap zoom { model | selectedId = Just stationId } ! []

        MapMsg msg ->
            model.map
                |> Maps.update msg
                |> Tuple.mapFirst (\map -> { model | map = map })
                |> Tuple.mapSecond (Cmd.map MapMsg)

        ApiMsg msg ->
            let
                ( newData, newMsg ) =
                    updateStore msg model.apiData
            in
            updatedMap False { model | apiData = newData } ! [ Cmd.map ApiMsg newMsg ]


listOfStations model =
    case model.apiData.stations of
        Loaded data ->
            let
                stations =
                    Dict.values data
            in
            H.table []
                (List.map
                    (\station ->
                        H.tr
                            [ A.style
                                (if model.selectedId == Just station.stationId then
                                    [ ( "background-color", "yellow" ) ]

                                 else
                                    []
                                )
                            , E.onClick <| SelectedStation station.stationId True
                            ]
                            [ H.td [] [ H.text station.address ]
                            ]
                    )
                    stations
                )

        _ ->
            H.text "Need data"



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html Msg
view model =
    div [ A.class "container", A.style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ]
        [ Maps.mapView MapMsg <| Maps.view model.map
        , listOfStations model
        ]
