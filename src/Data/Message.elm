module Data.Message exposing (..)

import Array exposing (Array)
import Data.Block exposing (Block(..), Item(..))
import Data.Person exposing (Job(..), Person)
import Random exposing (Generator)


type alias Random a =
    Generator a


type alias Mail =
    { sender : Person
    , message : String
    , request : Maybe Item
    , present : Maybe Item
    , accepted : Bool
    }


ranger : Person -> Array Mail
ranger person =
    [ { sender = person
      , message = "Good Morning Neighbor, I'm " ++ person.name ++ ", the hunter of our community. Would you have some spare to help the reforestation of our land?"
      , request = Just Coin
      , present = Nothing
      , accepted = False
      }
    , { sender = person
      , message = "Hi, could you help me rebuild by chicken pen?"
      , request = Just Coin
      , present = Nothing
      , accepted = False
      }
    , { sender = person
      , message = "You woun't be live it, but some chicks actually hatched! Do you want some?"
      , request = Nothing
      , present = Just Chick
      , accepted = False
      }
    ]
        |> Array.fromList


major : Person -> Array Mail
major person =
    [ { sender = person
      , message = "Hello, my name is " ++ person.name ++ ". Im the mayor of this little community. Take this as a little welcome gift."
      , request = Nothing
      , present = Just Coin
      , accepted = False
      }
    , { sender = person
      , message = "Thanks for your contribution to our community."
      , request = Nothing
      , present = Just Coin
      , accepted = False
      }
    ]
        |> Array.fromList


next : Person -> Maybe Mail
next person =
    person
        |> (case person.job of
                Ranger ->
                    ranger

                Major ->
                    major
           )
        |> Array.get person.progress


ellen : Array String
ellen =
    [ "Hi, Ellen here! I wanted to check in, if you came on save and sound. Heres a little gift to get you started"
    , "Hi again! I forgot to setup  your telephone. You should now be able to trade with your neighbors!"
    , "Hello, It's me Ellen ;) The local rescue shelter asked if we could take care of a little cat. Would you be interested?"
    ]
        |> Array.fromList


default : Person -> Random Mail
default person =
    let
        defaultRequest item =
            { sender = person
            , message = "Hi, do you have some " ++ Data.Block.toString (ItemBlock item) ++ "?"
            , request = Just item
            , present = Nothing
            , accepted = False
            }

        defaultPresent item =
            { sender = person
            , message = "Hi, would you want some " ++ Data.Block.toString (ItemBlock item) ++ "?"
            , request = Nothing
            , present = Just item
            , accepted = False
            }
    in
    Random.uniform (defaultRequest Data.Block.Coin)
        [ defaultRequest Data.Block.Berries
        , defaultRequest Data.Block.Diamand
        , defaultRequest Data.Block.Shrimps
        , defaultRequest Data.Block.Wood
        , defaultPresent Data.Block.Coin
        , defaultPresent Data.Block.Berries
        , defaultPresent Data.Block.Diamand
        , defaultPresent Data.Block.Shrimps
        , defaultPresent Data.Block.Wood
        ]
