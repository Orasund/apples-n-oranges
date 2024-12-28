module Data.Message exposing (..)

import Array exposing (Array)
import Data.Block exposing (Block(..), Item(..))
import Data.Person exposing (Job(..))
import Random exposing (Generator)


type alias Random a =
    Generator a


type alias Mail =
    { message : String
    , request : Maybe Item
    , present : Maybe Item
    }


ranger : Array Mail
ranger =
    {--[ { sender = person
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
    ]--}
    [ defaultRequest Data.Block.Wood ]
        |> Array.fromList


major : Array Mail
major =
    {--[ { sender = person
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
    ]--}
    [ defaultRequest Data.Block.Stone ]
        |> Array.fromList


next : { job : Job, progress : Int } -> Maybe Mail
next args =
    (case args.job of
        Ranger ->
            ranger

        Major ->
            major
    )
        |> Array.get args.progress


defaultRequest item =
    { message = "Hi, do you have some " ++ Data.Block.toString (ItemBlock item) ++ "?"
    , request = Just item
    , present = Nothing
    }


default : Random Mail
default =
    Random.uniform (defaultRequest Data.Block.Stone)
        [ defaultRequest Data.Block.Wood
        ]
