module Data.Person exposing (..)

import Array exposing (Array)
import Data.Block exposing (Block(..), Item(..))
import Random exposing (Generator)


type Job
    = Major
    | Ranger


type alias Person =
    { symbol : String
    , name : String
    , job : Job
    , progress : Int
    , nextMessage : Message
    }


alice : { symbol : String, name : String, job : Job }
alice =
    { symbol = "👩🏻"
    , name = "Alice"
    , job = Major
    }


rick : { symbol : String, name : String, job : Job }
rick =
    { symbol = "👨🏼"
    , name = "Rick"
    , job = Ranger
    }


jobToString : Job -> String
jobToString job =
    case job of
        Ranger ->
            "Ranger"

        Major ->
            "Major"


type alias Random a =
    Generator a


type alias Message =
    { content : String
    , request : Maybe Item
    , present : Maybe Item
    }


ranger : Array Message
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


major : Array Message
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


next : { job : Job, progress : Int } -> Maybe Message
next args =
    (case args.job of
        Ranger ->
            ranger

        Major ->
            major
    )
        |> Array.get args.progress


defaultRequest : Item -> Message
defaultRequest item =
    { content = "Hi, do you have some " ++ Data.Block.toString (ItemBlock item) ++ "?"
    , request = Just item
    , present = Nothing
    }


default : Random Message
default =
    Random.uniform (defaultRequest Data.Block.Stone)
        [ defaultRequest Data.Block.Wood
        ]


advanceProgress : Person -> Person
advanceProgress person =
    { person | progress = person.progress + 1 }


setNextMessage : Message -> Person -> Person
setNextMessage mail person =
    { person | nextMessage = mail }
