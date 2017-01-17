namespace SlackTG

module Async =  
    open System.Threading.Tasks

    type AsyncBuilder with
        member x.Bind (t: Task<_>, f : 'a -> Async<_>) = async.Bind(Async.AwaitTask t, f)

/// a lightweight wrapper around the api for Deckbrew <https://deckbrew.com/api/>, a MtG card api 
module Deckbrew =
    module Types = 
        open FSharp.Data

        let (|TextCI|_|) (s : string) v =
            if s.Equals(v, System.StringComparison.OrdinalIgnoreCase) then Some TextCI
            else None

        let getJsonStrings file = 
            file 
            |> System.IO.File.ReadAllText
            |> JsonValue.Parse
            |> (fun v -> v.AsArray())
            |> Array.map (fun v -> v.AsString())

        type CardModel = JsonProvider< @"../../samples/cards.json", SampleIsList = true, RootName = "card", InferTypesFromValues = true>
        type SetsModel = JsonProvider< @"../../samples/sets.json", SampleIsList = true, RootName = "set", InferTypesFromValues = true>
        
        let CardTypes = [
            "artifact"
            "conspiracy"
            "creature"
            "enchantment"
            "instant"
            "land"
            "phenomenon"
            "plane"
            "planeswalker"
            "scheme"
            "sorcery"
            "tribal"
            "vanguard"
        ]
        let SubTypes = [
            "Clue"
            "Contraption"
            "Equipment"
            "Fortification"
            "Aura"
            "Curse"
            "Shrine"
            "Advisor"
            "Ally"
            "Angel"
            "Antelope"
            "Ape"
            "Archer"
            "Archon"
            "Artificer"
            "Assassin"
            "Assembly-Worker"
            "Atog"
            "Aurochs"
            "Avatar"
            "Badger"
            "Barbarian"
            "Basilisk"
            "Bat"
            "Bear"
            "Beast"
            "Beeble"
            "Berserker"
            "Bird"
            "Blinkmoth"
            "Boar"
            "Bringer"
            "Brushwagg"
            "Camarid"
            "Camel"
            "Caribou"
            "Carrier"
            "Cat"
            "Centaur"
            "Cephalid"
            "Chimera"
            "Citizen"
            "Cleric"
            "Cockatrice"
            "Construct"
            "Coward"
            "Crab"
            "Crocodile"
            "Cyclops"
            "Dauthi"
            "Demon"
            "Deserter"
            "Devil"
            "Djinn"
            "Dragon"
            "Drake"
            "Dreadnought"
            "Drone"
            "Druid"
            "Dryad"
            "Dwarf"
            "Efreet"
            "Elder"
            "Eldrazi"
            "Elemental"
            "Elephant"
            "Elf"
            "Elk"
            "Eye"
            "Faerie"
            "Ferret"
            "Fish"
            "Flagbearer"
            "Fox"
            "Frog"
            "Fungus"
            "Gargoyle"
            "Germ"
            "Giant"
            "Gnome"
            "Goat"
            "Goblin"
            "God"
            "Golem"
            "Gorgon"
            "Graveborn"
            "Gremlin"
            "Griffin"
            "Hag"
            "Harpy"
            "Hellion"
            "Hippo"
            "Hippogriff"
            "Homarid"
            "Homunculus"
            "Horror"
            "Horse"
            "Hound"
            "Human"
            "Hydra"
            "Hyena"
            "Illusion"
            "Imp"
            "Incarnation"
            "Insect"
            "Jellyfish"
            "Juggernaut"
            "Kavu"
            "Kirin"
            "Kithkin"
            "Knight"
            "Kobold"
            "Kor"
            "Kraken"
            "Lamia"
            "Lammasu"
            "Leech"
            "Leviathan"
            "Lhurgoyf"
            "Licid"
            "Lizard"
            "Manticore"
            "Masticore"
            "Mercenary"
            "Merfolk"
            "Metathran"
            "Minion"
            "Minotaur"
            "Mole"
            "Monger"
            "Mongoose"
            "Monk"
            "Moonfolk"
            "Mutant"
            "Myr"
            "Mystic"
            "Naga"
            "Nautilus"
            "Nephilim"
            "Nightmare"
            "Nightstalker"
            "Ninja"
            "Noggle"
            "Nomad"
            "Nymph"
            "Octopus"
            "Ogre"
            "Ooze"
            "Orb"
            "Orc"
            "Orgg"
            "Ouphe"
            "Ox"
            "Oyster"
            "Pegasus"
            "Pentavite"
            "Pest"
            "Phelddagrif"
            "Phoenix"
            "Pincher"
            "Pirate"
            "Plant"
            "Praetor"
            "Prism"
            "Processor"
            "Rabbit"
            "Rat"
            "Rebel"
            "Reflection"
            "Rhino"
            "Rigger"
            "Rogue"
            "Sable"
            "Salamander"
            "Samurai"
            "Sand"
            "Saproling"
            "Satyr"
            "Scarecrow"
            "Scion"
            "Scorpion"
            "Scout"
            "Serf"
            "Serpent"
            "Shade"
            "Shaman"
            "Shapeshifter"
            "Sheep"
            "Siren"
            "Skeleton"
            "Slith"
            "Sliver"
            "Slug"
            "Snake"
            "Soldier"
            "Soltari"
            "Spawn"
            "Specter"
            "Spellshaper"
            "Sphinx"
            "Spider"
            "Spike"
            "Spirit"
            "Splinter"
            "Sponge"
            "Squid"
            "Squirrel"
            "Starfish"
            "Surrakar"
            "Survivor"
            "Tetravite"
            "Thalakos"
            "Thopter"
            "Thrull"
            "Treefolk"
            "Triskelavite"
            "Troll"
            "Turtle"
            "Unicorn"
            "Vampire"
            "Vedalken"
            "Viashino"
            "Volver"
            "Wall"
            "Warrior"
            "Weird"
            "Werewolf"
            "Whale"
            "Wizard"
            "Wolf"
            "Wolverine"
            "Wombat"
            "Worm"
            "Wraith"
            "Wurm"
            "Yeti"
            "Zombie"
            "Zubera"
            "Plains"
            "Island"
            "Swamp"
            "Mountain"
            "Forest"
            "Desert"
            "Gate"
            "Lair"
            "Locus"
            "Urza's"
            "Mine"
            "Power-Plant"
            "Tower"
            "Alara"
            "Arkhos"
            "Azgol"
            "Belenon"
            "Bolas'sMeditationRealm"
            "Dominaria"
            "Equilor"
            "Ergamon"
            "Fabacin"
            "Iquatana"
            "Innistrad"
            "Ir"
            "Kaldheim"
            "Kamigawa"
            "Karsus"
            "Kephalai"
            "Kolbahan"
            "Kyneth"
            "Lorwyn"
            "Luvion"
            "Mercadia"
            "Mirrodin"
            "Moag"
            "Mongseng"
            "Muraganda"
            "NewPhyrexia"
            "Phyrexia"
            "Pyrulea"
            "Rabiah"
            "Rath"
            "Ravnica"
            "Regatha"
            "Segovia"
            "Serra'sRealm"
            "Shadowmoor"
            "Shandalar"
            "Ulgrotha"
            "Valla"
            "Vryn"
            "Wildfire"
            "Zendikar"
            "Ajani"
            "Arlinn"
            "Ashiok"
            "Bolas"
            "Chandra"
            "Dack"
            "Daretti"
            "Domri"
            "Elspeth"
            "Freyalise"
            "Garruk"
            "Gideon"
            "Jace"
            "Karn"
            "Kiora"
            "Koth"
            "Liliana"
            "Nahiri"
            "Nissa"
            "Narset"
            "Nixilis"
            "Ral"
            "Sarkhan"
            "Sorin"
            "Tamiyo"
            "Teferi"
            "Tezzeret"
            "Tibalt"
            "Ugin"
            "Venser"
            "Vraska"
            "Xenagos"
            "Arcane"
            "Trap"
            ]
        let SuperTypes = [
            "Basic"
            "Elite"
            "Legendary"
            "Ongoing"
            "Snow"
            "World"
        ]

        type Rarity = Common | Uncommon | Rare | Mythic
        with 
            override x.ToString() = match x with | Common -> "common" | Uncommon -> "uncommon" | Rare -> "rare" | Mythic -> "mythic"
            static member TryParse (s : string) : Rarity option =
                match s with
                | TextCI "common" -> Some Common
                | TextCI "uncommon" -> Some Uncommon
                | TextCI "rare"  -> Some Rare
                | TextCI "mythic" -> Some Mythic
                | _ -> None

        type Color = White | Blue | Black | Red | Green
        with 
            override x.ToString() = match x with | White -> "white" | Blue -> "blue" | Black -> "black" | Red -> "red" | Green -> "green"
            static member TryParse (s : string) : Color option =
                match s with
                | TextCI "w" | TextCI "white" -> Some White
                | TextCI "u" | TextCI "blue" -> Some Blue
                | TextCI "b" | TextCI "black" -> Some Black
                | TextCI "r" | TextCI "red" -> Some Red
                | TextCI "g" | TextCI "green" -> Some Green
                | _ -> None
         
        type Format = Vintage | Legacy | Modern | Standard | Commander
        with 
            override x.ToString() = match x with | Vintage -> "vintage" | Legacy -> "legacy" | Modern -> "modern" | Standard -> "standard" | Commander -> "commander"
            static member TryParse (s : string) : Format option = 
                match s with
                | TextCI "vintage" -> Some Vintage
                | TextCI "legacy" -> Some Legacy
                | TextCI "modern" -> Some Modern
                | TextCI "standard" -> Some Standard
                | TextCI "commander" | TextCI "edh" -> Some Commander
                | _ -> None
        
        type Status = Legal | Banned | Restricted
        with 
            override x.ToString() = match x with | Legal -> "legal" | Banned -> "banned" | Restricted -> "restricted"
            static member TryParse (s : string) : Status option =
                match s with
                | TextCI "legal" -> Some Legal
                | TextCI "banned" -> Some Banned
                | TextCI "restricted" -> Some Restricted
                | _ -> None

        type CardType = CT of string
        with 
            static member TryParse (t : string) = 
                if CardTypes |> List.exists (fun s -> s.Equals(t, System.StringComparison.OrdinalIgnoreCase))
                then CT <| t.ToLower() |> Some
                else None
        
        type SubType = ST of string
        with
            static member TryParse (t : string) =
                if SubTypes |> List.exists (fun s -> s.Equals(t, System.StringComparison.OrdinalIgnoreCase))
                then ST <| t.ToLower() |> Some
                else None

        type SuperType = SUT of string
        with
            static member TryParse (t : string) = 
                if SuperTypes |> List.exists (fun s -> s.Equals(t, System.StringComparison.OrdinalIgnoreCase))
                then SUT <| t.ToLower() |> Some
                else None

        /// the model returned by Deckbrew for any 400+ status code
        type Error = {
            Errors : string list
        }

        type Link = Next of System.Uri | Prev of System.Uri
        type Linked<'a> = { Payload : 'a; Next : System.Uri option; Prev : System.Uri option }

    module API = 
        open FSharp.Text.RegexProvider
        open System
        open Async
        open HttpFs
        open HttpFs.Client
        open Hopac

        let root = Uri("https://api.deckbrew.com",UriKind.Absolute)

        type getRequestParameter = 
            | Type of Types.CardType
            | SubType of Types.SubType
            | SuperType of Types.SuperType
            | Name of string
            | Oracle of string
            | Set of string
            | Rarity of Types.Rarity
            | Color of Types.Color
            | Multicolor of bool
            | MultiverseId of string
            | Format of Types.Format
            | Status of Types.Status

        let getQSName (getRequestParam : getRequestParameter) : QueryStringName= 
            match getRequestParam with
            | Type (_) -> "type"
            | SubType(_) -> "subtype"
            | SuperType(_) -> "supertype"
            | Name(_) -> "name"
            | Oracle(_) -> "oracle"
            | Set(_) -> "set"
            | Rarity(_) -> "rarity"
            | Color(_) -> "color"
            | Multicolor(_) -> "multicolor"
            | MultiverseId(_) -> "multiverseid"
            | Format(_) -> "format"
            | Status(_) -> "status"

        let getQSValue (getRequestParam : getRequestParameter) : QueryStringValue =
            match getRequestParam with
            | Type(Types.CT t) -> t
            | SubType(Types.ST st) -> st
            | SuperType(Types.SUT st) -> st
            | Name(name) -> name
            | Oracle(oracleText) -> oracleText
            | Set(setId) -> setId
            | Rarity r -> string r
            | Color c -> string c
            | Multicolor m -> string m
            | MultiverseId mid -> mid
            | Format f -> string f
            | Status s -> string s 

        let parseErrorResponse resp =
            { Types.Error.Errors = [ resp ] }
        
        let uriLink link = 
            match link with
            | Types.Next u -> u
            | Types.Prev u -> u

        let parseLinks (headers : Map<ResponseHeader, string>) =
            let createLink s =
                let m = System.Text.RegularExpressions.Regex("<(?<uri>[\S]*)>;\srel=\"(?<rel>next|prev)\"").Match s
                if m.Success
                then 
                    match m.Groups.[1].Value with
                    | "next" -> Types.Next (System.Uri(m.Groups.[0].Value)) |> Some
                    | "prev" -> Types.Prev (System.Uri(m.Groups.[0].Value)) |> Some
                    | _ -> None
                else None
            
            match headers |> Map.tryFind Link with
            | None -> None, None
            | Some links -> 
                match links |> String.split ',' with
                | [] -> None, None
                | links -> 
                    let potentialLinks = links |> List.map (fun s -> s.Trim()) |> List.choose createLink
                    let prevs, nexts = potentialLinks |> List.partition (fun l -> match l with Types.Prev _ -> true | _ -> false)
                    List.tryHead prevs, List.tryHead nexts
        
        type ApiCall<'a> = Async<Choice<'a, Types.Error>>

        let doRequest (uri : Uri) (queryStringItems : seq<QueryStringName * QueryStringValue>) successF = async {
            let! resp = 
                queryStringItems 
                |> Seq.fold (fun r (k,v) -> Request.queryStringItem k v r) (Request.create Get uri) 
                |> getResponse 
                |> Job.toAsync
            let! content = resp |> Response.readBodyAsString |> Job.toAsync

            if resp.statusCode >= 400
            then return content |> parseErrorResponse |> Choice2Of2
            else return successF resp.headers content |> Choice1Of2
        }

        let getCards parameters : ApiCall<Types.Linked<Types.CardModel.Card list>> =
            let qsParamToNameValue qsp = getQSName qsp, getQSValue qsp

            let parseCards (headers : Map<ResponseHeader, string>) (content : string) =
                let parseCards content = 
                    let multiples = content |> FSharp.Data.JsonValue.Parse |> FSharp.Data.JsonExtensions.AsArray |> Array.toList
                    let stringified = multiples |> List.map string 
                    let cards = stringified |> List.map (Types.CardModel.Parse)
                    cards
                
                let prev, next = parseLinks headers
                let cards = parseCards content
                { Types.Linked.Payload = cards; Types.Linked.Next = Option.map uriLink next; Types.Linked.Prev = Option.map uriLink prev; }

            let cardsUri = Uri(root, "/mtg/cards")

            let qs = parameters |> List.map qsParamToNameValue
            doRequest cardsUri qs parseCards
        
        /// This does a fuzzy search on card name because we don't know gatherer ids yet
        let getCard cardName : ApiCall<Types.CardModel.Card> = async {
            let nameParam = Name cardName
            let! cardsChoice = getCards [nameParam] 
            match cardsChoice with
            | Choice2Of2 err -> return Choice2Of2 err
            | Choice1Of2 cards -> 
                match cards.Payload |> List.tryHead with
                | Some c -> return Choice1Of2 c
                | None -> return Choice2Of2 ({Errors = [sprintf "no card found for name \"%s\"" cardName] } )

        }