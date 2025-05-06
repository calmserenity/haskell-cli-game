{-# LANGUAGE LambdaCase #-} 
module Main where

import Text.Read (readMaybe)
import System.Random (randomRIO)
import Data.Sequence (Seq, (|>), index, deleteAt, fromList, findIndexL)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import System.Directory (doesFileExist)
import Data.List (intercalate) 


class Information a where
    getInfo :: a -> String

class Displayable a where
    display :: a -> IO ()

instance Displayable Player where
    display player = do
        putStrLn "----------------------------------------------"
        putStrLn (getInfo player)
        putStrLn "----------------------------------------------"

instance Displayable Boss where
    display boss = do
        putStrLn "------------------------------------------"
        putStrLn (getInfo boss)
        putStrLn "------------------------------------------"

data Player = Player
    {
        name :: String,
        health :: Int,
        inventory :: Seq (String, Char),
        energyLevel :: Int,
        grade :: Char,
        equippedItem :: (String, Char),
        money :: Float
    } deriving (Show, Eq)

data Boss = Boss
    {
        bossName :: String,
        bossHealth :: Int,
        bossGrade :: Char,
        normalMove :: Move,
        ultimateMove :: Move
    } deriving (Show, Eq)

data Move = Move
    {
        moveName :: String,
        moveDamage :: Int
    } deriving (Show, Eq)

newtype Store = Store
    {
        items :: [(String, Char, Float)]
    } deriving (Show, Eq)

newtype Enemy = Enemy Boss deriving (Show, Eq)

newtype PlayerEnemy = PlayerEnemy Player deriving (Show, Eq)

class Combatable a where
    getHealth :: a -> Int
    takeDamage :: a -> Int -> a
    isAlive :: a -> Bool
    isAlive a = getHealth a > 0

class PlayerCombatable a where
    playerAttack :: Player -> a -> IO (Player, a)

class BossCombatable a where
    bossAttack :: Boss -> a -> IO (Boss, a)

class Equippable a where
    equip :: a -> Player -> Player
    isEquipped :: a -> Player -> Bool

class Purchasable a where
    buy :: a -> Player -> Maybe (a, Player)
    sell :: a -> Player -> (a, Player)
    getPrice :: a -> Float

instance Information Player where
    getInfo player =
        "Name: " ++ name player ++
        "\nHealth: " ++ show (health player) ++
        "\nEquipped Item: " ++ fst (equippedItem player) ++ " (Grade " ++ [snd (equippedItem player)] ++ ")" ++
        "\nEnergy Level: " ++ show (energyLevel player) ++
        "\nMoney: " ++ show (money player) ++
        "\nGrade: " ++ [grade player] ++
        "\nInventory: " ++ showInventory (inventory player)
        where
        showInventory inv = if Seq.null inv
            then "Empty"
            else "\n  - " ++ unwords (map (\(itemName, itemGrade) -> itemName ++ " (Grade " ++ [itemGrade] ++ ")") (toList inv))

instance Information Boss where
    getInfo boss =
        "Boss Name: " ++ bossName boss ++
        "\nBoss Health: " ++ show (bossHealth boss) ++
        "\nBoss Grade: " ++ [bossGrade boss] ++
        "\nNormal Move: " ++ moveName (normalMove boss) ++ " (Damage: " ++ show (moveDamage (normalMove boss)) ++ ")" ++
        "\nUltimate Move: " ++ moveName (ultimateMove boss) ++ " (Damage: " ++ show (moveDamage (ultimateMove boss)) ++ ")"

instance Combatable Player where
    getHealth = health 
    takeDamage player amount = player { health = max 0 (health player - amount) }

instance PlayerCombatable Enemy where
    playerAttack player (Enemy boss) =
      if energyLevel player >= 15 && canAttack player boss
        then do
            let damageDealt = 30
                updatedBoss = takeDamage boss damageDealt
                updatedPlayer = player { energyLevel = max 0 (energyLevel player - 15) }
            putStrLn " "
            putStrLn "------------------------------------------"
            putStrLn ("You attack and deal " ++ show damageDealt ++ " damage!")
            putStrLn ("Your energy level is now " ++ show (energyLevel updatedPlayer))
            putStrLn ("Your health is now " ++ show (health updatedPlayer))
            putStrLn ("Boss health: " ++ show (getHealth updatedBoss))
            putStrLn " "
            return (updatedPlayer, Enemy updatedBoss)
        else do
            putStrLn " "
            putStrLn "You cannot attack because your grade or equipped item's grade is too low, or you don't have enough energy!"
            putStrLn " "
            return (player, Enemy boss)

instance Combatable Boss where
    getHealth = bossHealth
    takeDamage boss amount = boss { bossHealth = max 0 (bossHealth boss - amount) }

instance BossCombatable Player where
    bossAttack boss player = do
        let attack = if bossHealth boss <= 20 then ultimateMove boss else normalMove boss
        putStrLn "------------------------------------------"
        putStrLn ("The boss uses " ++ moveName attack ++ ".")
        putStrLn ("The attack deals " ++ show (moveDamage attack) ++ " damage.")
        putStrLn "Do you want to dodge? (y/n)"
        input <- getLine
        if input == "y"
          then do
              updatedPlayer <- attemptDodge player boss attack
              return (boss, updatedPlayer)
          else if input == "n"
            then do
                let damageTaken = moveDamage attack
                    updatedPlayer = takeDamage player damageTaken
                putStrLn " "
                putStrLn ("You take " ++ show damageTaken ++ " damage.")
                putStrLn ("Your health: " ++ show (health updatedPlayer))
                putStrLn "------------------------------------------"
                putStrLn " "
                return (boss, updatedPlayer)
          else do
              putStrLn "Invalid input. Please enter either y or n."
              bossAttack boss player

newtype Equipment = Equipment (String, Char) deriving (Show, Eq)

instance Equippable Equipment where
    equip (Equipment item) player = player { equippedItem = item }
    isEquipped (Equipment item) player = equippedItem player == item

newtype Purchase = Purchase (String, Char, Float) deriving (Show, Eq)

instance Purchasable Purchase where
    buy (Purchase item) player =
        if money player >= getPrice (Purchase item)
        then Just (Purchase item, player { money = money player - getPrice (Purchase item), inventory = inventory player |> (fst3 item, snd3 item)})
        else Nothing
    sell (Purchase item) player =
        (Purchase item, player { money = money player + getPrice (Purchase item), inventory = Seq.filter (\(itemName, _) -> itemName /= fst3 item) (inventory player) })
    getPrice (Purchase (_, _, price)) = price

-- Helper Functions

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

-- Enemy Data

blueFangedNaga :: Boss
blueFangedNaga = Boss
    {
        bossName= "Blue Fanged Naga",
        bossHealth = 100,
        bossGrade = 'D',
        normalMove = Move {moveName = "Venom Bite", moveDamage = 10},
        ultimateMove = Move {moveName = "Venom Inferno", moveDamage = 50}
    }

vulcan :: Boss
vulcan = Boss
    {
        bossName = "Vulcan",
        bossHealth = 150,
        bossGrade = 'C',
        normalMove = Move {moveName = "Fire Orb", moveDamage = 50},
        ultimateMove = Move {moveName = "Hell Fire", moveDamage = 80}
    }

iceElves :: Boss
iceElves = Boss
    {
        bossName = "Ice Elves",
        bossHealth = 200,
        bossGrade = 'B',
        normalMove = Move {moveName = "Frost Burn", moveDamage = 80},
        ultimateMove = Move {moveName = "Thousand Arrows", moveDamage = 120}
    }

deathKnights :: Boss
deathKnights = Boss
    {
        bossName = "Death Knights",
        bossHealth = 300,
        bossGrade = 'A',
        normalMove = Move {moveName = "Death by Sword", moveDamage = 120},
        ultimateMove = Move {moveName = "Instant Death", moveDamage = 180}
    }

gameStore :: Store
gameStore = Store
    {
        items = [("Improved Sword", 'C', 100.0),
                ("Advanced Sword", 'B', 120.0),
                ("Mastercrafted Sword", 'A', 140.0)]
    }

-- Game Logic

gameMainMenu :: IO Int
gameMainMenu =
    putStrLn "----------------------------------------------------" >>
    putStrLn "Welcome to Hunter! A Haskell based console game." >>
    putStrLn "1. Start Game" >>
    putStrLn "2. Exit Game" >>
    putStrLn "----------------------------------------------------" >>
    getLine >>= \input ->
        case readMaybe input :: Maybe Int of
            Just n -> return n
            Nothing -> putStrLn "Please enter the number that corresponds to the option." >> gameMainMenu

processgameMainMenu :: Int -> IO ()
processgameMainMenu x = case x of
    1 -> do
        playerName <- characterNameInput
        putStrLn "Loading game....."
        gameFreshStart playerName
    2 -> putStrLn "Exit Game"
    _ -> putStrLn "Invalid Option. Try again." >> main

characterNameInput :: IO String
characterNameInput = do
    putStrLn " "
    putStrLn "To start the game, please enter the character name."
    putStrLn "Character Name: "
    getLine >>= validateName
    where
        validateName input
            | null input = putStrLn "Character name cannot be empty. Please enter a valid name." >> characterNameInput
            | all (`elem` (['a'..'z']++ ['A'..'Z'])) input = return input
            | otherwise = putStrLn "Please enter a name without symbols or numbers or space." >> characterNameInput

gameFreshStart :: String -> IO()
gameFreshStart playerName = do
    maybePlayer <- loadPlayer playerName
    case maybePlayer of
        Just player -> do
            gameWelcome player
        Nothing -> do
            let player = Player { name = playerName, health = 100, inventory = fromList [("Simple Knife", 'D'),
            ("Rusty Sword", 'E')], energyLevel = 100, grade = 'D', equippedItem = ("Simple Knife", 'D'), money = 100.00}
            gameWelcome player

gameWelcome :: Player -> IO ()
gameWelcome player =
    putStrLn " " >>
    putStrLn ("Welcome, " ++ name player ++ "! Your Current Stats are: ") >>
    display player >>
    putStrLn "Press Enter to continue or 'q' to exit. Press 'g' to read the guide" >>
    putStrLn "For first-time players, it is recommended to read the guide." >>
    getGameStartResponse
    where
        getGameStartResponse = getLine >>= gameStartResponse
        gameStartResponse input
            | input == "q" = putStrLn "See you again!"
            | input == "g" = gameGuide player
            | otherwise    = gamePlayMenu player

gameGuide :: Player -> IO ()
gameGuide player =
    putStrLn " ">>
    putStrLn "You begin as a grade E hunter." >>
    putStrLn "To advance, you will have to enter alternate dimensions to fight dungeon monsters and gain experience." >>
    putStrLn "______________________________________________________________________________________________________________________" >>
    putStrLn "In this world, there are inter-dimensional tunnels that travel to a pocket dimension consisting of otherwordly monsters." >>
    putStrLn "Humans have adapted to this world and hunt such monsters to obtain the treasure that they hold." >>
    putStrLn "As a grade D hunter, you will have to travel into these tunnels and slay monsters in order to level up and earn coins." >>
    putStrLn "Press any key to continue." >>
    getLine >> gamePlayMenu player

gamePlayMenu :: Player -> IO ()
gamePlayMenu player =
    putStrLn " " >>
    putStrLn "Choose your path: " >>
    putStrLn "1. Game Play" >>
    putStrLn "2. Show Inventory" >>
    putStrLn "3. Check Stats" >>
    putStrLn "4. Go to Shop" >>
    putStrLn "5. Save and Quit" >>
    getLine >>= \input ->
        case readMaybe input :: Maybe Int of
            Just 1 -> putStrLn "Game Starting..." >> gamePlayOne player
            Just 2 -> putStrLn "Opening Inventory..." >> checkInventory player
            Just 3 -> putStrLn "Showing Stats..." >> gameWelcome player
            Just 4 -> putStrLn "Travelling to shop..." >> checkStore player gameStore
            Just 5 -> putStrLn "Saving game..." >> savePlayer player >> putStrLn "See you again!"
            _ -> putStrLn "Please enter the number that corresponds to the option." >> gamePlayMenu player

gamePlayOne :: Player -> IO ()
gamePlayOne player = do
    let tunnelLength = 40
    let startPosition = 10
    putStrLn " "
    putStrLn "You are now in a tunnel."
    putStrLn "Do you want to go left or right? (l/r)"
    input <- getLine
    case input of
        "l" -> gameLoop (startPosition - 10) tunnelLength
        "r" -> gameLoop (startPosition + 10) tunnelLength
        _   -> do
            putStrLn "Invalid direction. Please enter 'l' or 'r'."
            gamePlayOne player
    where
        gameLoop :: Int -> Int -> IO ()
        gameLoop position tunnelLength
            | position == 30 = spawnBoss player
            | otherwise = do
                displayTunnel position tunnelLength
                putStrLn "Press 'a' to move left, 'd' to move right, or 'q' to leave the tunnel."
                getLine >>= handlePositionInput
            where
                handlePositionInput input
                    | input == "a" && position > 0                = gameLoop (position - 10) tunnelLength
                    | input == "d" && position < tunnelLength - 1 = gameLoop (position + 10) tunnelLength
                    | input == "q"                                = putStrLn "Exiting the tunnel. Goodbye!" >> gamePlayMenu player
                    | otherwise                                   = gameLoop position tunnelLength

spawnBoss :: Player -> IO ()
spawnBoss player = do
    boss <- if grade player == 'D'
            then return blueFangedNaga
            else matchRandomBoss
    let originalHealth = bossHealth boss
    putStrLn " "
    display boss
    bossBattle player (Enemy boss) originalHealth
    where
        bossBattle :: Player -> Enemy -> Int -> IO ()
        bossBattle player enemy@(Enemy boss) originalHealth
            | not (isAlive boss) = gameWin player
            | not (isAlive player) = putStrLn "You have been defeated. Game Over!"
            | otherwise = do
                putStrLn "------------------------------------------"
                putStrLn "It's your turn! What will you do?"
                putStrLn "Press 'a' to attack or 'r' to retreat."
                action <- getLine
                case action of
                    "a" -> do
                        (updatedPlayer, Enemy updatedBoss) <- playerAttack player enemy
                        if isAlive updatedPlayer
                        then do
                            (updatedBoss', updatedPlayer') <- bossAttack updatedBoss updatedPlayer
                            bossBattle updatedPlayer' (Enemy updatedBoss') originalHealth
                        else bossBattle updatedPlayer (Enemy updatedBoss) originalHealth
                    "r" -> putStrLn "You retreat safely. The battle is over." >> gamePlayOne player
                    _   -> do
                        putStrLn "Invalid action. Try again."
                        bossBattle player enemy originalHealth

        matchRandomBoss :: IO Boss
        matchRandomBoss = do
            randomIndex <- randomRIO (0, 2)
            matchBossByIndex randomIndex

        matchBossByIndex :: Int -> IO Boss
        matchBossByIndex 0 = pure vulcan
        matchBossByIndex 1 = pure iceElves
        matchBossByIndex 2 = pure deathKnights
        matchBossByIndex _ = pure vulcan

attemptDodge :: Player -> Boss -> Move -> IO Player
attemptDodge player boss attack = do
    let dodgeSuccess = energyLevel player > 5
        updatedPlayer = player { energyLevel = max 0 (energyLevel player - 5) }
    if dodgeSuccess
        then do
            putStrLn " "
            putStrLn "------------------------------------------"
            putStrLn "You successfully dodged the attack!"
            putStrLn ("Your energy level is now " ++ show (energyLevel updatedPlayer))
            putStrLn ("Your health: " ++ show (health updatedPlayer))
            putStrLn " "
            return updatedPlayer
        else do
            let damageTaken = moveDamage attack
                furtherUpdatedPlayer = takeDamage updatedPlayer damageTaken
            putStrLn ("You failed to dodge and take " ++ show damageTaken ++ " damage.")
            putStrLn ("Your health: " ++ show (health furtherUpdatedPlayer))
            putStrLn "------------------------------------------"
            return furtherUpdatedPlayer


canAttack :: Player -> Boss -> Bool
canAttack player boss = compareGrades (grade player) (bossGrade boss) &&
                        compareGrades (snd (equippedItem player)) (bossGrade boss)

compareGrades :: Char -> Char -> Bool
compareGrades playerGrade bossGrade = gradeToValue playerGrade >= gradeToValue bossGrade
    where
        gradeToValue 'E' = 1
        gradeToValue 'D' = 2
        gradeToValue 'C' = 3
        gradeToValue 'B' = 4
        gradeToValue 'A' = 5
        gradeToValue _   = 0

gameWin :: Player -> IO ()
gameWin player =
    putStrLn "You have defeated the boss" >>
    putStrLn "Press 'e' to collect your rewards" >>
    getLine >>= \input ->
        case input of
            "e" -> rewardPlayer player
            _ -> putStrLn "You are exiting the tunnel without rewards." >> levelUp player

rewardPlayer :: Player -> IO ()
rewardPlayer player =
    putStrLn "">>
    putStrLn "You receive 100 coins, 200 health points and 200 energy points." >>
    let updatedPlayer = player { money = money player + 100, health = health player + 200, energyLevel = energyLevel player + 200 }
    in putStrLn ("Your coin balance: " ++ show (money updatedPlayer)) >>
    putStrLn ("Your health points: " ++ show (health updatedPlayer)) >>
    levelUp updatedPlayer

levelUp :: Player -> IO ()
levelUp player =
    let newGrade = case grade player of
            'D' -> 'C'
            'C' -> 'B'
            'B' -> 'A'
            _ -> grade player
        updatedPlayer = player { grade = newGrade }
    in (putStrLn ("You have leveled up to grade " ++ [newGrade])) >>
    askToContinue updatedPlayer

askToContinue :: Player -> IO ()
askToContinue player =
    putStrLn "Would you like to continue playing? (y/n)" >>
    getLine >>= \case
        "y" -> gamePlayOne player
        "n" -> gamePlayMenu player
        _ -> do putStrLn " "
                putStrLn "Invalid input. Please enter 'y' or 'n'."
                askToContinue player

displayTunnel :: Int -> Int -> IO()
displayTunnel position tunnelLength =
    putStrLn " " >>
    putStrLn (replicate tunnelLength '_') >>
    putStrLn (replicate position ' ' ++ "0" ++ replicate (tunnelLength - position - 1) ' ') >>
    putStrLn (replicate tunnelLength '_')

checkInventory :: Player -> IO ()
checkInventory player =
    putStrLn " ">>
    putStrLn "Inventory: " >>
    if Seq.null (inventory player)
        then putStrLn "Your inventory is empty" >> gamePlayMenu player
        else do
            mapM_ (\(i, (itemName, itemGrade)) ->
                putStrLn (show i ++ ". " ++ itemName ++ " (Grade " ++ [itemGrade] ++ ")"))
                (zip [1..] (toList (inventory player)))
            putStrLn "Press 'c' to change equipped items."
            putStrLn "Press 'd' to delete items from inventory."
            putStrLn "Press 'q' to return to previous menu."
            getLine >>= \case
                "c" -> equipItem player
                "d" -> deleteItem player
                "q" -> gamePlayMenu player
                _ -> checkInventory player

checkStore :: Player -> Store -> IO ()
checkStore player store =
    putStrLn " " >>
    putStrLn "Welcome to The Hunter Store! Here are the available items: " >>
    mapM_ (\(index, item) ->
        putStrLn $ show index ++ ". " ++ fst3 item ++ ": " ++ "Grade " ++ [snd3 item] ++ " - " ++ show (getPrice (Purchase item)) ++ " coins")
        (zip [1..] (items store)) >>
    putStrLn "Press the number of the item to buy, 's' to sell items, or 'q' to leave the store." >>
    getLine >>= storeInput player store

storeInput :: Player -> Store -> String -> IO ()
storeInput player store input
    | input == "q" = gamePlayMenu player
    | input == "s" = storeSell player store
    | otherwise = case readMaybe input :: Maybe Int of
        Just n ->
            if n > 0 && n <= length (items store)
                then let item = items store !! (n-1)
                         purchase = Purchase item
                     in case buy purchase player of
                        Just (_, updatedPlayer) -> do
                            putStrLn $ "You purchased " ++ fst3 item ++ " for " ++ show (getPrice purchase) ++ " coins."
                            putStrLn $ "Remaining money " ++ show (money updatedPlayer) ++ " coins."
                            gamePlayMenu updatedPlayer
                        Nothing -> do
                            putStrLn "You do not have enough money to purchase this item."
                            checkStore player store
            else do
                putStrLn "Invalid option. Please choose a valid number."
                checkStore player store
        Nothing -> do
            putStrLn "Invalid option. please enter a valid number."
            checkStore player store

storeSell :: Player -> Store -> IO ()
storeSell player store =
    if Seq.null (inventory player)
        then    putStrLn " " >>
                putStrLn "Your inventory is empty. There is nothing to sell." >> checkStore player store
        else do
            putStrLn " "
            putStrLn "Your inventory: "
            mapM_ (\(index, (itemName, itemGrade)) ->
                    putStrLn $ show index ++ ". " ++ itemName ++ " (Grade: " ++ show itemGrade ++ ")")
                (zip [1..] (toList (inventory player)))
            putStrLn "Enter the number of the item you want to sell or 'q' to quit."
            getLine >>= handleSellInput player store

handleSellInput :: Player -> Store -> String -> IO ()   
handleSellInput player store input
    | input == "q"  = gamePlayMenu player
    | otherwise = case readMaybe input :: Maybe Int of
        Just n ->
            if n > 0 && n <= length (inventory player)
            then do
                let maybeItemIndex = findIndexL (\(itemName, _) -> itemName == fst (inventory player `index` (n - 1))) (inventory player)
                case maybeItemIndex of
                    Just itemIndex -> do
                        let (itemToSell, _) = inventory player `index` itemIndex
                        if isEquipped (Equipment (itemToSell, snd(inventory player `index` (n - 1)))) player
                        then do
                            putStrLn "You cannot sell your equipped item. Please equip a different item before selling."
                            storeSell player store
                        else do
                            let sellPrice = 50
                                updatedPlayer = player {
                                    money = money player + sellPrice,
                                    inventory = deleteAt itemIndex (inventory player)
                                }
                            putStrLn $ "You sold " ++ itemToSell ++ " for " ++ show sellPrice ++ " coins."
                            putStrLn $ "Remaining money " ++ show (money updatedPlayer) ++ " coins."
                            gamePlayMenu updatedPlayer
                    Nothing -> do
                        putStrLn "Invalid item index."
                        storeSell player store
            else do
                putStrLn "Invalid option. Please choose a valid number of the item."
                storeSell player store
        Nothing -> do
            putStrLn "Invalid input. Please enter a valid number of the item."
            storeSell player store

equipItem :: Player -> IO ()
equipItem player = do
    putStrLn " "
    putStrLn "Choose your equipped weapon: "
    mapM_ putStrLn (zipWith (\i (item, grade) -> show i ++ ". " ++ item ++ " (Grade " ++ [grade] ++ ")") [1..] (toList (inventory player)))
    input <- getLine
    let item_index = read input :: Int
    if item_index > 0 && item_index <= length (inventory player)
    then do
        let selectedItem = inventory player `Seq.index` (item_index - 1)
        if isEquipped (Equipment selectedItem) player
        then do
            putStrLn "The item is already equipped."
            checkInventory player
        else do
            let updatedPlayer = equip (Equipment selectedItem) player
            putStrLn ("Equipped item changed to: " ++ fst (equippedItem updatedPlayer) ++ " (Grade " ++ [snd (equippedItem updatedPlayer)] ++ ")")
            gamePlayMenu updatedPlayer
    else do
        putStrLn "Item not in Inventory"
        equipItem player

deleteInventoryItem :: Player -> Int -> IO Player
deleteInventoryItem player delete_item_index
    | delete_item_index > 0 && delete_item_index <= Seq.length (inventory player)  =
        let itemToDelete = inventory player `Seq.index` (delete_item_index - 1)
        in if isEquipped (Equipment itemToDelete) player
            then
                putStrLn "You cannot delete currently equipped item. Please equip another item before deleting." >>
                return player
            else
                return player { inventory = Seq.deleteAt (delete_item_index - 1) (inventory player) }
    | otherwise = do
        putStrLn "Item not in Inventory"
        return player

deleteItem :: Player -> IO ()
deleteItem player =
    putStrLn " ">>
    putStrLn "Choose item to delete: " >>
    mapM_ putStrLn (zipWith (\i (item, _) -> show i ++ ". " ++ item) [1..] (toList (inventory player))) >>
    getLine >>= \input ->
        let delete_item_index = read input :: Int
        in deleteInventoryItem player delete_item_index >>= \updatedPlayer -> do
            putStrLn ("Updated Inventory: " ++ unwords (map fst (toList (inventory updatedPlayer)))) >>
                gamePlayMenu updatedPlayer


-- Player Data 
savePlayer :: Player -> IO ()
savePlayer player = do
    let filename = name player ++ ".txt"
    writeFile filename $ unlines
        [ name player
        , show (health player)
        , formatInventory (inventory player)  
        , show (energyLevel player)
        , [grade player]
        , formatEquippedItem (equippedItem player) 
        , show (money player)
        ]
    where
        formatInventory inv =
            if Seq.null inv
                then ""
                else intercalate "|" (toList $ fmap (\(n, g) -> n ++ "," ++ [g]) inv)


formatEquippedItem (itemName, itemGrade) = itemName ++ "," ++ [itemGrade]

loadPlayer :: String -> IO (Maybe Player)
loadPlayer playerName =
    doesFileExist filename >>= \case
      True ->
          readFile filename >>= \content ->
          case lines content of
              [pName, pHealth, pInventory, pEnergy, pGrade, pEquipped, pMoney] ->
                  pure $ Just Player
                      { name = pName
                      , health = read pHealth
                      , inventory = parseInventory pInventory
                      , energyLevel = read pEnergy
                      , grade = head pGrade
                      , equippedItem = parseItem pEquipped
                      , money = read pMoney
                      }
              _ -> pure Nothing
      False -> pure Nothing
  where
    filename = playerName ++ ".txt"

    parseInventory :: String -> Seq (String, Char)
    parseInventory "" = Seq.empty
    parseInventory invStr = fromList $ map parseItem (splitString '|' invStr)

    parseItem :: String -> (String, Char)
    parseItem itemStr =
      let (itemName, itemGrade) = break (== ',') itemStr
      in (itemName, head (tail itemGrade))


    splitString :: Char -> String -> [String]
    splitString delimiter str = case break (== delimiter) str of
      (first, rest)
        | null rest -> [first]
        | otherwise -> first : splitString delimiter (tail rest)

main :: IO ()
main = gameMainMenu >>= processgameMainMenu