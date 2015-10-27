

import Prelude
import System.Random(randomR, getStdRandom)
import Data.Char(toLower)
--           (X  , Y  , Health, Key , Sword, map     )
type State = (Int, Int, Int   , Bool, Bool , [[Room]])

data RoomType = Desert | Mountain | Forest | Dungeon | Road | Gate

--          (RoomType, Enemy, Key , Sword, Heal, Message)
type Room = (RoomType, Enemy, Bool, Bool , Bool, String )

data Command = Attack | Block | Feint | Flee | Error deriving (Enum)

--           (Name  , Health, Strength)
type Enemy = (String, Int   , Int     )


getEnemyAttack :: IO Command
getEnemyAttack = f <$> getStdRandom (randomR (1,3::Int))
                  where f 1 = Attack
                        f 2 = Block
                        f 3 = Feint

fight :: String -> Command -> Command -> IO Ordering
fight s Attack Attack = putStrLn ("You and the " ++ s ++ " both attack. Neither one of you lands a good hit.")
                        *> pure EQ
fight s Attack Block  = putStrLn ("You attack, but the " ++ s ++ " blocks it. It manages to hit you with a counter-attack.")
                        *> pure LT
fight s Attack Feint  = putStrLn ("The " ++ s ++ " tries to feint, but you see though the ruse and hit it.")
                        *> pure GT
fight s Block  Attack = putStrLn ("The " ++ s ++ " attacks. You are able to block the attack, and manage to land an attack of your own.")
                        *> pure GT
fight s Block  Block  = putStrLn ("You and the " ++ s ++ " both stands defensively. No one makes an attack.")
                        *> pure EQ
fight s Block  Feint  = putStrLn ("The " ++ s ++ " feints. You try to block the feint and are hit from the other side.")
                        *> pure LT
fight s Feint  Attack = putStrLn ("You try to feint, but the " ++ s ++ " isn't fooled. It catches you of balance and hits you.")
                        *> pure LT
fight s Feint  Block  = putStrLn ("You feint left. The " ++ s ++ " tries to block, not realizing the real attack is coming from the right. You land a solid hit.")
                        *> pure EQ
fight s Feint  Feint  = putStrLn ("You and the " ++ s ++ " both try to feint, but neither of you makes an actual attack.")
                        *> pure EQ
fight s Flee   _      = putStrLn ("You decide it's best to live to fight another day. The " ++ s ++ " lands an attack on you as you flee")
                        *> pure LT


evalCombat :: State -> Enemy -> Command -> IO ()
evalCombat (x,y,h,k,s,m) (name,health,strength) com = getEnemyAttack
                                                      >>= fight name com
                                                      >>= \ord -> let d = if s then 15 else 5
                                                            in case ord of
                                                              EQ -> readCombat (x,y,h,k,s,m) (name,health,strength)
                                                              GT -> if health > d
                                                                then readCombat (x,y,h,k,s,m) (name,health-d,strength)
                                                                else winCombat (x,y,h,k,s,m) name
                                                              LT -> if h > strength
                                                                then readCombat (x,y,h-strength,k,s,m) (name,health,strength)
                                                                else loseCombat name

readCombat :: State -> Enemy -> IO ()
readCombat st en = putStrLn "What do you do?" 
                   >>  getLine
                   >>= evalCombat st en . parseCombat

parseCombat :: String -> Command
parseCombat s = case toLower <$> s of
                  "flee"   -> Flee
                  "feint"  -> Feint
                  "block"  -> Block
                  "attack" -> Attack
                  _        -> Error

loseCombat :: String -> IO ()
loseCombat s = putStrLn ("You fell prey to the mighty " ++ s ++ ". Better luck next time!")

winCombat :: State -> String -> IO ()
winCombat st s = putStrLn "Yeah"


{-
*****^D^00000
******g000000
******r000000
******r000000
******r000000
******r000000

-}
