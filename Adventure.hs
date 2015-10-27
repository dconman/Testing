import Prelude
import System.Random(randomR, getStdRandom)
import Data.Char(toLower)
--           (X  , Y  , Life, Key , Sword, map     )
type State = (Int, Int, Int , Bool, Bool , [[Room]])

data RoomType = Desert | Mountain | Forest | Dungeon | Road | Gate

--          (RoomType, Enemy, Key , Sword, Heal, Message)
type Room = (RoomType, Enemy, Bool, Bool , Bool, String )

data Command = Go Dir | Search | Attack | Block | Feint | Flee | Error deriving (Enum)

data Dir = N | E | S | W

--           (Name  , Health, Strength, Attacks)
type Enemy = (String, Int   , Int     , [Int])


updateEnemy' :: Int -> Enemy -> Map
updateEnemy' 0 en ((t,_,k,s,h,m):rs) = (t,en,k,s,h,m):rs
updateEnemy' y en (r:rs)             = r:(updateEnemy' y-1 en rs)

updateEnemy :: Int -> Int -> Enemy -> Map
updateEnemy 0 y en (rs:rss) = (updateEnemy' y en rs):rss
updateEnemy x y en (rs:rss) = sr:(updateEnemy x-1 y en rss)


generateEnemyAttack :: IO [Command]
generateEnemyAttack = let f 1 = Attack
                          f 2 = Block
                          f 3 = Feint
                      in f <$> getStdRandom (randomRs (1,3::Int))

fight :: String -> Command -> Command -> IO ()
fight (x,y,l,k,s,m) (n,h,str,coms) c1 = let d = if s then 15 else 5 
      case (c1, head coms) of
       (Attack,Attack) -> putStrLn ("You and the " ++ n ++ " both attack. Neither one of you lands a good hit.")
                          >> readCombat (x,y,l,k,s,m) (n,h,str,coms)
       (Attack,Block)  -> putStrLn ("You attack, but the " ++ n ++ " blocks it. It manages to hit you with a counter-attack.")
                         >> readCombat (x,y,max 0 l-str,k,s,m) (n,h,str,coms)
       (Attack,Feint)  -> putStrLn ("The " ++ n ++ " tries to feint, but you see though the ruse and hit it.")
                         >> readCombat (x,y,l,k,s,m) (n,max 0 h-l,str,coms)
       (Block,Attack)  -> putStrLn ("The " ++ n ++ " attacks. You are able to block the attack, and manage to land an attack of your own.")
                         >> readCombat (x,y,l,k,s,m) (n,max 0 h-d,str,coms)
       (Block,Block)   -> putStrLn ("You and the " ++ n ++ " both stands defensively. No one makes an attack.")
                         >> readCombat (x,y,l,k,s,m) (n,h,str,coms)
       (Block,Feint)   ->putStrLn ("The " ++ n ++ " feints. You try to block the feint and are hit from the other side.")
                         >> readCombat (x,y,max 0 l-str,k,s,m) (n,h,str,coms)
       (Feint,Attack)  -> putStrLn ("You try to feint, but the " ++ n ++ " isn't fooled. It catches you of balance and hits you.")
                         >> readCombat (x,y,max 0 l-str,k,s,m) (n,h,str,coms)
       (Feint,Block)   -> putStrLn ("You feint left. The " ++ n ++ " tries to block, not realizing the real attack is coming from the right. You land a solid hit.")
                         >> readCombat (x,y,l,k,s,m) (n,max 0 h-d,str,coms)
       (Feint,Feint)   -> putStrLn ("You and the " ++ n ++ " both try to feint, but neither of you makes an actual attack.")
                         >> readCombat (x,y,l,k,s,m) (n,h,str,coms)
                        
flee :: State -> Enemy -> IO ()
flee (x,y,l,k,s,m) (n,_,str,_) = if 2*str > l
                                   then putStrLn ("The " ++ n ++ " has defeated you!") >> return ()
                                   else if x != 0
                                           then putStrLn "You flee south!" >> mainEval (x,y,l-(2*str),k,s,m) (Go S)
                                           else if y > startX 
                                                  then putStrLn "You flee west!" >> mainEval (x,y,l-(2*str),k,s,m) (Go W)
                                                  else putStrLn "You flee east!" >> mainEval (x,y,l-(2*str),k,s,m) (Go E)


evalCombat :: State -> Enemy -> Command -> IO ()
evalCombat st en Error = putStrLn "You must Attack, Block, Feint, or Flee!" >> readCombat st en
evalCombat st en Flee  = flee st en
evalCombat st en Fight = fight st en

readCombat :: State -> Enemy -> IO ()
readCombat (x,y,0,k,s,m) (n,_,_,_)  = putStrLn ("The " ++ n ++ " has defeated you!") >> return ()
readCombat (x,y,l,k,s,m) (n,0,_,_)  = putStrLn ("You have defeated the " ++ n ++ "!") >> mainEval (x,y,l,k,s,updateMap x y ("",0,0,[])) Search
readCombat st            en         = putStrLn "What do you do?" >>
                                      getLine >>=
                                      evalCombat st en . parseCombat

parseCombat :: String -> Command
parseCombat s = case toLower <$> s of
                  "flee"   -> Flee
                  "feint"  -> Feint
                  "block"  -> Block
                  "attack" -> Attack
                  _        -> Error


mainEval = undefined


{-
*****^D^00000
******g000000
******r000000
******r000000
******r000000
******r000000
-}
