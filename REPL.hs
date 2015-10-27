{-|
Module      : REPL
Description : Read Evaluate Print Loop assistant
Copyright   : (c) Dawson Conway 2015
License     : GPL-3
Maintainer  : dconway2@ku.edu
Stability   : experimental
Portability : portable

Still working
-}
module REPL (
EvalFunc,
EndLoop,
repl,
replr
) where

{-|
The 'Eval' type holds information that handles evaluation of user input. It is designed for use with 'repl' and 'replr'.


A value of type @'Eval' a@ can either be 'Endloop'
or @'EvalFunc' f@ where f is a function of form @(('String',a) -> ('String', a, 'Eval' a)@.
'Endloop' represents a termination of the REPL cycle, ending after the print step.
The function in @'EvalFunc' f@ is designed to take a tuple of the user input and the current state and return a tuple of the prompt, state, and 'Eval' for the next loop.

==== __Examples__

>>> let repeatXTimes = EvalFunc (\(input, x) -> if x==0 then ("Okay, I'm done\n", 0, Endloop) else (input ++ "\n>", x-1, repeatXTimes))
>>> repl ">" 4 repeatXTimes
>Hello
Hello
>Who are you?
Who are you?
>I asked first!
I asked first!
>Are you just repeating me?
Are you just repeating me?
>Stop!
Okay,I'm done
>>>


>>> let evaluate = EvalFunc (\(input,(x,y,z)) -> case input of 
>>|                                    "quit" -> ("Goodbye\n",            (x,y,z), Endloop )
>>|                                    "incX" -> ("Adding one to X\n",  (x+1,y,z), evaluate)
>>|                                    "incY" -> ("Adding one to Y\n",  (x,y+1,z), evaluate)
>>|                                    "incZ" -> ("Adding one to Z\n",  (x,y,z+1), evaluate)
>>|                                    _      -> ("I don't understand\n", (x,y,z), evaluate))
>>|
>>> replr "All zero\n" (0,0,0) evaluate
All zero
incX
Adding one to X\n

-}
data Eval a = EvalFunc ((String,a) -> (String,a,Eval a)) | Endloop

--| Begin the REPL execution, do not return the final state
repl :: String      -- ^ The initial prompt for the user
     -> state       -- ^ The initial state of the program
     -> Eval state  -- ^ 'Eval' holding the function for evaluating the user input
     -> IO ()       -- ^ no return information
repl out x ef = replr out x ef >> return ()

--| Begin the REPL execution, return the final state wrapped in an 'IO'
replr :: String      -- ^ The initial prompt for the user
      -> state       -- ^ The initial state of the program
      -> Eval state  -- ^ 'Eval' holding the function for evaluating the user input
      -> IO state    -- ^ the final state, wrapped in an 'IO'
replr out x Endloop  = putStr out >> return x
replr out x (EvalFunc f) = putStr out >>
                           getLine >>=
                           \input -> let (out',x',e) =  f (input,x)
                                      in  replr out' x' e
