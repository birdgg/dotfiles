module TUI (selectToolsInteractive) where

import Control.Exception (finally)
import Control.Monad (forM_)
import System.IO
    ( BufferMode (..)
    , hFlush
    , hGetBuffering
    , hGetChar
    , hGetEcho
    , hReady
    , hSetBuffering
    , hSetEcho
    , stdin
    , stdout
    )
import Tool (ToolName, allToolNames, toolCheck, toolDisplayName, toolFor)

-- Types

data Key
    = KeyUp
    | KeyDown
    | KeyEnter
    | KeySpace
    | KeyEsc
    | KeyChar Char

data SelectItem = SelectItem
    { itemName :: ToolName
    , itemDisplayName :: String
    , itemInstalled :: Bool
    , itemSelected :: Bool
    }

data AppState = AppState
    { stItems :: [SelectItem]
    , stCursor :: Int
    }

-- Entry point

selectToolsInteractive :: IO [ToolName]
selectToolsInteractive = do
    items <- mapM mkItem allToolNames
    let initial = AppState{stItems = items, stCursor = 0}
    oldBuffering <- hGetBuffering stdin
    oldEcho <- hGetEcho stdin
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    putStr "\ESC[?25l" -- hide cursor
    hFlush stdout
    result <-
        loop initial `finally` do
            hSetBuffering stdin oldBuffering
            hSetEcho stdin oldEcho
            putStr "\ESC[?25h" -- show cursor
            clearScreen
    pure result

mkItem :: ToolName -> IO SelectItem
mkItem name = do
    let tool = toolFor name
    installed <- toolCheck tool
    pure
        SelectItem
            { itemName = name
            , itemDisplayName = toolDisplayName tool
            , itemInstalled = installed
            , itemSelected = False
            }

-- Main loop

loop :: AppState -> IO [ToolName]
loop st = do
    render st
    k <- getKey
    case k of
        KeyChar 'q' -> pure []
        KeyEsc -> pure []
        KeyEnter -> pure [itemName item | item <- stItems st, itemSelected item]
        KeySpace -> loop (toggleCurrent st)
        KeyChar 'a' -> loop (toggleAll st)
        KeyChar 'j' -> loop (moveCursor 1 st)
        KeyChar 'k' -> loop (moveCursor (-1) st)
        KeyDown -> loop (moveCursor 1 st)
        KeyUp -> loop (moveCursor (-1) st)
        KeyChar '\t' -> loop (moveCursor 1 (toggleCurrent st))
        _ -> loop st

-- Input

getKey :: IO Key
getKey = do
    c <- hGetChar stdin
    case c of
        '\ESC' -> do
            ready <- hReady stdin
            if ready
                then do
                    c2 <- hGetChar stdin
                    case c2 of
                        '[' -> do
                            c3 <- hGetChar stdin
                            pure $ case c3 of
                                'A' -> KeyUp
                                'B' -> KeyDown
                                _ -> KeyChar c
                        _ -> pure KeyEsc
                else pure KeyEsc
        '\n' -> pure KeyEnter
        ' ' -> pure KeySpace
        _ -> pure (KeyChar c)

-- Rendering

clearScreen :: IO ()
clearScreen = do
    putStr "\ESC[2J\ESC[H"
    hFlush stdout

render :: AppState -> IO ()
render st = do
    clearScreen
    putStrLn ""
    putStrLn $ "  " ++ bold "dot install" ++ "\n"
    forM_ (zip [0 :: Int ..] (stItems st)) $ \(idx, item) -> do
        let cur = idx == stCursor st
        let pointer = if cur then " > " else "   "
        let circle =
                if itemSelected item
                    then green "● "
                    else dim "○ "
        let name = pad 14 (itemDisplayName item)
        let status =
                if itemInstalled item
                    then green "✓ installed"
                    else red "✗ not installed"
        if cur
            then putStrLn $ bold $ pointer ++ circle ++ name ++ status
            else putStrLn $ pointer ++ circle ++ name ++ status
    putStrLn ""
    putStrLn $
        "  "
            ++ key " Space "
            ++ " Toggle  "
            ++ key " Enter "
            ++ " Install  "
            ++ key " a "
            ++ " All  "
            ++ key " q "
            ++ " Quit"
    putStrLn ""
    hFlush stdout

-- State updates (immutable)

toggleCurrent :: AppState -> AppState
toggleCurrent st =
    let idx = stCursor st
        items =
            zipWith
                ( \i item ->
                    if i == idx
                        then item{itemSelected = not (itemSelected item)}
                        else item
                )
                [0 :: Int ..]
                (stItems st)
     in st{stItems = items}

toggleAll :: AppState -> AppState
toggleAll st =
    let allSel = all itemSelected (stItems st)
        items = map (\item -> item{itemSelected = not allSel}) (stItems st)
     in st{stItems = items}

moveCursor :: Int -> AppState -> AppState
moveCursor delta st =
    let len = length (stItems st)
        new = (stCursor st + delta) `mod` len
     in st{stCursor = new}

-- ANSI helpers

bold :: String -> String
bold s = "\ESC[1m" ++ s ++ "\ESC[0m"

dim :: String -> String
dim s = "\ESC[2m" ++ s ++ "\ESC[0m"

green :: String -> String
green s = "\ESC[32m" ++ s ++ "\ESC[0m"

red :: String -> String
red s = "\ESC[31m" ++ s ++ "\ESC[0m"

key :: String -> String
key s = "\ESC[7m" ++ s ++ "\ESC[0m"

pad :: Int -> String -> String
pad n s = s ++ replicate (max 0 (n - length s)) ' '

