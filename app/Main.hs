module Main (main) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless)
import Data.List (intercalate)
import Options.Applicative
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Exit (ExitCode (..), exitFailure)
import System.Process (system)
import TUI (selectToolsInteractive)
import Tool

-- CLI commands

data InstallTarget = One ToolName | All

data Command
    = Install (Maybe InstallTarget)
    | List

-- CLI parsing

parseCommand :: Parser Command
parseCommand =
    subparser
        ( command "install" (info (installParser <**> helper) (progDesc "Install tools (interactive if no tool specified)"))
            <> command "list" (info (pure List <**> helper) (progDesc "List available tools and their status"))
        )

installParser :: Parser Command
installParser = Install <$> optional (argument readInstallTarget (metavar "TOOL" <> help availableToolsHelp))

readInstallTarget :: ReadM InstallTarget
readInstallTarget = eitherReader $ \s ->
    case s of
        "all" -> Right All
        _ -> case lookup s nameMap of
            Just name -> Right (One name)
            Nothing ->
                Left $
                    "Unknown tool: "
                        ++ s
                        ++ "\nAvailable: all, "
                        ++ intercalate ", " (map toolNameStr allToolNames)
  where
    nameMap = map (\n -> (toolNameStr n, n)) allToolNames

availableToolsHelp :: String
availableToolsHelp = "Tool to install: all, " ++ intercalate ", " (map toolNameStr allToolNames)

opts :: ParserInfo Command
opts =
    info
        (parseCommand <**> helper)
        ( fullDesc
            <> progDesc "Manage development tool installations"
            <> header "dot - idempotent development environment setup"
        )

-- Execution

main :: IO ()
main = do
    cmd <- execParser opts
    case cmd of
        Install Nothing -> installInteractive
        Install (Just All) -> installAll
        Install (Just (One tool)) -> installTool tool
        List -> listTools

installAll :: IO ()
installAll = do
    putStrLn $ "Installing all " ++ show (length allToolNames) ++ " tool(s)...\n"
    forM_ allToolNames installTool

installInteractive :: IO ()
installInteractive = do
    selected <- selectToolsInteractive
    case selected of
        [] -> putStrLn "No tools selected."
        tools -> do
            putStrLn $ "\nInstalling " ++ show (length tools) ++ " tool(s)...\n"
            forM_ tools installTool

installTool :: ToolName -> IO ()
installTool name = do
    let tool = toolFor name
    installed <- toolCheck tool
    unless installed $ do
        putStrLn $ "  Installing " ++ toolDisplayName tool ++ "..."
        result <- try (toolInstall tool) :: IO (Either IOException ExitCode)
        case result of
            Right ExitSuccess ->
                putStrLn $ "  ✓ " ++ toolDisplayName tool ++ " installed"
            Right (ExitFailure code) -> do
                putStrLn $
                    "  ✗ "
                        ++ toolDisplayName tool
                        ++ " installation failed (exit code "
                        ++ show code
                        ++ ")"
                exitFailure
            Left err -> do
                putStrLn $
                    "  ✗ "
                        ++ toolDisplayName tool
                        ++ " installation failed: "
                        ++ show err
                exitFailure
    case toolStowPackage tool of
        Just pkg -> do
            ensureStow
            home <- getHomeDirectory
            dotfilesDir <- getCurrentDirectory
            stowOne dotfilesDir home pkg
        Nothing -> pure ()

listTools :: IO ()
listTools = do
    putStrLn "Available tools:\n"
    forM_ allToolNames $ \name -> do
        let tool = toolFor name
        installed <- toolCheck tool
        let status = if installed then "✓ installed" else "✗ not installed"
        putStrLn $
            "  "
                ++ toolDisplayName tool
                ++ " ["
                ++ status
                ++ "]"

ensureStow :: IO ()
ensureStow = do
    installed <- commandExists "stow"
    unless installed $ do
        putStrLn "  GNU Stow not found, installing via brew..."
        brewInstalled <- commandExists "brew"
        unless brewInstalled $ do
            putStrLn "  ✗ Homebrew is required but not installed. Run: dot install brew"
            exitFailure
        result <- system "brew install stow"
        case result of
            ExitSuccess -> putStrLn "  ✓ stow installed"
            ExitFailure code -> do
                putStrLn $ "  ✗ stow installation failed (exit code " ++ show code ++ ")"
                exitFailure

stowOne :: FilePath -> FilePath -> String -> IO ()
stowOne dotfilesDir home pkg = do
    putStrLn $ "  Stowing " ++ pkg ++ "..."
    result <- try (system $ "stow --adopt -d " ++ dotfilesDir ++ " -t " ++ home ++ " " ++ pkg) :: IO (Either IOException ExitCode)
    case result of
        Right ExitSuccess ->
            putStrLn $ "  ✓ " ++ pkg ++ " stowed"
        Right (ExitFailure code) -> do
            putStrLn $ "  ✗ " ++ pkg ++ " stow failed (exit code " ++ show code ++ ")"
            exitFailure
        Left err -> do
            putStrLn $ "  ✗ " ++ pkg ++ " stow failed: " ++ show err
            exitFailure
