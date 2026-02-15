module Tool
    ( ToolName (..)
    , Tool (..)
    , allToolNames
    , toolNameStr
    , toolFor
    , commandExists
    ) where

import Data.Maybe (isJust)
import System.Directory (doesDirectoryExist, findExecutable, getCurrentDirectory)
import System.Exit (ExitCode (..))
import System.Process (system)

data ToolName = Brew | Zsh | Gpg | Tmux | Bun | Ghostty | Claude | Codex | Haskell | Infuse | Rust | Chrome | OnePassword | VSCode | Telegram | Zed | Discord | OrbStack | ClashX
    deriving (Show, Eq, Bounded, Enum)

data Tool = Tool
    { toolDisplayName :: String
    , toolCheck :: IO Bool
    , toolInstall :: IO ExitCode
    , toolStowPackage :: Maybe String
    }

allToolNames :: [ToolName]
allToolNames = [minBound .. maxBound]

toolNameStr :: ToolName -> String
toolNameStr Brew = "brew"
toolNameStr Rust = "rust"
toolNameStr Bun = "bun"
toolNameStr Ghostty = "ghostty"
toolNameStr Claude = "claude"
toolNameStr Codex = "codex"
toolNameStr Haskell = "haskell"
toolNameStr Tmux = "tmux"
toolNameStr Gpg = "gpg"
toolNameStr Zsh = "zsh"
toolNameStr Infuse = "infuse"
toolNameStr Chrome = "chrome"
toolNameStr OnePassword = "1password"
toolNameStr VSCode = "vscode"
toolNameStr Telegram = "telegram"
toolNameStr Zed = "zed"
toolNameStr Discord = "discord"
toolNameStr OrbStack = "orbstack"
toolNameStr ClashX = "clashx"

toolFor :: ToolName -> Tool
toolFor Brew =
    Tool
        { toolDisplayName = "Homebrew"
        , toolCheck = commandExists "brew"
        , toolInstall =
            system "/bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\""
        , toolStowPackage = Nothing
        }
toolFor Rust =
    Tool
        { toolDisplayName = "Rust"
        , toolCheck = commandExists "rustc"
        , toolInstall =
            system "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y"
        , toolStowPackage = Nothing
        }
toolFor Bun =
    Tool
        { toolDisplayName = "Bun"
        , toolCheck = commandExists "bun"
        , toolInstall = system "curl -fsSL https://bun.sh/install | bash"
        , toolStowPackage = Nothing
        }
toolFor Ghostty =
    Tool
        { toolDisplayName = "Ghostty"
        , toolCheck = doesDirectoryExist "/Applications/Ghostty.app"
        , toolInstall = system "brew install --cask ghostty"
        , toolStowPackage = Just "ghostty"
        }
toolFor Claude =
    Tool
        { toolDisplayName = "Claude"
        , toolCheck = commandExists "claude"
        , toolInstall = system "curl -fsSL https://claude.ai/install.sh | bash"
        , toolStowPackage = Nothing
        }
toolFor Codex =
    Tool
        { toolDisplayName = "Codex"
        , toolCheck = commandExists "codex"
        , toolInstall = system "bun install -g @openai/codex"
        , toolStowPackage = Nothing
        }
toolFor Haskell =
    Tool
        { toolDisplayName = "Haskell"
        , toolCheck = commandExists "ghcup"
        , toolInstall =
            system "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
        , toolStowPackage = Nothing
        }
toolFor Tmux =
    Tool
        { toolDisplayName = "Tmux"
        , toolCheck = commandExists "tmux"
        , toolInstall = system "brew install tmux"
        , toolStowPackage = Just "tmux"
        }
toolFor Gpg =
    Tool
        { toolDisplayName = "GPG"
        , toolCheck = commandExists "gpg"
        , toolInstall = do
            dotfilesDir <- getCurrentDirectory
            system $
                "brew install gnupg pinentry-mac"
                    ++ " && gpg --import "
                    ++ dotfilesDir
                    ++ "/gnupg/public.asc"
        , toolStowPackage = Just "gnupg"
        }
toolFor Zsh =
    Tool
        { toolDisplayName = "Zsh"
        , toolCheck = commandExists "zsh"
        , toolInstall = pure ExitSuccess
        , toolStowPackage = Just "zsh"
        }
toolFor Infuse =
    Tool
        { toolDisplayName = "Infuse"
        , toolCheck = doesDirectoryExist "/Applications/Infuse.app"
        , toolInstall = system "mas install 1136220934"
        , toolStowPackage = Nothing
        }

toolFor Chrome =
    Tool
        { toolDisplayName = "Google Chrome"
        , toolCheck = doesDirectoryExist "/Applications/Google Chrome.app"
        , toolInstall = system "brew install --cask google-chrome"
        , toolStowPackage = Nothing
        }

toolFor OnePassword =
    Tool
        { toolDisplayName = "1Password"
        , toolCheck = doesDirectoryExist "/Applications/1Password.app"
        , toolInstall = system "brew install --cask 1password"
        , toolStowPackage = Nothing
        }

toolFor VSCode =
    Tool
        { toolDisplayName = "Visual Studio Code"
        , toolCheck = doesDirectoryExist "/Applications/Visual Studio Code.app"
        , toolInstall = system "brew install --cask visual-studio-code"
        , toolStowPackage = Nothing
        }

toolFor Telegram =
    Tool
        { toolDisplayName = "Telegram"
        , toolCheck = doesDirectoryExist "/Applications/Telegram.app"
        , toolInstall = system "brew install --cask telegram"
        , toolStowPackage = Nothing
        }

toolFor Zed =
    Tool
        { toolDisplayName = "Zed"
        , toolCheck = doesDirectoryExist "/Applications/Zed.app"
        , toolInstall = system "brew install --cask zed"
        , toolStowPackage = Nothing
        }

toolFor Discord =
    Tool
        { toolDisplayName = "Discord"
        , toolCheck = doesDirectoryExist "/Applications/Discord.app"
        , toolInstall = system "brew install --cask discord"
        , toolStowPackage = Nothing
        }

toolFor OrbStack =
    Tool
        { toolDisplayName = "OrbStack"
        , toolCheck = doesDirectoryExist "/Applications/OrbStack.app"
        , toolInstall = system "brew install --cask orbstack"
        , toolStowPackage = Nothing
        }

toolFor ClashX =
    Tool
        { toolDisplayName = "ClashX Meta"
        , toolCheck = doesDirectoryExist "/Applications/ClashX Meta.app"
        , toolInstall =
            system $
                "curl -fsSL -o /tmp/ClashX.Meta.zip"
                    ++ " $(curl -fsSL https://api.github.com/repos/MetaCubeX/ClashX.Meta/releases/latest"
                    ++ " | grep 'browser_download_url.*ClashX.Meta.zip'"
                    ++ " | head -1 | cut -d '\"' -f 4)"
                    ++ " && unzip -o /tmp/ClashX.Meta.zip -d /tmp/ClashX.Meta"
                    ++ " && mv '/tmp/ClashX.Meta/ClashX Meta.app' /Applications/"
                    ++ " && rm -rf /tmp/ClashX.Meta /tmp/ClashX.Meta.zip"
        , toolStowPackage = Nothing
        }

commandExists :: String -> IO Bool
commandExists cmd = isJust <$> findExecutable cmd
