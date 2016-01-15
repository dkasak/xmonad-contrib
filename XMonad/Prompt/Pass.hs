-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Pass
-- Copyright   :  (c) 2014 Igor Babuschkin, Antoine R. Dumont, Denis Kasak
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Antoine R. Dumont <eniotna.t@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- This module provides 3 <XMonad.Prompt> to ease passwords manipulation (generate, read, remove):
--
-- - one to lookup passwords in the password-storage.
--
-- - one to generate a password for a given password label that the user inputs.
--
-- - one to delete a stored password for a given password label that the user inputs.
--
-- All those prompts benefit from the completion system provided by the module <XMonad.Prompt>.
--
-- The password store is setuped through an environment variable PASSWORD_STORE_DIR.
-- If this is set, use the content of the variable.
-- Otherwise, the password store is located on user's home @$HOME\/.password-store@.
--
--
-- Source:
--
-- - The password storage implementation is <http://git.zx2c4.com/password-store the password-store cli>.
--
-- - Inspired from <http://babushk.in/posts/combining-xmonad-and-pass.html>
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Pass (
                            -- * Usages
                            -- $usages
                              passPrompt
                            , passGeneratePrompt
                            , passRemovePrompt
                            ) where

import Control.Monad (liftM, mfilter, join)
import Data.List (sort, isPrefixOf)
import XMonad.Core
import XMonad.Prompt ( XPrompt
                     , XPType(..)
                     , showXPrompt
                     , commandToComplete
                     , nextCompletion
                     , getNextCompletion
                     , completionFunction
                     , modeAction
                     , XPConfig
                     , mkXPrompt
                     , mkXPromptWithModes)
import System.Directory (getHomeDirectory, doesDirectoryExist, getDirectoryContents)
import System.FilePath (takeExtension, takeDirectory, takeFileName,
                        dropExtension, combine, (</>), addTrailingPathSeparator)
import System.Posix.Env (getEnv)
import XMonad.Util.Run (safeSpawn)

-- $usages
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt.Pass
--
-- Then add a keybinding for 'passPrompt', 'passGeneratePrompt' or 'passRemovePrompt':
--
-- >   , ((modMask x , xK_p)                              , passPrompt xpconfig)
-- >   , ((modMask x .|. controlMask, xK_p)               , passGeneratePrompt xpconfig)
-- >   , ((modMask x .|. controlMask  .|. shiftMask, xK_p), passRemovePrompt xpconfig)
--
-- For detailed instructions on:
--
-- - editing your key bindings, see "XMonad.Doc.Extending#Editing_key_bindings".
--
-- - how to setup the password storage, see <http://git.zx2c4.com/password-store/about/>
--

data PassGet         = PassGet
data PassGetUsername = PassGetUsername
data PassGetURL      = PassGetURL
data PassGenerate    = PassGenerate
data PassRemove      = PassRemove

instance XPrompt PassGenerate where
  showXPrompt       _   = "Generate pass: "
  commandToComplete _ c = c
  nextCompletion    _   = getNextCompletion

instance XPrompt PassRemove where
  showXPrompt       _   = "Remove pass: "
  commandToComplete _ c = c
  nextCompletion    _   = getNextCompletion

instance XPrompt PassGet where
  showXPrompt        _ = "Pass: "
  commandToComplete  _ = id
  completionFunction _ = passComplete
  nextCompletion     _ = getNextCompletion
  modeAction     _ c _ = selectPassword c

instance XPrompt PassGetUsername where
  showXPrompt        _ = "Pass (username): "
  commandToComplete  _ = id
  completionFunction _ = passComplete
  nextCompletion     _ = getNextCompletion
  modeAction     _ c _ = selectUsername c

instance XPrompt PassGetURL where
  showXPrompt        _ = "Pass (URL): "
  commandToComplete  _ = id
  completionFunction _ = passComplete
  nextCompletion     _ = getNextCompletion
  modeAction     _ c _ = selectURL c

-- | Default password store folder in $HOME/.password-store
--
passwordStoreFolderDefault :: String -> String
passwordStoreFolderDefault home = combine home ".password-store"

-- | Compute the password store's location.
-- Use the PASSWORD_STORE_DIR environment variable to set the password store.
-- If empty, return the password store located in user's home.
--
passwordStoreFolder :: IO String
passwordStoreFolder =
  getEnv "PASSWORD_STORE_DIR" >>= computePasswordStoreDir
  where computePasswordStoreDir Nothing         = liftM passwordStoreFolderDefault getHomeDirectory
        computePasswordStoreDir (Just storeDir) = return storeDir


addTrailingIfDir :: FilePath -> FilePath -> IO FilePath
addTrailingIfDir path name = do
    exists <- doesDirectoryExist $ path </> name
    return $ if exists
        then addTrailingPathSeparator name
        else name

passComplete :: String -> IO [String]
passComplete s = do
    store <- passwordStoreFolder
    let path = store </> s
    exists <- doesDirectoryExist path
    if exists
       then let contents = fmap (s </>) <$> getDirectoryContents' path
                completions = join $ mapM (addTrailingIfDir store) <$> contents
            in sort <$> completions
       else let dir = takeDirectory path
                initSegment' = takeDirectory s
                initSegment = if initSegment' == "." then "" else initSegment'
                lastSegment = takeFileName s
                contents = getDirectoryContents' dir
                completions = fmap (initSegment </>)
                            . filter (isPrefixOf lastSegment)
                          <$> contents
            in join $ mapM (addTrailingIfDir store) <$> completions

getDirectoryContents' :: FilePath -> IO [FilePath]
getDirectoryContents' dir = fmap removeGpgExtension <$> dirContents
    where dirContents' = getDirectoryContents dir
          dirContents  = mfilter (not . isPrefixOf ".") <$> dirContents'

-- | A prompt to retrieve a password from a given entry.
--
passPrompt :: XPConfig -> X ()
passPrompt = mkXPromptWithModes [XPT PassGet, XPT PassGetUsername, XPT PassGetURL]

-- | A prompt to generate a password for a given entry.
-- This can be used to override an already stored entry.
-- (Beware that no confirmation is asked)
--
passGeneratePrompt :: XPConfig -> X ()
passGeneratePrompt xpconfig = mkXPrompt PassGenerate xpconfig passComplete generatePassword

-- | A prompt to remove a password for a given entry.
-- (Beware that no confirmation is asked)
--
passRemovePrompt :: XPConfig -> X ()
passRemovePrompt xpconfig = mkXPrompt PassRemove xpconfig passComplete removePassword

-- | Select a password.
--
selectPassword :: String -> X ()
selectPassword passLabel = safeSpawn "pass" ["--clip", passLabel]

-- | Select a username paired with a password.
--
selectUsername :: String -> X ()
selectUsername passLabel = safeSpawn "pass-username" ["-c", passLabel]

-- | Select a URL paired with a password.
--
selectURL :: String -> X ()
selectURL passLabel = safeSpawn "pass-url" ["-c", passLabel]

-- | Generate a 30 characters password for a given entry.
-- If the entry already exists, it is updated with a new password.
--
generatePassword :: String -> X ()
generatePassword passLabel = safeSpawn "pass" ["generate", "--force", passLabel, "30"]

-- | Remove a password stored for a given entry.
--
removePassword :: String -> X ()
removePassword passLabel = safeSpawn "pass" ["rm", "--force", passLabel]

removeGpgExtension :: String -> String
removeGpgExtension file | takeExtension file == ".gpg" = dropExtension file
                        | otherwise                    = file
