{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
module Craft.Craftable where

import           Control.Lens
import           Control.Monad.Reader     (ask)
import qualified Data.ByteString.Lazy     as BL
import           Data.List                (intercalate)
import           Data.Maybe               (catMaybes, isJust)
-- import           Data.Monoid
import           Formatting               hiding (char)
import qualified Path

import           Craft.DSL
import qualified Craft.File               as File
import           Craft.Helpers
import           Craft.Internal.Helpers
import           Craft.Internal.UserGroup
import           Craft.Types
import qualified Craft.User               as User


data Watched
  = Unchanged
  | Created
  | Updated
  | Removed
  deriving (Eq, Show)
makePrisms ''Watched

{-----------------------------------
-- This instance might be unsound.
--- What to do about `mappend Created Removed`?
instance Monoid Watched where
  mempty = Unchanged
  mappend a         Unchanged = a
  mappend Unchanged a         = a
  mappend Created   Removed   = Updated???
  mappend Removed   Created   = Updated???
  mappend Created   Updated   = Created
  mappend Updated   Created   = Created
  mappend Updated   Removed   = Removed
  mappend Removed   Updated   = Removed
  mappend a b | a == b        = a
              | otherwise     = error "Impossible!"
-----------------------------------}


changed :: Watched -> Bool
changed = not . unchanged


unchanged :: Watched -> Bool
unchanged Unchanged = True
unchanged _         = False


created :: Watched -> Bool
created Created = True
created _       = False


updated :: Watched -> Bool
updated Updated = True
updated _       = False


removed :: Watched -> Bool
removed Removed = True
removed _       = False


watch :: Eq a => Craft (Maybe a) -> Craft b -> Craft (Watched, Maybe a, b)
watch getter action = do
  !beforeMB <- getter
  !result   <- action
  !afterMB  <- getter
  let (w, x) = case (beforeMB, afterMB) of
                 (Nothing,     Nothing)    -> (Unchanged, Nothing)
                 (Just y,      Nothing)    -> (Removed,   Just y)
                 (Nothing,     Just y)     -> (Created,   Just y)
                 (Just before, Just after) -> if before == after
                                                then (Unchanged, Just after)
                                                else (Updated,   Just after)
  return (w, x, result)


watch_ :: Eq a => Craft (Maybe a) -> Craft b -> Craft Watched
watch_ getter action = do
  (w, _, _) <- watch getter action
  return w


class Craftable a b | a -> b where
  watchCraft :: a -> Craft (Watched, b)

  craft :: a -> Craft b
  craft x = snd <$> watchCraft x

  craft_ :: a -> Craft ()
  craft_ = void . craft

  watchCraft_ :: a -> Craft Watched
  watchCraft_ x = fst <$> watchCraft x

  {-# MINIMAL watchCraft #-}


class Destroyable a where
  watchDestroy :: a -> Craft (Watched, Maybe a)

  destroy :: a -> Craft (Maybe a)
  destroy x = snd <$> watchDestroy x

  destroy_ :: a -> Craft ()
  destroy_ = void . destroy

  watchDestroy_ :: a -> Craft Watched
  watchDestroy_ x = fst <$> watchDestroy x

  {-# MINIMAL watchDestroy #-}


instance Craftable a b => Craftable [a] [b] where
  craft_ = mapM_ craft_
  craft = mapM craft
  watchCraft as = do
    (ws, bs) <- unzip <$> mapM watchCraft as
    let w = if all (== Unchanged) ws then
              Unchanged
            else if all (== Created) ws then
              Created
            else
              Updated
    return (w, bs)


instance Destroyable a => Destroyable [a] where
  destroy_ = mapM_ destroy_
  destroy xs = do
    rs <- mapM destroy xs
    return $ case catMaybes rs of
               []  -> Nothing
               rs' -> Just rs'
  watchDestroy xs = do
    (ws, rs) <- unzip <$> mapM watchDestroy xs
    let w = if all (== Unchanged) ws then
              Unchanged
            else if all (== Removed) ws then
              Removed
            else
              Updated
    let res = case catMaybes rs of
                []  -> Nothing
                rs' -> Just rs'
    return (w, res)


instance Craftable User.UserOptions User where
  watchCraft uopts = do
    let notfound = "craft `"++uopts ^. User.optName++"` failed. Not Found!"
    let name = UserName $ uopts ^. User.optName
    User.fromName name >>= \case
      Nothing           -> do
        User.createUser uopts
        User.fromName name >>= \case
          Nothing          -> $craftError notfound
          Just createdUser -> do
            madeChanges <- User.ensureUserOpts createdUser uopts
            if not madeChanges
              then return (Created, createdUser)
              else User.fromName name >>= \case
                Nothing -> $craftError notfound
                Just u  -> return (Created, u)
      Just existingUser -> do
        madeChanges <- User.ensureUserOpts existingUser uopts
        if not madeChanges
          then return (Unchanged, existingUser)
          else User.fromName name >>= \case
            Nothing -> $craftError notfound
            Just u  -> return (Updated, u)

instance Craftable Package Package where
  watchCraft pkg = do
    ce <- ask
    let pm       = ce ^. craftPackageManager
    let name     = pkg ^. pkgName
    let version  = pkg ^. pkgVersion
    let get      = (pm ^. pmGetter) name
    let install  = (pm ^. pmInstaller) pkg
    let upgrade  = (pm ^. pmUpgrader) pkg
    let pkgError = "craft Package `" ++ name ++ "` failed! "
    let notFound = pkgError ++ "Not Found."
    let wrongVersion got = pkgError ++ "Wrong Version: " ++ show got ++ " Excepted: " ++ show version
    get >>= \case                                                                -- Is the package installed?
      Nothing           -> do                                                    -- It's not installed.
        install                                                                  -- Install it.
        get >>= \case                                                            -- Verify the installation.
          Nothing           -> $craftError notFound                              -- Not Found. The install failed!
          Just installedPkg ->
            let ok = return (Created, installedPkg)
            in case version of                                                   -- Make sure it's the right version
                 AnyVersion -> ok
                 Latest     -> ok
                 Version  _ -> if version == installedPkg ^. pkgVersion
                                then ok
                                else $craftError $ wrongVersion (installedPkg ^. pkgVersion)
      Just installedPkg -> do                                                    -- Package was already installed.
        let installedVersion = installedPkg ^. pkgVersion
        case version of
          AnyVersion -> return (Unchanged, installedPkg)
          Latest     -> do                                                       -- Ensure it's the latest version.
            upgrade
            get >>= \case
              Nothing          -> $craftError notFound                           -- Not found. Where did it go?
              Just upgradedPkg ->
                return $
                  if upgradedPkg^.pkgVersion /= installedPkg^.pkgVersion         -- If the package version increased,
                  then (Updated, upgradedPkg)                                    -- Then the package was upgraded
                  else (Unchanged, upgradedPkg)                                  -- Else it was already the latest.
          Version _  ->                                                          -- Expecting a specific version
            if version == installedVersion                                       -- Is the correct version installed?
            then return (Unchanged, installedPkg)
            else do
              upgrade                                                           -- Try upgrading to the correct version.
              get >>= \case
                Nothing          -> $craftError notFound                        -- Where did it go?
                Just upgradedPkg ->
                  if version == upgradedPkg ^. pkgVersion                        -- Is the correct version installed?
                  then return (Updated, upgradedPkg)
                  else $craftError $ wrongVersion (upgradedPkg ^. pkgVersion)


instance Destroyable Package where
  watchDestroy pkg = do
    ce <- ask
    let pm   = ce ^. craftPackageManager
    let name = pkg ^. pkgName
    let get  = (pm ^. pmGetter) name
    get >>= \case
      Nothing -> return (Unchanged, Nothing)
      Just installedPkg -> do
        (pm ^. pmUninstaller) pkg
        get >>= \case
          Nothing            -> return (Removed, Just installedPkg)
          Just unexpectedPkg -> $craftError $ "destroy Package `" ++ name ++ "` failed! " ++ "Found: " ++ show unexpectedPkg


instance Craftable User User where
  watchCraft user = do
    let name = show $ user ^. userName
    let notFound = $craftError $ "User `" ++ name ++ "` not found!"
    userFromStr name >>= \case
      Nothing     -> do
        useradd user
        userFromStr name >>= \case
          Nothing -> notFound
          Just actualUser -> do
            verify user actualUser
            return (Created, actualUser)
      Just existingUser -> do
        res <- mapM (\(test', act) -> if test' user existingUser
                                      then return True
                                      else act >> return False)
                 [ (test userName,            $notImplemented "set username")
                 , (test uid,                 $notImplemented "set uid")
                 , (test (userGroup . groupName), $notImplemented "set group")
                 , (test userGroups,              $notImplemented "set groups")
                 , (test userHome,                $notImplemented "set home")
                 , (test userPasswordHash,        $notImplemented "set passwordHash")
                 , (test userShell,               $notImplemented "set shell")
                 ]
        if and res
        then return (Unchanged, existingUser)
        else
          userFromStr (show name) >>= \case
            Nothing -> notFound
            Just user'' -> do
              verify user'' user
              return (Updated, user'')

   where
    test :: Eq a => Lens' b a -> b -> b -> Bool
    test l a b = a ^. l == b ^. l
    verify :: User -> User -> Craft ()
    verify _expectedUser _actualUser = $notImplemented "verify User"


instance Craftable Group Group where
  watchCraft grp = do
    _ <- $notImplemented "craft Group"
    -- groupFromName . groupname
    exec_ "groupadd" $ toArg "--gid" (grp ^. Craft.Types.gid) ++ [show $ grp ^. groupName]
    exec_ "gpasswd" ["--members", intercalate "," (map show (grp ^. groupMembers))
                             , show $ grp ^. groupName]
    return (Unchanged, grp)


instance Craftable File File where
  watchCraft f = do
    let fp = f ^. path
    -- FIXME: Don't use _Just
    let expectedMD5 = show . md5 . BL.fromStrict $ f ^. fileContent . _Just
    let err :: String -> Craft a
        err str = $craftError $ "craft File `"++show fp++"` failed! "++str
    let verifyMode m =
          when (m /= f ^. mode) $
            err $ "Wrong Mode: " ++ show m ++ " Expected: " ++ show (f ^. mode)
    let verifyOwner o =
          when (o /= f ^. ownerID) $
            err $ "Wrong OwnerID: " ++ show o ++ " Expected: " ++ show (f ^. ownerID)
    let verifyGroup g =
          when (g /= f ^. groupID) $
            err $ "Wrong GroupID: " ++ show g ++ " Expected: " ++ show (f ^. groupID)
    let verifyStats (m, o, g) = verifyMode m >> verifyOwner o >> verifyGroup g
    getStats fp >>= \case
      Nothing          -> do
        case f ^. fileContent of
          Nothing -> exec_ "touch" [fromAbsFile fp]
          Just c  -> File.write fp c
        File.setStats f
        when (isJust $ f  ^. fileContent) $ do
          actualMD5 <- File.md5sum fp
          unless (actualMD5 == expectedMD5) $
            err "Content Mismatch."
        return (Created, f)
      Just (m', o', g') -> do
        let checks = [ (f ^. mode    == m', setMode    (f ^. mode)    fp)
                     , (f ^. ownerID == o', setOwnerID (f ^. ownerID) fp)
                     , (f ^. groupID == g', setGroupID (f ^. groupID) fp)
                     ]
        mapM_ (uncurry unless) checks
        case f ^. fileContent of
          Nothing -> if all fst checks
                     then return (Unchanged, f)
                     else do
                       getStats fp >>= \case
                         Nothing    -> err "Not Found."
                         Just stats -> verifyStats stats
                       return (Updated, f)
          Just c -> do
            actualMD5 <- File.md5sum fp
            if actualMD5 == expectedMD5
            then return $ if all fst checks
                          then (Unchanged, f)
                          else (Updated, f)
            else do
              File.write fp c
              md5AfterWrite <- File.md5sum fp
              if md5AfterWrite == expectedMD5
                then return (Updated, f)
                else err "Content Mismatch."

  craft f = do
    let fp = f^.path
    exec_ "touch" ["-a", fromAbsFile fp]
    case f ^. fileContent of
      Nothing -> return ()
      Just c  -> do
        let expectedMD5 = show . md5 $ BL.fromStrict c
        actualMD5 <- File.md5sum fp
        unless (actualMD5 == expectedMD5) $ do
          File.write fp c
          md5AfterWrite <- File.md5sum fp
          unless (expectedMD5 == md5AfterWrite) $
            $craftError $ "craft File `"++show fp++"` failed! Content Mismatch."
    File.setStats f
    return f


instance Craftable (Path Abs Path.File) File where
  watchCraft fp = watchCraft $ file fp
  watchCraft_ fp = watchCraft_ $ file fp
  craft fp = craft $ file fp
  craft_ fp = craft_ $ file fp


instance Destroyable (Path Abs Path.File) where
  watchDestroy fp =
    File.exists fp >>= \case
      False -> return (Unchanged, Nothing)
      True  -> do
        destroy_ fp
        return (Removed, Just fp)

  destroy_ fp = do
    exec_ "rm" ["-f", fromAbsFile fp]
    File.exists fp >>= flip when (
      $craftError $ "destroy File `"++show fp++"` failed! Found.")


instance Destroyable File where
  watchDestroy f =
    File.get (f^.path) >>= \case
      Nothing -> return (Unchanged, Nothing)
      Just f' -> do
        destroy_ f
        return (Removed, Just f')

  destroy_ f = destroy_ $ f^.path


instance Craftable Directory Directory where
  watchCraft d = do
    let dp = d ^. path
        setMode'  = setMode (d ^. mode) dp
        setOwner' = setOwnerID (d ^. ownerID) dp
        setGroup' = setGroupID (d ^. groupID) dp
        error' :: String -> Craft a
        error' str = $craftError
           $ formatToString ("craft Directory `"%string%"` failed! "%string)
                            (show dp) str
        verifyMode m =
          when (m /= d ^. mode) $
            error' $ formatToString ("Wrong Mode: "%shown%" Expected: "%shown)
                                    m (d ^. mode)
        verifyOwner o =
          when (o /= d ^. ownerID) $
            error' $ formatToString ("Wrong Owner ID: "%shown%" Expected: "%shown)
                                    o (d ^. ownerID)
        verifyGroup g =
          when (g /= d ^. groupID) $
            error' $ formatToString ("Wrong Group ID: "%shown%" Expected: "%shown)
                                    g (d ^. groupID)
        verifyStats (m, o, g) =
          verifyMode m >> verifyOwner o >> verifyGroup g
    getStats dp >>= \case
      Nothing -> do
        exec_ "mkdir" ["-p", fromAbsDir dp]
        setMode' >> setOwner' >> setGroup'
        getStats dp >>= \case
          Nothing -> error' "Not Found."
          Just stats' -> verifyStats stats' >> return (Created, d)
      Just (m', o', g') -> do
        let checks = [ (d^.mode    == m', setMode')
                     , (d^.ownerID == o', setOwner')
                     , (d^.groupID == g', setGroup')
                     ]
        mapM_ (uncurry unless) checks
        if all fst checks then
          return (Unchanged, d)
        else
          getStats dp >>= \case
            Nothing -> error' "Not Found."
            Just stats' -> verifyStats stats' >> return (Updated, d)

instance Craftable AbsDirPath Directory where
  watchCraft dp = watchCraft $ directory dp
  watchCraft_ dp = watchCraft_ $ directory dp
  craft dp = craft $ directory dp
  craft_ dp = craft_ $ directory dp
