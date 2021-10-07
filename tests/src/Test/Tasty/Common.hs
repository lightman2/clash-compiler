module Test.Tasty.Common where

import           Clash.Driver.Manifest     (readManifest, Manifest(..))
import           Control.Monad.Extra       (concatMapM, forM_)
import           Data.Default              (Default, def)
import           Data.Maybe                (fromMaybe)
import           System.Directory          (copyFile)
import           System.FilePath.Glob      (glob)
import           System.FilePath           ((</>), takeFileName)

data TestExitCode
  = TestExitCode
  | TestSpecificExitCode Int
  | NoTestExitCode

instance Default TestExitCode where
  def = TestExitCode


getManifests :: String -> IO [(FilePath, Manifest)]
getManifests pattern = mapM go =<< glob pattern
 where
  go :: FilePath -> IO (FilePath, Manifest)
  go path = do
    let err = error ("Failed to read/decode: " <> show path)
    manifest <- fromMaybe err <$> readManifest path
    pure (path, manifest)

testExitCode :: TestExitCode -> Bool
testExitCode TestExitCode = True
testExitCode (TestSpecificExitCode _) = True
testExitCode NoTestExitCode = False

specificExitCode :: TestExitCode -> Maybe Int
specificExitCode (TestSpecificExitCode n) = Just n
specificExitCode _ = Nothing

-- | Clash currently creates a directory for every top entity it synthesizes. For
-- example, two top entities 'foo' and 'bar' in module FooBar would be synthesized
-- in the folders:

--   $hdldir/FooBar.foo
--   $hdldir/FooBar.bar

-- Any data files used in 'foo' would be copied to its directory, while any data
-- files used by 'bar' would be copied to  _its_ directory. For example, if 'foo'
-- and 'bar' would both instantiate a 'blockRamFile' with 'memory_foo.list' and
-- 'memory_bar.list' respectively, the following files would exist after synthesizing:

--   $hdldir/FooBar.foo/foo.vhdl
--   $hdldir/FooBar.foo/memory_foo.list
--   $hdldir/FooBar.bar/bar.vhdl
--   $hdldir/FooBar.bar/memory_bar.list

-- The HDL files (`foo.vhdl` and `bar.vhdl`) would refer to those files relative
-- to it, for example just "memory_foo.list". Some tools look for these
-- files relative to the HDL files (most synthesis tools), while others will try to
-- find them in whatever directory they're executed in (most simulators).

-- The "hack" in this case refers to copying the files from the HDL directory to
-- the directory the simulator is run from. It currently copies all files with a
-- "list" or "bin" extension.

-- Note: the sensible thing for Clash would be to use relative paths in the HDL
--       files as such: `../FooBar.foo/memory_foo.list`. This would satisfy both
--       tooling looking relative to HDL files, and tooling run from a sibling
--       directory.
--
copyDataFilesHack :: FilePath -> FilePath -> IO ()
copyDataFilesHack src workDir = do
  filePaths <- concatMapM (glob . pat) exts
  forM_ filePaths $ \filePath ->
    copyFile filePath (workDir </> takeFileName filePath)
 where
  pat ext = src </> ("*/*." <> ext)
  exts = ["list", "bin"]
